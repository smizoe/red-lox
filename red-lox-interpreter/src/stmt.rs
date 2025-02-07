use std::collections::HashMap;
use std::rc::Rc;

use red_lox_ast::{scanner::Token, stmt::Stmt};

use crate::expr::{self, Callable};
use crate::{expr::Value, Interpreter};

#[derive(Clone)]
pub enum Action {
    Print(Value),
    Eval(Value),
    Return(Option<Value>),
    Break,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to evaluate the expression of the statement: {}", .0)]
    ExprEvalError(expr::Error),
    #[error("Superclass must be a class, but found {}", .0.to_string())]
    InvalidSuperclassTypeError(Value),
}

impl<'a, 'b> Interpreter<'a, 'b> {
    pub fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<Action, Error> {
        match stmt {
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self
                    .evaluate_expr(&condition)
                    .map_err(Error::ExprEvalError)?
                    .is_truthy()
                {
                    self.evaluate_stmt(then_branch)
                } else if let Some(else_stmt) = else_branch {
                    self.evaluate_stmt(else_stmt)
                } else {
                    Ok(Action::Eval(Value::Nil))
                }
            }
            Stmt::Print(e) => match self.evaluate_expr(e) {
                Ok(v) => Ok(Action::Print(v)),
                Err(e) => Err(Error::ExprEvalError(e)),
            },
            Stmt::While { condition, body } => {
                while self
                    .evaluate_expr(condition)
                    .map_err(Error::ExprEvalError)?
                    .is_truthy()
                {
                    match self.evaluate_stmt(body)? {
                        Action::Break => break,
                        act @ Action::Return(_) => return Ok(act),
                        _ => (),
                    }
                }
                Ok(Action::Eval(Value::Nil))
            }
            Stmt::Expression(e) => match self.evaluate_expr(e) {
                Ok(v) => Ok(Action::Eval(v)),
                Err(e) => Err(Error::ExprEvalError(e)),
            },
            Stmt::Function { name, params, body } => {
                self.environment.define(
                    name.token.id_name().to_string(),
                    Value::Function {
                        name: name.token.id_name().to_string(),
                        callable: Callable::new(
                            body.clone(),
                            params.clone(),
                            self.environment.clone(),
                            false,
                        ),
                    },
                );
                Ok(Action::Eval(Value::Nil))
            }
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                let mut supercls = None;
                if let Some(expr) = superclass {
                    let val = self.evaluate_expr(expr).map_err(Error::ExprEvalError)?;
                    match val {
                        Value::Class { .. } => {
                            supercls.replace(val);
                        }
                        _ => {
                            return Err(Error::InvalidSuperclassTypeError(val));
                        }
                    }
                }
                self.environment
                    .define(name.token.id_name().to_string(), Value::Nil);
                let mut method_map = HashMap::new();
                for method in methods {
                    match method.as_ref() {
                        Stmt::Function { name, params, body } => {
                            method_map.insert(
                                name.token.id_name().to_string(),
                                Value::Function {
                                    name: match &name.token {
                                        Token::Identifier(n) => n.clone(),
                                        _ => unreachable!(),
                                    },
                                    callable: Callable::new(
                                        body.clone(),
                                        params.clone(),
                                        self.environment.clone(),
                                        name.token.id_name() == "init",
                                    ),
                                },
                            );
                        }

                        _ => unreachable!(),
                    }
                }
                self.environment
                    .assign(
                        name,
                        Value::Class {
                            name: name.token.id_name().to_string(),
                            methods: Rc::new(method_map),
                            superclass: supercls.map(Rc::new),
                        },
                    )
                    .map(|v| Action::Eval(v))
                    .map_err(Error::ExprEvalError)
            }
            Stmt::Var(t, expr) => match expr.as_ref() {
                Some(e) => match self.evaluate_expr(e) {
                    Ok(v) => {
                        self.environment.define(t.token.id_name().to_string(), v);
                        Ok(Action::Eval(Value::Nil))
                    }
                    Err(e) => Err(Error::ExprEvalError(e)),
                },
                None => {
                    self.environment
                        .define(t.token.id_name().to_string(), Value::Nil);
                    Ok(Action::Eval(Value::Nil))
                }
            },
            Stmt::Block(stmts) => {
                let mut guard = self.enter();
                guard.execute_block(stmts)
            }
            Stmt::Return(_t, expr) => match expr {
                None => Ok(Action::Return(None)),
                Some(expr) => self
                    .evaluate_expr(expr)
                    .map(|v| Action::Return(Some(v)))
                    .map_err(Error::ExprEvalError),
            },
            Stmt::Break => Ok(Action::Break),
        }
    }
}
