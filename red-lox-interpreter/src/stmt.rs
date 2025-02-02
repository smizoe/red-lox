use std::rc::Rc;

use red_lox_ast::{scanner::Token, stmt::Stmt};

use crate::expr;
use crate::{expr::Value, Interpreter};

#[derive(Clone)]
pub enum Action {
    Print(Value),
    Eval(Value),
    Return(Value),
    Break,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to evaluate the expression of the statement: {}", .0)]
    ExprEvalError(expr::Error),
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
                        name: match &name.token {
                            Token::Identifier(n) => n.clone(),
                            _ => unreachable!(),
                        },
                        body: Rc::new(body.clone()),
                        params: params.clone(),
                        closure: self.environment.clone(),
                    },
                );
                Ok(Action::Eval(Value::Nil))
            }
            Stmt::Class { name, methods } => {
                self.environment
                    .define(name.token.id_name().to_string(), Value::Nil);
                self.environment
                    .assign(
                        name,
                        Value::Class {
                            name: name.token.id_name().to_string(),
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
            Stmt::Return(_t, v) => Ok(Action::Return(
                self.evaluate_expr(v).map_err(Error::ExprEvalError)?,
            )),
            Stmt::Break => Ok(Action::Break),
        }
    }
}
