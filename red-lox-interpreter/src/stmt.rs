use std::rc::Rc;

use red_lox_ast::expr::Evaluator as _;
use red_lox_ast::{
    scanner::Token,
    stmt::{Evaluator, Stmt},
};

use crate::expr;
use crate::{expr::Value, Interpreter};

#[derive(Clone)]
pub enum Action {
    Print(Value),
    Eval(Value),
    Define(Token, Value),
    Return(Value),
    Break,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to evaluate the expression of the statement: {}", .0)]
    ExprEvalError(expr::Error),
    #[error("A return statement encountered outside the function/method context.")]
    InvalidReturnStmtError(),
}

impl<'a, 'b> Evaluator<Result<Action, Error>> for Interpreter<'a, 'b> {
    fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<Action, Error> {
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
            Stmt::Function { name, params, body } => Ok(Action::Define(
                name.token.clone(),
                Value::Function {
                    name: match &name.token {
                        Token::Identifier(n) => n.clone(),
                        _ => unreachable!(),
                    },
                    body: Rc::new(body.clone()),
                    params: params.clone(),
                    closure: self.environment.clone(),
                },
            )),
            Stmt::Var(t, expr) => match expr.as_ref() {
                Some(e) => match self.evaluate_expr(e) {
                    Ok(v) => Ok(Action::Define(t.clone(), v)),
                    Err(e) => Err(Error::ExprEvalError(e)),
                },
                None => Ok(Action::Define(t.clone(), Value::Nil)),
            },
            Stmt::Block(stmts) => self.execute_block(stmts),
            Stmt::Return(t, v) => Ok(Action::Return(
                self.evaluate_expr(v).map_err(Error::ExprEvalError)?,
            )),
            Stmt::Break => Ok(Action::Break),
        }
    }
}
