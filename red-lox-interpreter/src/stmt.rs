use red_lox_ast::expr::Evaluator as _;
use red_lox_ast::{
    scanner::Token,
    stmt::{Evaluator, Stmt},
};

use crate::expr;
use crate::{expr::Value, Interpreter};

pub enum Action {
    Print(Value),
    Eval(Value),
    Define(Token, Value),
    Break,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to evaluate the expression of the statement: {}", .0)]
    ExprEvalError(expr::Error),
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
                        _ => (),
                    }
                }
                Ok(Action::Eval(Value::Nil))
            }
            Stmt::Expression(e) => match self.evaluate_expr(e) {
                Ok(v) => Ok(Action::Eval(v)),
                Err(e) => Err(Error::ExprEvalError(e)),
            },
            Stmt::Var(t, expr) => match expr.as_ref() {
                Some(e) => match self.evaluate_expr(e) {
                    Ok(v) => Ok(Action::Define(t.clone(), v)),
                    Err(e) => Err(Error::ExprEvalError(e)),
                },
                None => Ok(Action::Define(t.clone(), Value::Nil)),
            },
            Stmt::Block(stmts) => {
                let guard = self.enter();
                let mut action = Action::Eval(Value::Nil);
                for stmt in stmts {
                    if guard.interpreter.execute(&stmt)? {
                        action = Action::Break;
                        break;
                    }
                }
                Ok(action)
            }
            Stmt::Break => Ok(Action::Break),
        }
    }
}
