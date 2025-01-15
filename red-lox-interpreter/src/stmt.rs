use red_lox_ast::scanner::{Location, Token, TokenWithLocation};
use red_lox_ast::{expr::Expr, stmt::Stmt, visitor::Visitor};

use crate::expr;
use crate::{expr::Value, Interpreter};

pub enum Action {
    Print(Value),
    Eval(Value),
    Define(Token, Value)
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to evaluate the expression of the statement: {}", .0)]
    ExprEvalError(expr::Error),
}

impl Visitor<Result<Action, Error>> for Interpreter {
    fn visit_expr(&self, _: &Expr) -> Result<Action, Error> {
        unimplemented!()
    }

    fn visit_stmt(&self, stmt: &Stmt) -> Result<Action, Error> {
        match stmt {
            Stmt::Print(e) => match Visitor::<Result<Value, expr::Error>>::visit_expr(self, e) {
                Ok(v) => Ok(Action::Print(v)),
                Err(e) => Err(Error::ExprEvalError(e)),
            },
            Stmt::Expression(e) => {
                match Visitor::<Result<Value, expr::Error>>::visit_expr(self, e) {
                    Ok(v) => Ok(Action::Eval(v)),
                    Err(e) => Err(Error::ExprEvalError(e)),
                }
            }
            Stmt::Var(t, expr) => match expr.as_ref() {
                Some(e) => match Visitor::<Result<Value, expr::Error>>::visit_expr(self, e) {
                    Ok(v) => Ok(Action::Define(t.clone(), v)),
                    Err(e) => Err(Error::ExprEvalError(e)),
                },
                None => Ok(Action::Define(t.clone(), Value::Nil)),
            },
        }
    }
}
