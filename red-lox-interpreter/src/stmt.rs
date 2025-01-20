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
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to evaluate the expression of the statement: {}", .0)]
    ExprEvalError(expr::Error),
}

impl<'a, 'b> Evaluator<Result<Action, Error>> for Interpreter<'a, 'b> {
    fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<Action, Error> {
        match stmt {
            Stmt::Print(e) => match self.evaluate_expr(e) {
                Ok(v) => Ok(Action::Print(v)),
                Err(e) => Err(Error::ExprEvalError(e)),
            },
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
                self.environment.enter();
                for stmt in stmts {
                    if let Err(e) = self.execute(&stmt) {
                        self.environment.exit();
                        return Err(e);
                    }
                }
                self.environment.exit();
                Ok(Action::Eval(Value::Nil))
            }
        }
    }
}
