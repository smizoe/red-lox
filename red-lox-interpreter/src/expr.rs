use std::fmt::Display;

use red_lox_ast::{
    expr::Expr,
    scanner::{Token, TokenWithLocation},
    stmt::Stmt,
    visitor::Visitor,
};

use crate::Interpreter;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    String(String),
    Number(f64),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Nil => write!(f, "nil"),
            String(s) => write!(f, "{}", s),
            Number(v) => write!(f, "{}", v),
            Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{} Operand of {:?} is expected be of type {expected_type} but {actual_result:?} is passed", .operator.location, .operator.token)]
    InvalidUnaryOpOperandError {
        actual_result: Value,
        expected_type: String,
        operator: TokenWithLocation,
    },
    #[error("{} Operands of operator {:?} must {verb}, but the following were passed:\n    lhs: {lhs:?}\n    rhs: {rhs:?}", .operator.location, .operator.token)]
    InvalidBinaryOpOperandError {
        verb: String,
        lhs: Value,
        rhs: Value,
        operator: TokenWithLocation,
    },
    #[error("{} Undefined variable {:?} found", .0.location, .0.token)]
    UndefinedVariableError(TokenWithLocation),
}

fn handle_binary_op(
    left_expr: Value,
    right_expr: Value,
    operator: &TokenWithLocation,
) -> Result<Value, Error> {
    match (left_expr, right_expr, &operator.token) {
        (Value::Number(l), Value::Number(r), Token::Plus) => {
            Ok(Value::Number(l + r))
        }
        (Value::String(mut l), Value::String(r), Token::Plus) => {
            l.push_str(&r);
            Ok(Value::String(l))
        }
        (lhs, rhs, Token::Plus) => Err(Error::InvalidBinaryOpOperandError {
            verb: "be two numbers or two strings".to_string(),
            lhs,
            rhs,
            operator: operator.clone(),
        }),
        (Value::Number(l), Value::Number(r), Token::Minus) => {
            Ok(Value::Number(l - r))
        }
        (Value::Number(l), Value::Number(r), Token::Slash) => {
            Ok(Value::Number(l / r))
        }
        (Value::Number(l), Value::Number(r), Token::Star) => {
            Ok(Value::Number(l * r))
        }
        (Value::Number(l), Value::Number(r), Token::Greater) => {
            Ok(Value::Bool(l > r))
        }
        (Value::Number(l), Value::Number(r), Token::GreaterEqual) => {
            Ok(Value::Bool(l >= r))
        }
        (Value::Number(l), Value::Number(r), Token::Less) => Ok(Value::Bool(l < r)),
        (Value::Number(l), Value::Number(r), Token::LessEqual) => {
            Ok(Value::Bool(l <= r))
        }
        (
            l,
            r,
            Token::Minus
            | Token::Slash
            | Token::Star
            | Token::Greater
            | Token::GreaterEqual
            | Token::Less
            | Token::LessEqual,
        ) => Err(Error::InvalidBinaryOpOperandError {
            verb: "be two numbers".to_string(),
            lhs: l,
            rhs: r,
            operator: operator.clone(),
        }),
        (l, r, Token::BangEqual) => Ok(Value::Bool(l != r)),
        (l, r, Token::EqualEqual) => Ok(Value::Bool(l == r)),
        _ => unimplemented!(),
    }
}

impl Visitor<Result<Value, Error>> for Interpreter {
    fn visit_expr(&self, expr: &Expr) -> Result<Value, Error> {
        use Expr::*;
        match expr {
            LiteralBool(b, _) => Ok(Value::Bool(*b)),
            LiteralNil(_) => Ok(Value::Nil),
            LiteralNumber(v, _) => Ok(Value::Number(*v)),
            LiteralString(s, _) => Ok(Value::String(s.clone())),
            Grouping(e, _) => self.visit_expr(e),
            Binary {
                left,
                operator,
                right,
            } => {
                let le: Value =
                    Visitor::<Result<Value, Error>>::visit_expr(self, left)?;
                let re: Value =
                    Visitor::<Result<Value, Error>>::visit_expr(self, right)?;
                handle_binary_op(le, re, operator)
            }
            Unary { operator, right } => {
                let r: Value =
                    Visitor::<Result<Value, Error>>::visit_expr(self, right)?;
                match (&operator.token, r) {
                    (Token::Minus, Value::Number(v)) => Ok(Value::Number(-v)),
                    (Token::Minus, r) => Err(Error::InvalidUnaryOpOperandError {
                        actual_result: r,
                        expected_type: "Number".to_string(),
                        operator: operator.clone(),
                    }),
                    (Token::Bang, r) => match r {
                        Value::Nil => Ok(Value::Bool(false)),
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        Value::Number(_) | Value::String(_) => Ok(Value::Bool(true)),
                    },
                    _ => panic!(
                        "Token {:?} is found when visiting Unary expr.",
                        operator.token
                    ),
                }
            }
            Variable(t) => {
                self.environment.get(t).cloned()
            }
        }
    }

    fn visit_stmt(&self, _: &Stmt) -> Result<Value, Error> {
        unimplemented!()
    }
}
