use std::fmt::Display;

use red_lox_ast::{
    expr::{Evaluator, Expr},
    scanner::{Location, Token, TokenWithLocation},
};

use crate::Interpreter;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    String(String),
    Number(f64),
    Bool(bool),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            Value::Number(_) | Value::String(_) => true,
        }
    }

    pub fn to_string(&self) -> String {
        use Value::*;
        match self {
            Nil => std::string::String::new(),
            String(s) => s.clone(),
            Number(v) => v.to_string(),
            Bool(b) => b.to_string(),
        }
    }

    pub fn to_type_str(&self) -> &'static str {
        use Value::*;
        match self {
            Nil => "Nil",
            String(_) => "String",
            Number(_) => "Number",
            Bool(_) => "Bool",
        }
    }
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
    #[error("{} Operand of unary operator {:?} is expected be of type {expected_type} but {} is passed", .operator.location, .operator.token, .actual_result.to_type_str())]
    InvalidUnaryOpOperandError {
        actual_result: Value,
        expected_type: String,
        operator: TokenWithLocation,
    },
    #[error("{} Operands of binary operator {:?} {description}, but the following were passed:\n    lhs: {}\n    rhs: {}", .operator.location, .operator.token, .lhs.to_type_str(), .rhs.to_type_str())]
    InvalidBinaryOpOperandError {
        description: String,
        lhs: Value,
        rhs: Value,
        operator: TokenWithLocation,
    },
    #[error("{} Undefined variable {:?} found.", .0.location, .0.token)]
    UndefinedVariableError(TokenWithLocation),
    #[error("{} Division by zero occurred.", .0)]
    DivisionByZeroError(Location),
}

fn handle_binary_op(
    left_expr: Value,
    right_expr: Value,
    operator: &TokenWithLocation,
) -> Result<Value, Error> {
    match (left_expr, right_expr, &operator.token) {
        (Value::Number(l), Value::Number(r), Token::Plus) => Ok(Value::Number(l + r)),
        (Value::String(mut l), r, Token::Plus) => {
            l.push_str(&r.to_string());
            Ok(Value::String(l))
        }
        (l, Value::String(r), Token::Plus) => {
            let mut l = l.to_string();
            l.push_str(&r.to_string());
            Ok(Value::String(l))
        }
        (lhs, rhs, Token::Plus) => Err(Error::InvalidBinaryOpOperandError {
            description: "must be two numbers or one of them must be a string".to_string(),
            lhs,
            rhs,
            operator: operator.clone(),
        }),
        (Value::Number(l), Value::Number(r), Token::Minus) => Ok(Value::Number(l - r)),
        (Value::Number(l), Value::Number(r), Token::Slash) => {
            if r == 0.0 {
                return Err(Error::DivisionByZeroError(operator.location.clone()));
            }
            Ok(Value::Number(l / r))
        }
        (Value::Number(l), Value::Number(r), Token::Star) => Ok(Value::Number(l * r)),
        (l, r, Token::Minus | Token::Slash | Token::Star) => {
            Err(Error::InvalidBinaryOpOperandError {
                description: "must be two numbers".to_string(),
                lhs: l,
                rhs: r,
                operator: operator.clone(),
            })
        }
        (Value::Number(l), Value::Number(r), Token::Greater) => Ok(Value::Bool(l > r)),
        (Value::Number(l), Value::Number(r), Token::GreaterEqual) => Ok(Value::Bool(l >= r)),
        (Value::Number(l), Value::Number(r), Token::Less) => Ok(Value::Bool(l < r)),
        (Value::Number(l), Value::Number(r), Token::LessEqual) => Ok(Value::Bool(l <= r)),
        (Value::String(l), Value::String(r), Token::Greater) => Ok(Value::Bool(l > r)),
        (Value::String(l), Value::String(r), Token::GreaterEqual) => Ok(Value::Bool(l >= r)),
        (Value::String(l), Value::String(r), Token::Less) => Ok(Value::Bool(l < r)),
        (Value::String(l), Value::String(r), Token::LessEqual) => Ok(Value::Bool(l <= r)),
        (l, r, Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual) => {
            Err(Error::InvalidBinaryOpOperandError {
                description: "must be two numbers or two strings".to_string(),
                lhs: l,
                rhs: r,
                operator: operator.clone(),
            })
        }
        (l, r, Token::BangEqual) => Ok(Value::Bool(l != r)),
        (l, r, Token::EqualEqual) => Ok(Value::Bool(l == r)),
        _ => unimplemented!(),
    }
}

impl<'a, 'b> Evaluator<Result<Value, Error>> for Interpreter<'a, 'b> {
    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        use Expr::*;
        match expr {
            LiteralBool(b, _) => Ok(Value::Bool(*b)),
            LiteralNil(_) => Ok(Value::Nil),
            LiteralNumber(v, _) => Ok(Value::Number(*v)),
            LiteralString(s, _) => Ok(Value::String(s.clone())),
            Grouping(e, _) => self.evaluate_expr(e),
            Binary {
                left,
                operator,
                right,
            } => {
                let le: Value = self.evaluate_expr(left)?;
                let re: Value = self.evaluate_expr(right)?;
                handle_binary_op(le, re, operator)
            }
            Ternary { cond, left, right } => {
                let c = self.evaluate_expr(cond)?;
                if c.is_truthy() {
                    self.evaluate_expr(left)
                } else {
                    self.evaluate_expr(right)
                }
            }
            Unary { operator, right } => {
                let r: Value = self.evaluate_expr(right)?;
                match (&operator.token, r) {
                    (Token::Minus, Value::Number(v)) => Ok(Value::Number(-v)),
                    (Token::Minus, r) => Err(Error::InvalidUnaryOpOperandError {
                        actual_result: r,
                        expected_type: "Number".to_string(),
                        operator: operator.clone(),
                    }),
                    (Token::Bang, r) => Ok(Value::Bool(!r.is_truthy())),
                    _ => panic!(
                        "Token {:?} is found when visiting Unary expr.",
                        operator.token
                    ),
                }
            }
            Variable(t) => self.environment.get(t),
            Assign { name, expr } => {
                let value = self.evaluate_expr(&expr)?;
                self.environment.assign(name, value)
            }
            ExprSeries(exprs) => {
                let mut last_value = self.evaluate_expr(&exprs[0])?;
                for expr in exprs.iter().skip(1) {
                    last_value = self.evaluate_expr(&expr)?;
                }
                Ok(last_value)
            }
        }
    }
}
