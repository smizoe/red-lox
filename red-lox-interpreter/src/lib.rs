use red_lox_ast::{
    expr::Expr,
    scanner::{Token, TokenWithLocation},
    visitor::Visitor,
};

#[derive(Debug, Clone, PartialEq)]
enum EvalResult {
    Nil,
    String(String),
    Number(f64),
    Bool(bool),
}

#[derive(Debug, thiserror::Error)]
enum EvalError {
    #[error("{}Operand of {:?} is expected be of type {expected_type} but {actual_result:?} is passed", .operator.location, .operator.token)]
    InvalidUnaryOpOperandError {
        actual_result: EvalResult,
        expected_type: String,
        operator: TokenWithLocation,
    },
    #[error("{}Operands of operator {:?} must {verb}, but the following were passed:\n    lhs: {lhs:?}\n    rhs: {rhs:?}", .operator.location, .operator.token)]
    InvalidBinaryOpOperandError {
        verb: String,
        lhs: EvalResult,
        rhs: EvalResult,
        operator: TokenWithLocation,
    },
}

struct Interpreter {}

fn handle_binary_op(
    left_expr: EvalResult,
    right_expr: EvalResult,
    operator: &TokenWithLocation,
) -> Result<EvalResult, EvalError> {
    match (left_expr, right_expr, &operator.token) {
        (EvalResult::Number(l), EvalResult::Number(r), Token::Plus) => {
            Ok(EvalResult::Number(l + r))
        }
        (EvalResult::String(mut l), EvalResult::String(r), Token::Plus) => {
            l.push_str(&r);
            Ok(EvalResult::String(l))
        }
        (lhs, rhs, Token::Plus) => Err(EvalError::InvalidBinaryOpOperandError {
            verb: "be two numbers or two strings".to_string(),
            lhs,
            rhs,
            operator: operator.clone(),
        }),
        (EvalResult::Number(l), EvalResult::Number(r), Token::Minus) => {
            Ok(EvalResult::Number(l - r))
        }
        (EvalResult::Number(l), EvalResult::Number(r), Token::Slash) => {
            Ok(EvalResult::Number(l / r))
        }
        (EvalResult::Number(l), EvalResult::Number(r), Token::Star) => {
            Ok(EvalResult::Number(l * r))
        }
        (EvalResult::Number(l), EvalResult::Number(r), Token::Greater) => {
            Ok(EvalResult::Bool(l > r))
        }
        (EvalResult::Number(l), EvalResult::Number(r), Token::GreaterEqual) => {
            Ok(EvalResult::Bool(l >= r))
        }
        (EvalResult::Number(l), EvalResult::Number(r), Token::Less) => Ok(EvalResult::Bool(l < r)),
        (EvalResult::Number(l), EvalResult::Number(r), Token::LessEqual) => {
            Ok(EvalResult::Bool(l <= r))
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
        ) => Err(EvalError::InvalidBinaryOpOperandError {
            verb: "be two numbers".to_string(),
            lhs: l,
            rhs: r,
            operator: operator.clone(),
        }),
        (l, r, Token::BangEqual) => Ok(EvalResult::Bool(l != r)),
        (l, r, Token::EqualEqual) => Ok(EvalResult::Bool(l == r)),
        _ => unimplemented!(),
    }
}

impl Visitor<Result<EvalResult, EvalError>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<EvalResult, EvalError> {
        use Expr::*;
        match expr {
            LiteralBool(b, _) => Ok(EvalResult::Bool(*b)),
            LiteralNil(_) => Ok(EvalResult::Nil),
            LiteralNumber(v, _) => Ok(EvalResult::Number(*v)),
            LiteralString(s, _) => Ok(EvalResult::String(s.clone())),
            Grouping(e, _) => self.visit_expr(e),
            Binary {
                left,
                operator,
                right,
            } => self
                .visit_expr(left)
                .and_then(|le| self.visit_expr(right).map(|re| (le, re)))
                .and_then(|(le, re)| handle_binary_op(le, re, operator)),
            Unary { operator, right } => {
                self.visit_expr(right)
                    .and_then(|r| match (&operator.token, r) {
                        (Token::Minus, EvalResult::Number(v)) => Ok(EvalResult::Number(-v)),
                        (Token::Minus, r) => Err(EvalError::InvalidUnaryOpOperandError {
                            actual_result: r,
                            expected_type: "Number".to_string(),
                            operator: operator.clone(),
                        }),
                        (Token::Bang, r) => match r {
                            EvalResult::Nil => Ok(EvalResult::Bool(false)),
                            EvalResult::Bool(b) => Ok(EvalResult::Bool(!b)),
                            EvalResult::Number(_) | EvalResult::String(_) => {
                                Ok(EvalResult::Bool(true))
                            }
                        },
                        _ => panic!(
                            "Token {:?} is found when visiting Unary expr.",
                            operator.token
                        ),
                    })
            }
        }
    }
}

#[cfg(test)]
mod tests {}
