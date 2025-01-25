use std::{cell::RefCell, fmt::Display, rc::Rc};

use red_lox_ast::{
    expr::{Evaluator, Expr},
    scanner::{Location, Token, TokenWithLocation},
    stmt::Stmt,
};

use crate::{
    environment::Environment,
    stmt::{self, Action},
    Interpreter,
};

// Value has to implement Clone to return the value of Value when it is assigned to a variable.
#[derive(Clone)]
pub enum Value {
    Nil,
    String(String),
    Number(f64),
    Bool(bool),
    NativeFn {
        name: String,
        fun: Rc<RefCell<dyn FnMut(Vec<Value>) -> Result<Value, Error>>>,
        arity: usize,
    },
    Function {
        name: String,
        // body is Rc<..> to make this clonable.
        body: Rc<Vec<Box<Stmt>>>,
        params: Vec<TokenWithLocation>,
        closure: Rc<Environment>,
    },
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::NativeFn { name: l_name, .. }, Self::NativeFn { name: r_name, .. }) => {
                l_name == r_name
            }
            (Self::Function { name: l_name, .. }, Self::Function { name: r_name, .. }) => {
                l_name == r_name
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::NativeFn { name, .. } => f
                .debug_struct("NativeFn")
                .field("name", name)
                .field("fun", &format_args!("_native_fn_"))
                .finish(),
            Self::Function { name, params, .. } => f
                .debug_struct("Function")
                .field("name", name)
                .field("body", &format_args!("_function_body_"))
                .field("params", params)
                .finish(),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            Value::Number(_) | Value::String(_) => true,
            Value::NativeFn { .. } | Value::Function { .. } => true,
        }
    }

    pub fn to_string(&self) -> String {
        use Value::*;
        match self {
            Nil => std::string::String::new(),
            String(s) => s.clone(),
            Number(v) => v.to_string(),
            Bool(b) => b.to_string(),
            NativeFn { name, .. } => format!("<native fn {}>", name),
            Function { name, .. } => format!("<fn {}>", name),
        }
    }

    pub fn to_type_str(&self) -> &'static str {
        use Value::*;
        match self {
            Nil => "Nil",
            String(_) => "String",
            Number(_) => "Number",
            Bool(_) => "Bool",
            NativeFn { .. } => "NativeFn",
            Function { .. } => "Function",
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
            _ => write!(f, "{}", self.to_string()),
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
    #[error("{location} Expected {arity} arguments for {name} but got {num_arguments}.")]
    ArityMismatchError {
        name: String,
        arity: usize,
        num_arguments: usize,
        location: Location,
    },
    #[error("{} Can only call functions and classes.", .0)]
    InvalidCalleeError(Location),
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
            Logical {
                left,
                operator,
                right,
            } => match operator.token {
                Token::Or => {
                    if self.evaluate_expr(left)?.is_truthy() {
                        Ok(Value::Bool(true))
                    } else {
                        Ok(Value::Bool(self.evaluate_expr(&right)?.is_truthy()))
                    }
                }
                Token::And => {
                    if !self.evaluate_expr(left)?.is_truthy() {
                        Ok(Value::Bool(false))
                    } else {
                        Ok(Value::Bool(self.evaluate_expr(&right)?.is_truthy()))
                    }
                }
                _ => unreachable!(),
            },
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
            Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate_expr(callee)?;
                let mut args = Vec::with_capacity(arguments.len());
                for arg in arguments {
                    args.push(self.evaluate_expr(&arg)?);
                }
                match callee {
                    Value::NativeFn { name, fun, arity } => {
                        if arity != args.len() {
                            return Err(Error::ArityMismatchError {
                                name,
                                arity,
                                num_arguments: arguments.len(),
                                location: paren.location.clone(),
                            });
                        }
                        fun.borrow_mut()(args)
                    }
                    Value::Function {
                        name,
                        body,
                        params,
                        closure,
                    } => {
                        if arguments.len() != params.len() {
                            return Err(Error::ArityMismatchError {
                                name,
                                arity: params.len(),
                                num_arguments: arguments.len(),
                                location: paren.location.clone(),
                            });
                        }
                        let mut expr_args = Vec::with_capacity(arguments.len());
                        for arg in arguments {
                            expr_args.push(self.evaluate_expr(arg)?);
                        }
                        let guard = self.start_calling_fn(Rc::new(Environment::new(closure)));
                        for (arg, param) in expr_args.into_iter().zip(params) {
                            guard.interpreter.environment.define(
                                match param.token {
                                    Token::Identifier(id) => id,
                                    _ => unreachable!(),
                                },
                                arg,
                            );
                        }
                        guard
                            .interpreter
                            .execute_block(&body)
                            .map_err(|e| match e {
                                stmt::Error::ExprEvalError(e) => e,
                                _ => unreachable!(),
                            })
                            .map(|action| match action {
                                Action::Return(v) => v,
                                _ => Value::Nil,
                            })
                    }
                    _ => Err(Error::InvalidCalleeError(paren.location.clone())),
                }
            }
        }
    }
}
