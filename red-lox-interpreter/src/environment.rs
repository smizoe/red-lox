use std::collections::HashMap;

use red_lox_ast::scanner::{Token, TokenWithLocation};

use crate::{expr, expr::Value};

#[derive(Debug, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, name: String, val: Value) -> Option<Value> {
        self.values.insert(name, val)
    }

    pub fn get<'a>(&'a self, token: &TokenWithLocation) -> Result<&'a Value, expr::Error> {
        let name = match &token.token {
            Token::Identifier(n) => n,
            _ => unreachable!(),
        };
        self.values
            .get(name)
            .ok_or(expr::Error::UndefinedVariableError(token.clone()))
    }
}
