use std::collections::HashMap;

use red_lox_ast::scanner::{Token, TokenWithLocation};

use crate::{expr, expr::Value};

#[derive(Debug, Default)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enter(self: &mut Box<Environment>) {
        let mut other = Box::new(Self::default());
        std::mem::swap(self, &mut other);
        self.enclosing.replace(other);
    }

    pub fn exit(self: &mut Box<Environment>) {
        let mut enclosing = self.enclosing.take().unwrap();
        std::mem::swap(self, &mut enclosing);
    }

    pub fn define(&mut self, name: String, val: Value) -> Option<Value> {
        self.values.insert(name, val)
    }

    pub fn get(&self, token: &TokenWithLocation) -> Result<Value, expr::Error> {
        let name = match &token.token {
            Token::Identifier(n) => n,
            _ => unreachable!(),
        };
        self.get_internal(name)
            .ok_or(expr::Error::UndefinedVariableError(token.clone()))
    }

    pub fn get_internal(&self, name: &str) -> Option<Value> {
        self.values
            .get(name)
            .cloned()
            .or_else(|| self.enclosing.as_ref().and_then(|e| e.get_internal(name)))
    }

    pub fn assign(&mut self, token: &TokenWithLocation, val: Value) -> Result<Value, expr::Error> {
        let name = match &token.token {
            Token::Identifier(n) => n,
            _ => unreachable!(),
        };
        self.assign_internal(name, val)
            .ok_or(expr::Error::UndefinedVariableError(token.clone()))
    }

    pub fn assign_internal(&mut self, name: &str, val: Value) -> Option<Value> {
        match self.values.get_mut(name) {
            Some(v) => {
                *v = val;
                Some(v.clone())
            }
            None => self
                .enclosing
                .as_mut()
                .and_then(|e| e.assign_internal(name, val)),
        }
    }
}
