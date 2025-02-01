use std::{cell::RefCell, collections::HashMap, rc::Rc};

use red_lox_ast::scanner::{Token, TokenWithLocation};

use crate::{expr, expr::Value};

#[derive(Debug, Default)]
pub struct Environment {
    enclosing: RefCell<Option<Rc<Environment>>>,
    values: RefCell<HashMap<String, Value>>,
}

impl Environment {
    pub fn new(enclosing: Rc<Environment>) -> Self {
        Self {
            enclosing: RefCell::new(Some(enclosing)),
            values: RefCell::new(HashMap::default()),
        }
    }

    pub fn enter(self: &mut Rc<Self>) {
        let mut other = Rc::new(Self::default());
        std::mem::swap(self, &mut other);
        self.enclosing.borrow_mut().replace(other);
    }

    pub fn exit(self: &mut Rc<Self>) {
        let mut enclosing = self.enclosing.borrow().clone().unwrap();
        std::mem::swap(self, &mut enclosing);
    }

    pub fn define(&self, name: String, val: Value) -> Option<Value> {
        self.values.borrow_mut().insert(name, val)
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
        self.values.borrow().get(name).cloned().or_else(|| {
            self.enclosing
                .borrow()
                .as_ref()
                .and_then(|e| e.get_internal(name))
        })
    }

    fn ancestor(self: &Rc<Self>, distance: usize) -> Option<Rc<Self>> {
        let mut env = self.clone();
        for _ in 0..distance {
            let enclosing = env.enclosing.borrow().clone();
            match enclosing {
                Some(e) => env = e,
                None => return None,
            }
        }
        Some(env)
    }

    pub fn get_at(
        self: &Rc<Environment>,
        distance: usize,
        token: &TokenWithLocation,
    ) -> Result<Value, expr::Error> {
        let env = self.ancestor(distance);
        env.and_then(|env| {
            let borrowed = env.values.borrow();
            borrowed.get(token.token.id_name()).cloned()
        })
        .ok_or(expr::Error::UndefinedVariableError(token.clone()))
    }

    pub fn assign(&self, token: &TokenWithLocation, val: Value) -> Result<Value, expr::Error> {
        let name = match &token.token {
            Token::Identifier(n) => n,
            _ => unreachable!(),
        };
        self.assign_internal(name, val)
            .ok_or(expr::Error::UndefinedVariableError(token.clone()))
    }

    fn assign_internal(&self, name: &str, val: Value) -> Option<Value> {
        match self.values.borrow_mut().get_mut(name) {
            Some(v) => {
                *v = val;
                Some(v.clone())
            }
            None => self
                .enclosing
                .borrow()
                .as_ref()
                .and_then(|e| e.assign_internal(name, val)),
        }
    }

    pub fn assign_at(
        self: &Rc<Environment>,
        distance: usize,
        token: &TokenWithLocation,
        value: Value,
    ) -> Result<Value, expr::Error> {
        let env = self.ancestor(distance);
        env.map(|e| {
            e.values
                .borrow_mut()
                .insert(token.token.id_name().to_string(), value.clone());
            value
        })
        .ok_or(expr::Error::UndefinedVariableError(token.clone()))
    }
}
