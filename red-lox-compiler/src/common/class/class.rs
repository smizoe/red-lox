use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::common::{value::Value, InternedString};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Class {
    name: InternedString,
}

impl Class {
    pub(crate) fn new(name: InternedString) -> Self {
        Self { name }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "class {}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Instance {
    class: Rc<Class>,
    fields: Fields,
}

impl Instance {
    pub(crate) fn new(class: Rc<Class>) -> Self {
        Self {
            class,
            fields: Fields::new(),
        }
    }

    pub(crate) fn get_field<S>(&self, name: &S) -> Option<Value>
    where
        S: AsRef<str>,
    {
        self.fields.get(name)
    }

    pub(crate) fn set_field(&mut self, name: InternedString, value: Value) {
        self.fields.set(name, value);
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Fields {
    internal: Rc<RefCell<HashMap<InternedString, Value>>>,
}

impl Fields {
    fn new() -> Self {
        Self {
            internal: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    fn get<S>(&self, name: &S) -> Option<Value>
    where
        S: AsRef<str>,
    {
        self.internal.borrow().get(name.as_ref()).cloned()
    }

    fn set(&mut self, name: InternedString, value: Value) {
        self.internal.borrow_mut().insert(name, value);
    }
}
