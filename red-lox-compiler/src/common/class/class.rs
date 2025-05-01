use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::common::{function::Closure, value::Value, InternedString};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Class {
    name: InternedString,
    methods: Methods,
}

impl Class {
    pub(crate) fn new(name: InternedString) -> Self {
        Self {
            name,
            methods: Methods::new(),
        }
    }

    pub(crate) fn get_method<S>(&self, name: &S) -> Option<Box<Closure>>
    where
        S: AsRef<str>,
    {
        self.methods.get(name)
    }

    pub(crate) fn set_method(&self, name: InternedString, value: Box<Closure>) {
        self.methods.set(name, value);
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "class {}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Methods {
    internal: Rc<RefCell<HashMap<InternedString, Box<Closure>>>>,
}

impl Methods {
    fn new() -> Self {
        Self {
            internal: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    fn get<S>(&self, name: &S) -> Option<Box<Closure>>
    where
        S: AsRef<str>,
    {
        self.internal.borrow().get(name.as_ref()).cloned()
    }

    fn set(&self, name: InternedString, value: Box<Closure>) {
        self.internal.borrow_mut().insert(name, value);
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

    pub(crate) fn get_method<S>(&self, name: &S) -> Option<Box<Closure>>
    where
        S: AsRef<str>,
    {
        self.class.get_method(name)
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BoundMethod {
    receiver: Box<Instance>,
    method: Box<Closure>,
}

impl Display for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.method.fun())
    }
}

impl BoundMethod {
    pub(crate) fn new(receiver: Box<Instance>, method: Box<Closure>) -> Self {
        Self { receiver, method }
    }

    pub(crate) fn destructure(self) -> (Box<Instance>, Box<Closure>) {
        (self.receiver, self.method)
    }
}
