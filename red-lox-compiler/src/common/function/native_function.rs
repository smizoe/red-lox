use std::{cell::RefCell, collections::HashMap, rc::Rc, time::SystemTime};

use crate::{
    common::function::{Error, Result},
    common::value::Value,
    common::{InternedString, InternedStringRegistry},
};

pub(crate) struct NativeFunction {
    fun: RefCell<Box<dyn FnMut(Vec<Value>) -> Result<Value>>>,
    name: InternedString,
    arity: usize,
}

impl NativeFunction {
    pub(crate) fn new(
        fun: RefCell<Box<dyn FnMut(Vec<Value>) -> Result<Value>>>,
        name: InternedString,
        arity: usize,
    ) -> Self {
        Self { fun, name, arity }
    }

    pub(crate) fn call(&self, args: Vec<Value>) -> Result<Value> {
        if args.len() != self.arity {
            return Err(Error::FunctionArityMismatchError {
                name: self.name.clone(),
                args_len: args.len(),
                arity: self.arity,
            });
        }
        self.fun.borrow_mut()(args)
    }

    pub(crate) fn name(&self) -> InternedString {
        self.name.clone()
    }
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn: {}>", self.name)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

fn clock(_args: Vec<Value>) -> Result<Value> {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|duration| Value::Number(duration.as_secs_f64()))
        .map_err(|e| Error::NativeFunctionCallError {
            msg: format!("Falied to get time: {}", e),
        })
}

pub fn register_native_functions(
    globals: &mut HashMap<InternedString, Value>,
    interned_string_registry: &mut InternedStringRegistry,
) {
    let clcok_name = interned_string_registry.intern_string("clock");
    globals.insert(
        clcok_name.clone(),
        Value::NativeFunction(Rc::new(NativeFunction::new(
            RefCell::new(Box::new(clock)),
            clcok_name,
            0,
        ))),
    );
}
