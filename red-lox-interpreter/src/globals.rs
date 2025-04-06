use std::{cell::RefCell, rc::Rc, time::SystemTime};

use crate::{
    environment::Environment,
    expr::{Error, Value},
};

pub fn register_globals(environment: &mut Environment) {
    environment.define(
        "clock".to_string(),
        Value::NativeFn {
            name: "clock".to_string(),
            fun: Rc::new(RefCell::new(clock)),
            arity: 0,
        },
    );
}

fn clock(_: Vec<Value>) -> Result<Value, Error> {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|duration| Value::Number(duration.as_secs_f64()))
        .map_err(|e| Error::NativeFunctionCallError {
            msg: format!("Falied to get time: {}", e),
        })
}
