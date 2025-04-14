use crate::common::variable_location::UpValue;

use super::LoxFunction;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    fun: LoxFunction,
    upvalues: Vec<UpValue>,
}

impl Closure {
    pub(crate) fn new(fun: LoxFunction, upvalues: Vec<UpValue>) -> Self {
        Self { fun, upvalues }
    }

    pub fn fun(&self) -> &LoxFunction {
        &self.fun
    }

    pub(crate) fn fun_mut(&mut self) -> &mut LoxFunction {
        &mut self.fun
    }
}
