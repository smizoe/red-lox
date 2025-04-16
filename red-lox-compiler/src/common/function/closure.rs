use std::rc::Rc;

use crate::common::Stack;

use super::{LoxFunction, UpValue};

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    fun: Rc<LoxFunction>,
    upvalues: Vec<UpValue>,
    upvalue_count: usize,
}

impl Closure {
    pub(crate) fn new(fun: LoxFunction, upvalue_count: usize) -> Self {
        Self {
            fun: Rc::new(fun),
            upvalues: Vec::new(),
            upvalue_count,
        }
    }

    pub fn fun(&self) -> &LoxFunction {
        &self.fun
    }

    pub fn upvalue_count(&self) -> usize {
        self.upvalue_count
    }

    pub fn add_upvalue(&mut self, upvalue: UpValue) {
        self.upvalues.push(upvalue);
    }

    pub(crate) fn get_upvalue(&self, index: usize) -> &UpValue {
        &self.upvalues[index]
    }

    pub(crate) fn get_upvalue_mut(&mut self, index: usize) -> &mut UpValue {
        &mut self.upvalues[index]
    }
}
