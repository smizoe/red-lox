use std::{cell::RefCell, rc::Rc};

use crate::common::{value::Value, Stack};

use super::{Error, Result};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UpValue {
    internal: Rc<RefCell<UpValueInternal>>,
}

#[derive(Debug, Clone)]
enum UpValueInternal {
    Open { index: usize, stack: Stack },
    Closed(Value),
}

impl PartialEq for UpValueInternal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // For Open variant, we ignore the stack field since there is only one stack.
            (Self::Open { index: l_index, .. }, Self::Open { index: r_index, .. }) => {
                l_index == r_index
            }
            (Self::Closed(l0), Self::Closed(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl UpValue {
    pub fn new(index: usize, stack: Stack) -> Self {
        Self {
            internal: Rc::new(RefCell::new(UpValueInternal::Open { index, stack })),
        }
    }

    /// Gets the Value represented by this UpValue
    pub fn get_value(&self) -> Value {
        match &*self.internal.borrow() {
            UpValueInternal::Open { index, stack } => stack.get_at(*index),
            UpValueInternal::Closed(v) => v.clone(),
        }
    }

    /// Sets the Value to the UpValue.
    pub fn set_value(&mut self, value: Value) {
        match &mut *self.internal.borrow_mut() {
            UpValueInternal::Open { index, stack } => stack.set_at(*index, value),
            UpValueInternal::Closed(v) => *v = value,
        }
    }

    /// Closes this UpValue.
    /// Assumption: when this method is called, the Value at the top of the stack is
    /// what is pointed to by this value.
    pub fn close(&mut self) -> Result<()> {
        let value = match &mut *self.internal.borrow_mut() {
            UpValueInternal::Open { stack, .. } => stack.pop(),
            _ => return Err(Error::DoublyClosedUpvalueError),
        };
        *self.internal.borrow_mut() = UpValueInternal::Closed(value.unwrap());
        Ok(())
    }
}
