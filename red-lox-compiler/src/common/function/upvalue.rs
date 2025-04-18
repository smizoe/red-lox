use std::{cell::RefCell, rc::Rc};

use crate::common::{value::Value, Stack};

use super::{Error, Result};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UpValue {
    internal: Rc<RefCell<UpValueInternal>>,
}

#[derive(Debug, Clone)]
enum UpValueInternal {
    Open {
        index: usize,
        stack: Rc<RefCell<Stack>>,
    },
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
    pub fn new(index: usize, stack: Rc<RefCell<Stack>>) -> Self {
        Self {
            internal: Rc::new(RefCell::new(UpValueInternal::Open { index, stack })),
        }
    }

    /// Gets the Value represented by this UpValue
    pub fn get_value(&self) -> Value {
        match &*self.internal.borrow() {
            UpValueInternal::Open { index, stack } => stack.borrow().get_at(*index),
            UpValueInternal::Closed(v) => v.clone(),
        }
    }

    /// Sets the Value to the UpValue.
    pub fn set_value(&mut self, value: Value) {
        match &mut *self.internal.borrow_mut() {
            UpValueInternal::Open { index, stack } => stack.borrow_mut().set_at(*index, value),
            UpValueInternal::Closed(v) => *v = value,
        }
    }

    /// Gets the index part of the open upvalue.
    /// If the object called on is a closed upvalue, this returns an Err(e) value.
    pub fn get_index(&self) -> Result<usize> {
        match &*self.internal.borrow() {
            UpValueInternal::Closed(_) => Err(Error::ClosedUpValueLocationRequestError),
            UpValueInternal::Open { index, .. } => Ok(*index),
        }
    }

    /// Closes this UpValue.
    pub fn close(&mut self) -> Result<()> {
        if self.is_closed() {
            return Err(Error::DoublyClosedUpvalueError);
        }
        let value = self.get_value();
        *self.internal.borrow_mut() = UpValueInternal::Closed(value);
        Ok(())
    }

    pub fn is_closed(&self) -> bool {
        match &*self.internal.borrow() {
            UpValueInternal::Closed(_) => true,
            _ => false,
        }
    }
}
