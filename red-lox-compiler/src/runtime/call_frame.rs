use crate::common::function::Closure;

pub(super) struct CallFrame {
    // The closure associated with the current frame.
    pub closure: Box<Closure>,
    // The instruction pointer/index for the current frame.
    pub ip: usize,
    // The bottom (the index into the 0-th byte in the stack) of the current frame.
    pub slot_start: usize,
}

impl CallFrame {
    pub fn new(closure: Box<Closure>, ip: usize, slot_index: usize) -> Self {
        Self {
            closure,
            ip,
            slot_start: slot_index,
        }
    }

    pub fn last_executed_line(&self) -> usize {
        self.closure.fun().chunk().line_of(self.ip - 1)
    }
}
