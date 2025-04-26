use std::fmt::{Debug, Formatter};

use crate::common::value::Value;

use super::STACK_MAX;

pub(crate) struct Stack {
    internal: [Option<Value>; STACK_MAX],
    stack_top: usize,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            internal: [const { None }; STACK_MAX],
            stack_top: 0,
        }
    }

    pub fn stack_top(&self) -> usize {
        self.stack_top
    }

    pub fn set_stack_top(&mut self, value: usize) {
        self.stack_top = value;
    }

    pub fn push(&mut self, value: Value) {
        self.internal[self.stack_top].replace(value);
        self.stack_top += 1;
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack_top -= 1;
        let mut v = None;
        std::mem::swap(&mut self.internal[self.stack_top], &mut v);
        v
    }

    pub fn peek(&self, depth: usize) -> Option<Value> {
        self.internal[self.stack_top - 1 - depth].clone()
    }

    pub fn get_at(&self, index: usize) -> Value {
        self.internal[index].clone().unwrap()
    }

    pub fn set_at(&mut self, index: usize, value: Value) {
        self.internal[index].replace(value);
    }

    pub fn print(&self) {
        for v in self.internal[0..self.stack_top].iter() {
            print!("          ");
            use Value::*;
            match v.as_ref().unwrap() {
                Nil => println!("[  nil  ]"),
                Bool(b) => println!("[{:^7}]", b),
                Number(v) => println!("[{:^7.3}]", v),
                String(s) => println!("[{:^7}]", s),
                Closure(f) => println!("[{:^7}]", f.fun()),
                nf @ NativeFunction(_) => println!("[{:^7}]", nf),
                Class(c) => println!("[{:^7}]", c),
                Instance(i) => println!("[{:^7}]", i),
            }
        }
    }
}

impl Debug for Stack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Stack {{ size: {}, stack_top: {} }}",
            self.internal.len(),
            self.stack_top
        )
    }
}
