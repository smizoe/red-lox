use std::io::Write;

use crate::{chunk::Chunk, debug::disassemble_instruction, op_code::OpCode, value::Value};

const STACK_MAX: usize = 256;

pub struct VirtualMachine<'a, 'b, 'c> {
    chunk: &'a Chunk,
    ip: usize,
    stack: [Option<Value>; STACK_MAX],
    stack_top: usize,
    out: &'b mut dyn Write,
    err: &'c mut dyn Write,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("compile error")]
    CompileError,
    #[error("[At line {line}] runtime conversion error: {error}")]
    OperationConversionError {
        line: usize,
        error: crate::op_code::ConversionError,
    },
    #[error("[At line {line}] runtime operation error: {msg}")]
    InvalidOperandError { line: usize, msg: String },
    #[error(
        "[At line {line}] runtime error: tried to refer to an uninitialized location in a stack"
    )]
    UninitializedStackReferenceError { line: usize },
}

impl<'a, 'b, 'c> VirtualMachine<'a, 'b, 'c> {
    pub fn new(chunk: &'a Chunk, out: &'b mut dyn Write, err: &'c mut dyn Write) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: std::array::from_fn(|_| None),
            stack_top: 0,
            out,
            err,
        }
    }

    pub fn interpret(&mut self) -> Result<(), Error> {
        if cfg!(debug_assertions) {
            println!("== __execution_trace__ ==");
        }
        loop {
            if cfg!(debug_assertions) {
                self.print_stack();
                disassemble_instruction(self.ip, &self.chunk);
            }
            let op = OpCode::try_from(self.read_byte()).map_err(|error| {
                Error::OperationConversionError {
                    line: self.ip - 1,
                    error,
                }
            })?;
            match op {
                OpCode::Constant => {
                    let v = self.get_constant();
                    self.push(Value::Number(v));
                }
                OpCode::Nil => {
                    self.push(Value::Nil);
                }
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Bool(a == b));
                }
                OpCode::Negate => {
                    {
                        let v = self.peek(0)?;
                        if !v.is_number() {
                            return Err(Error::InvalidOperandError {
                                line: self.line_of(self.ip - 1),
                                msg: format!(
                                    "Operand of unary minus must be a number but found a {}",
                                    v.to_type_str()
                                ),
                            });
                        }
                    }
                    let v = self.pop()?.to_number();
                    self.push(Value::Number(-v));
                }
                OpCode::Return => {
                    let v = self.pop()?;
                    writeln!(self.out, "{}", v).expect("Failed to write to output.");
                    return Ok(());
                }
                OpCode::Add => {
                    self.check_numeric_binary_op_operands(op)?;
                    let b = self.pop()?.to_number();
                    let a = self.pop()?.to_number();
                    self.push(Value::Number(a + b));
                }
                OpCode::Subtract => {
                    self.check_numeric_binary_op_operands(op)?;
                    let b = self.pop()?.to_number();
                    let a = self.pop()?.to_number();
                    self.push(Value::Number(a - b));
                }
                OpCode::Multiply => {
                    self.check_numeric_binary_op_operands(op)?;
                    let b = self.pop()?.to_number();
                    let a = self.pop()?.to_number();
                    self.push(Value::Number(a * b));
                }
                OpCode::Divide => {
                    self.check_numeric_binary_op_operands(op)?;
                    let b = self.pop()?.to_number();
                    let a = self.pop()?.to_number();
                    self.push(Value::Number(a / b));
                }
                OpCode::Greater => {
                    self.check_numeric_binary_op_operands(op)?;
                    let b = self.pop()?.to_number();
                    let a = self.pop()?.to_number();
                    self.push(Value::Bool(a > b));
                }
                OpCode::Less => {
                    self.check_numeric_binary_op_operands(op)?;
                    let b = self.pop()?.to_number();
                    let a = self.pop()?.to_number();
                    self.push(Value::Bool(a < b));
                }
                OpCode::Not => {
                    let v = self.pop()?.is_falsy();
                    self.push(Value::Bool(v));
                }
                OpCode::Comma => {
                    let top = self.pop()?;
                    let _ = self.pop();
                    self.push(top);
                }
            }
        }
    }

    fn get_constant(&mut self) -> f64 {
        self.chunk.get_constant(self.read_byte().into())
    }

    fn read_byte(&mut self) -> u8 {
        let v = self.chunk.code[self.ip];
        self.ip += 1;
        v
    }

    fn line_of(&self, offset: usize) -> usize {
        self.chunk.line_of(offset)
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = Some(value);
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack_top -= 1;
        let mut v = None;
        std::mem::swap(&mut self.stack[self.stack_top], &mut v);
        v.ok_or(Error::UninitializedStackReferenceError {
            line: self.line_of(self.ip - 1),
        })
    }

    fn peek(&self, depth: usize) -> Result<&Value, Error> {
        self.stack[self.stack_top - 1 - depth].as_ref().ok_or(
            Error::UninitializedStackReferenceError {
                line: self.line_of(self.ip - 1),
            },
        )
    }

    fn check_numeric_binary_op_operands(&self, op_code: OpCode) -> Result<(), Error> {
        use Value::*;
        match (self.peek(1)?, self.peek(0)?) {
            (Number(_), Number(_)) => Ok(()),
            (lhs, rhs) => {
                return Err(Error::InvalidOperandError {
                    line: self.line_of(self.ip - 1),
                    msg: format!(
                        "The operands of {} should be two numbers but got lhs: {}, rhs: {}",
                        op_code,
                        lhs.to_type_str(),
                        rhs.to_type_str()
                    ),
                });
            }
        }
    }

    fn print_stack(&self) {
        for v in self.stack[0..self.stack_top].iter() {
            print!("          ");
            use Value::*;
            match v.as_ref().unwrap() {
                Nil => println!("[  nil  ]"),
                Bool(b) => println!("[{:^7}]", b),
                Number(v) => println!("[{:^7.3}]", v),
            }
        }
    }
}
