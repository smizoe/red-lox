use std::{collections::HashMap, io::Write};

use crate::{
    chunk::Chunk,
    debug::disassemble_instruction,
    interned_string::{intern_string, InternedString},
    op_code::OpCode,
    value::Value,
};

const STACK_MAX: usize = 256;

pub struct VirtualMachine<'a, 'b> {
    chunk: &'a Chunk,
    ip: usize,
    stack: [Option<Value>; STACK_MAX],
    stack_top: usize,
    out: &'b mut dyn Write,
    strings: HashMap<InternedString, Option<u8>>,
    globals: HashMap<InternedString, Value>,
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
    #[error(
        "[At line {line}] runtime error: variable '{var_name}' was accessed before it is defined."
    )]
    UndefinedVariableError { line: usize, var_name: String },
}

impl<'a, 'b> VirtualMachine<'a, 'b> {
    pub fn new(
        chunk: &'a Chunk,
        strings: HashMap<InternedString, Option<u8>>,
        out: &'b mut dyn Write,
    ) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: std::array::from_fn(|_| None),
            stack_top: 0,
            out,
            strings,
            globals: HashMap::new(),
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
                    self.push(v);
                }
                OpCode::Nil => {
                    self.push(Value::Nil);
                }
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::GetLocal => {
                    let index = self.read_byte();
                    self.push(self.stack[index as usize].clone().unwrap());
                }
                OpCode::SetLocal => {
                    let index = self.read_byte();
                    self.stack[index as usize].replace(self.peek(0)?.clone());
                }
                OpCode::GetGlobal => match self.get_constant() {
                    Value::String(s) => {
                        let v =
                            self.globals
                                .get(&s)
                                .ok_or_else(|| Error::UndefinedVariableError {
                                    line: self.line_of(self.ip - 1),
                                    var_name: s.to_string(),
                                })?;
                        self.push(v.clone());
                    }
                    _ => unreachable!(),
                },
                OpCode::DefineGlobal => match self.get_constant() {
                    Value::String(s) => {
                        self.globals.insert(s, self.peek(0)?.clone());
                        self.pop()?;
                    }
                    _ => unreachable!(),
                },
                OpCode::SetGlobal => match self.get_constant() {
                    Value::String(s) => {
                        if !self.globals.contains_key(&s) {
                            return Err(Error::UndefinedVariableError {
                                line: self.line_of(self.ip - 1),
                                var_name: s.to_string(),
                            });
                        }
                        let v = self.peek(0)?.clone();
                        if let Some(ent) = self.globals.get_mut(&s) {
                            *ent = v;
                        }
                    }
                    _ => unreachable!(),
                },
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
                OpCode::Print => {
                    let v = self.pop()?;
                    writeln!(self.out, "{}", v).expect("Failed to write to output.");
                }
                OpCode::JumpIfFalse => {
                    let jump_size = self.read_short();
                    if self.peek(0)?.is_falsy() {
                        self.ip += usize::from(jump_size);
                    }
                }
                OpCode::Jump => {
                    let jump_size = self.read_short();
                    self.ip += usize::from(jump_size);
                }
                OpCode::Loop => {
                    let jump_size = self.read_short();
                    self.ip -= usize::from(jump_size);
                }
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::Add => {
                    self.check_binary_plus_operands()?;
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (a, b) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.push(Value::Number(lhs + rhs))
                        }
                        (Value::String(v), rhs) => {
                            let interned = self.intern_string(&format!("{}{}", v, rhs));
                            self.push(Value::String(interned));
                        }
                        (lhs, Value::String(s)) => {
                            let interned = self.intern_string(&format!("{}{}", lhs, s));
                            self.push(Value::String(interned));
                        }
                        _ => unreachable!(),
                    }
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
                    self.check_binary_comparison_op_operands(op)?;
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let cmp = Value::Bool(if a.is_number() {
                        a.to_number() > b.to_number()
                    } else {
                        a.to_string() > b.to_string()
                    });
                    self.push(cmp);
                }
                OpCode::Less => {
                    self.check_binary_comparison_op_operands(op)?;
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let cmp = Value::Bool(if a.is_number() {
                        a.to_number() < b.to_number()
                    } else {
                        a.to_string() < b.to_string()
                    });
                    self.push(cmp);
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

    fn get_constant(&mut self) -> Value {
        self.chunk.get_constant(self.read_byte().into())
    }

    fn read_byte(&mut self) -> u8 {
        let v = self.chunk.get_code(self.ip);
        self.ip += 1;
        v
    }

    fn read_short(&mut self) -> u16 {
        let v = u16::from_be_bytes([
            self.chunk.get_code(self.ip),
            self.chunk.get_code(self.ip + 1),
        ]);
        self.ip += 2;
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

    fn check_binary_plus_operands(&self) -> Result<(), Error> {
        use Value::*;
        match (self.peek(1)?, self.peek(0)?) {
            (Number(_), Number(_)) => Ok(()),
            (String(_), _) => Ok(()),
            (_, String(_)) => Ok(()),
            (lhs, rhs) => {
                return Err(Error::InvalidOperandError {
                    line: self.line_of(self.ip - 1),
                    msg: format!(
                        "The operands of OP_PLUS must be two numbers or one of them must be a string but got lhs: {}, rhs: {}",
                        lhs.to_type_str(),
                        rhs.to_type_str()
                    ),
                });
            }
        }
    }

    fn check_binary_comparison_op_operands(&self, op_code: OpCode) -> Result<(), Error> {
        use Value::*;
        match (self.peek(1)?, self.peek(0)?) {
            (Number(_), Number(_)) => Ok(()),
            (String(_), String(_)) => Ok(()),
            (lhs, rhs) => {
                return Err(Error::InvalidOperandError {
                    line: self.line_of(self.ip - 1),
                    msg: format!(
                        "The operands of {} must be two numbers or two strings but got lhs: {}, rhs: {}",
                        op_code,
                        lhs.to_type_str(),
                        rhs.to_type_str()
                    ),
                });
            }
        }
    }

    fn check_numeric_binary_op_operands(&self, op_code: OpCode) -> Result<(), Error> {
        use Value::*;
        match (self.peek(1)?, self.peek(0)?) {
            (Number(_), Number(_)) => Ok(()),
            (lhs, rhs) => {
                return Err(Error::InvalidOperandError {
                    line: self.line_of(self.ip - 1),
                    msg: format!(
                        "The operands of {} must be two numbers but got lhs: {}, rhs: {}",
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
                String(_) => println!("[string ]"),
            }
        }
    }

    fn intern_string(&mut self, s: &str) -> InternedString {
        intern_string(&mut self.strings, s)
    }
}
