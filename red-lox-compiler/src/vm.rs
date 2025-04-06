use std::{collections::HashMap, fmt::Write, rc::Rc};

use crate::{
    chunk::Chunk,
    debug::disassemble_instruction,
    interned_string::{InternedString, InternedStringRegistry},
    lox_function::LoxFunction,
    native_function::{self, register_native_functions, NativeFunction},
    op_code::OpCode,
    value::Value,
};

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = 256 * FRAMES_MAX;

pub struct VirtualMachine<'a> {
    stack: [Option<Value>; STACK_MAX],
    stack_top: usize,
    frames: [Option<CallFrame>; FRAMES_MAX],
    frame_count: usize,
    out: &'a mut dyn std::io::Write,
    interned_string_registry: InternedStringRegistry,
    globals: HashMap<InternedString, Value>,
}

struct CallFrame {
    // The function associated with the current frame.
    pub function: Rc<LoxFunction>,
    // The instruction pointer/index for the current frame.
    pub ip: usize,
    // The bottom (the index into the 0-th byte in the stack) of the current frame.
    pub slot_start: usize,
}

impl CallFrame {
    pub fn new(function: Rc<LoxFunction>, ip: usize, slot_index: usize) -> Self {
        Self {
            function,
            ip,
            slot_start: slot_index,
        }
    }

    pub fn last_executed_line(&self) -> usize {
        self.function.chunk().line_of(self.ip - 1)
    }
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
    #[error(
        "[At line {line}] a runtime error occurred while executing native function {name}: {error}"
    )]
    NativeFunctionCallError {
        line: usize,
        name: InternedString,
        error: native_function::Error,
    },
}

impl<'a> VirtualMachine<'a> {
    pub fn new(
        script: LoxFunction,
        interned_string_registry: InternedStringRegistry,
        out: &'a mut dyn std::io::Write,
    ) -> Self {
        let mut vm = Self {
            stack: [const { None }; STACK_MAX],
            stack_top: 0,
            frames: [const { None }; FRAMES_MAX],
            frame_count: 1,
            out,
            interned_string_registry,
            globals: HashMap::new(),
        };
        let fun = Rc::new(script);
        vm.frames[0].replace(CallFrame::new(fun.clone(), 0, 0));
        vm.push(Value::Function(fun));
        register_native_functions(&mut vm.globals, &mut vm.interned_string_registry);
        vm
    }

    pub fn interpret(&mut self) -> Result<(), Error> {
        if cfg!(debug_assertions) {
            println!("== __execution_trace__ ==");
        }
        loop {
            if cfg!(debug_assertions) {
                self.print_stack();
                disassemble_instruction(self.ip(), self.chunk());
            }
            let op = OpCode::try_from(self.read_byte()).map_err(|error| {
                Error::OperationConversionError {
                    line: self.ip() - 1,
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
                    self.push(self.slot(index.into()).clone());
                }
                OpCode::SetLocal => {
                    let index = self.read_byte();
                    *self.slot_mut(index.into()) = self.peek(0)?.clone();
                }
                OpCode::GetGlobal => match self.get_constant() {
                    Value::String(s) => {
                        let v =
                            self.globals
                                .get(&s)
                                .ok_or_else(|| Error::UndefinedVariableError {
                                    line: self.line_of(self.ip() - 1),
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
                                line: self.line_of(self.ip() - 1),
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
                                line: self.line_of(self.ip() - 1),
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
                        *self.ip_mut() += usize::from(jump_size);
                    }
                }
                OpCode::Jump => {
                    let jump_size = self.read_short();
                    *self.ip_mut() += usize::from(jump_size);
                }
                OpCode::Loop => {
                    let jump_size = self.read_short();
                    *self.ip_mut() -= usize::from(jump_size);
                }
                OpCode::Return => {
                    let result = self.pop()?;
                    self.frame_count -= 1;
                    let prev_frame = self.frames[self.frame_count].take().unwrap();
                    if self.frame_count == 0 {
                        return Ok(());
                    }
                    self.stack_top = prev_frame.slot_start;
                    self.push(result);
                }
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    match self.peek(arg_count.into())?.clone() {
                        Value::Function(f) => self.handle_lox_function_call(f, arg_count)?,
                        Value::NativeFunction(nf) => {
                            self.handle_native_function_call(nf, arg_count)?
                        }
                        others => {
                            return Err(Error::InvalidOperandError {
                                line: self.line_of(self.ip() - 1),
                                msg: format!(
                                    "A value of type {} cannot be called.",
                                    others.to_type_str()
                                ),
                            })
                        }
                    };
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

    fn handle_lox_function_call(
        &mut self,
        function: Rc<LoxFunction>,
        arg_count: u8,
    ) -> Result<(), Error> {
        if function.arity != arg_count.into() {
            return Err(Error::InvalidOperandError {
                line: self.line_of(self.ip() - 1),
                msg: format!(
                    "Expected {} arguments but got {}",
                    function.arity, arg_count
                ),
            });
        }
        if self.frame_count == FRAMES_MAX {
            return Err(Error::InvalidOperandError {
                line: self.line_of(self.ip() - 1),
                msg: format!("Stack overflow."),
            });
        }
        self.frames[self.frame_count].replace(CallFrame {
            function,
            ip: 0,
            slot_start: self.stack_top - usize::from(arg_count) - 1,
        });
        self.frame_count += 1;
        Ok(())
    }
    fn handle_native_function_call(
        &mut self,
        nf: Rc<NativeFunction>,
        arg_count: u8,
    ) -> Result<(), Error> {
        let args = self.stack[(self.stack_top - usize::from(arg_count))..self.stack_top]
            .iter()
            .map(|v| v.clone().unwrap())
            .collect::<Vec<Value>>();
        let result = nf.call(args).map_err(|e| Error::NativeFunctionCallError {
            line: self.line_of(self.ip() - 1),
            name: nf.name(),
            error: e,
        })?;
        self.stack_top -= usize::from(arg_count) + 1;
        self.push(result);
        Ok(())
    }

    fn frame(&self) -> &CallFrame {
        self.frames[self.frame_count - 1].as_ref().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames[self.frame_count - 1].as_mut().unwrap()
    }

    fn chunk(&self) -> &Chunk {
        self.frame().function.chunk()
    }

    fn ip(&self) -> usize {
        self.frame().ip
    }

    fn ip_mut(&mut self) -> &mut usize {
        &mut self.frame_mut().ip
    }

    fn get_constant(&mut self) -> Value {
        let id = self.read_byte();
        self.chunk().get_constant(id.into())
    }

    fn read_byte(&mut self) -> u8 {
        let v = self.chunk().get_code(self.ip());
        *self.ip_mut() += 1;
        v
    }

    fn read_short(&mut self) -> u16 {
        let v = u16::from_be_bytes([
            self.chunk().get_code(self.ip()),
            self.chunk().get_code(self.ip() + 1),
        ]);
        *self.ip_mut() += 2;
        v
    }

    fn line_of(&self, offset: usize) -> usize {
        self.chunk().line_of(offset)
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
            line: self.line_of(self.ip() - 1),
        })
    }

    fn peek(&self, depth: usize) -> Result<&Value, Error> {
        self.stack[self.stack_top - 1 - depth].as_ref().ok_or(
            Error::UninitializedStackReferenceError {
                line: self.line_of(self.ip() - 1),
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
                    line: self.line_of(self.ip() - 1),
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
                    line: self.line_of(self.ip() - 1),
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
                    line: self.line_of(self.ip() - 1),
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
                String(s) => println!("[{:^7}]", s),
                Function(f) => println!("[{:^7}]", f),
                nf @ NativeFunction(_) => println!("[{:^7}]", nf),
            }
        }
    }

    fn intern_string(&mut self, s: &str) -> InternedString {
        self.interned_string_registry.intern_string(s)
    }

    fn slot(&self, index: usize) -> &Value {
        self.stack[self.frame().slot_start + index]
            .as_ref()
            .unwrap()
    }

    fn slot_mut(&mut self, index: usize) -> &mut Value {
        self.stack[self.frame().slot_start + index]
            .as_mut()
            .unwrap()
    }

    pub fn stack_trace(&self) -> String {
        let mut trace = String::new();
        for index in (0..self.frame_count).rev() {
            let frame = self.frames[index].as_ref().unwrap();
            writeln!(
                trace,
                "[line {}] in {}",
                frame.last_executed_line(),
                frame.function
            )
            .unwrap();
        }
        trace
    }
}
