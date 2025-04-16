use std::{collections::HashMap, fmt::Write, rc::Rc};

use crate::common::{
    chunk::{debug::disassemble_instruction, Chunk},
    function::{register_native_functions, Closure, NativeFunction, UpValue},
    op_code::OpCode,
    value::Value,
    InternedString, InternedStringRegistry, Stack, FRAMES_MAX,
};

use super::{CallFrame, Error};

pub struct VirtualMachine<'a> {
    stack: Stack,
    frames: [Option<CallFrame>; FRAMES_MAX],
    frame_count: usize,
    out: &'a mut dyn std::io::Write,
    interned_string_registry: InternedStringRegistry,
    globals: HashMap<InternedString, Value>,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(
        script: Closure,
        interned_string_registry: InternedStringRegistry,
        out: &'a mut dyn std::io::Write,
    ) -> Self {
        let mut vm = Self {
            stack: Stack::new(),
            frames: [const { None }; FRAMES_MAX],
            frame_count: 1,
            out,
            interned_string_registry,
            globals: HashMap::new(),
        };
        let fun = script;
        vm.frames[0].replace(CallFrame::new(fun.clone(), 0, 0));
        vm.push(Value::Closure(fun));
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
                    self.push(self.slot(index.into()));
                }
                OpCode::SetLocal => {
                    let index = self.read_byte();
                    self.set_slot(index.into(), self.peek(0)?);
                }
                OpCode::GetUpValue => {
                    let upvalue_index = self.read_byte();
                    let v = self
                        .frame()
                        .closure
                        .get_upvalue(upvalue_index.into())
                        .get_value();
                    self.push(v);
                }
                OpCode::SetUpValue => {
                    let upvalue_index = self.read_byte();
                    let v = self.peek(0)?;
                    self.frame_mut()
                        .closure
                        .get_upvalue_mut(upvalue_index.into())
                        .set_value(v);
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
                        self.globals.insert(s, self.peek(0)?);
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
                        let v = self.peek(0)?;
                        if let Some(ent) = self.globals.get_mut(&s) {
                            *ent = v;
                        }
                    }
                    _ => unreachable!(),
                },
                OpCode::Closure => {
                    let mut closure = self.get_constant();
                    let c = match &mut closure {
                        Value::Closure(c) => c,
                        _ => unreachable!(),
                    };
                    for _ in 0..c.upvalue_count() {
                        let is_local = self.read_byte();
                        let index = self.read_byte();
                        if is_local > 0 {
                            c.add_upvalue(UpValue::new(
                                self.frame().slot_start + usize::from(index),
                                self.stack.clone(),
                            ));
                        } else {
                            c.add_upvalue(self.frame().closure.get_upvalue(index.into()).clone());
                        }
                    }
                    self.push(closure);
                }
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
                    self.stack.set_stack_top(prev_frame.slot_start);
                    self.push(result);
                }
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    match self.peek(arg_count.into())? {
                        Value::Closure(f) => self.handle_lox_function_call(f, arg_count)?,
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

    fn handle_lox_function_call(&mut self, closure: Closure, arg_count: u8) -> Result<(), Error> {
        if closure.fun().arity != arg_count.into() {
            return Err(Error::InvalidOperandError {
                line: self.line_of(self.ip() - 1),
                msg: format!(
                    "Expected {} arguments but got {}",
                    closure.fun().arity,
                    arg_count
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
            closure,
            ip: 0,
            slot_start: self.stack.stack_top() - usize::from(arg_count) - 1,
        });
        self.frame_count += 1;
        Ok(())
    }

    fn handle_native_function_call(
        &mut self,
        nf: Rc<NativeFunction>,
        arg_count: u8,
    ) -> Result<(), Error> {
        let args = (0..usize::from(arg_count))
            .rev()
            .map(|i| self.stack.peek(i).unwrap())
            .collect::<Vec<Value>>();
        let result = nf.call(args).map_err(|e| Error::NativeFunctionCallError {
            line: self.line_of(self.ip() - 1),
            name: nf.name(),
            error: e,
        })?;
        self.stack
            .set_stack_top(self.stack.stack_top() - (usize::from(arg_count) + 1));
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
        self.frame().closure.fun().chunk()
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
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack
            .pop()
            .ok_or(Error::UninitializedStackReferenceError {
                line: self.line_of(self.ip() - 1),
            })
    }

    fn peek(&self, depth: usize) -> Result<Value, Error> {
        self.stack
            .peek(depth)
            .ok_or(Error::UninitializedStackReferenceError {
                line: self.line_of(self.ip() - 1),
            })
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
        self.stack.print();
    }

    fn intern_string(&mut self, s: &str) -> InternedString {
        self.interned_string_registry.intern_string(s)
    }

    fn slot(&self, index: usize) -> Value {
        self.stack.get_at(self.frame().slot_start + index)
    }

    fn set_slot(&mut self, index: usize, value: Value) {
        self.stack.set_at(self.frame().slot_start + index, value);
    }

    pub fn stack_trace(&self) -> String {
        let mut trace = String::new();
        for index in (0..self.frame_count).rev() {
            let frame = self.frames[index].as_ref().unwrap();
            writeln!(
                trace,
                "[line {}] in {}",
                frame.last_executed_line(),
                frame.closure.fun()
            )
            .unwrap();
        }
        trace
    }
}
