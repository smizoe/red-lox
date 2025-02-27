use crate::{chunk::Chunk, debug::disassemble_instruction, op_code::OpCode};

const STACK_MAX: usize = 256;

pub struct VirtualMachine<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: [f64; STACK_MAX],
    stack_top: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("compile error")]
    CompileError,
    #[error("runtime conversion error: {}", .0)]
    RuntimeOperationConversionError(crate::op_code::ConversionError),
}

impl<'a> VirtualMachine<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: [0.0; STACK_MAX],
            stack_top: 0,
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
            let op = OpCode::try_from(self.read_byte())
                .map_err(Error::RuntimeOperationConversionError)?;
            match op {
                OpCode::Constant => {
                    let v = self.get_constant();
                    self.push(v);
                }
                OpCode::Negate => {
                    let v = self.pop();
                    self.push(-v);
                }
                OpCode::Return => {
                    println!("{}", self.pop());
                    return Ok(());
                }
                OpCode::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a + b);
                }
                OpCode::Subtract => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a - b);
                }
                OpCode::Multiply => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a * b);
                }
                OpCode::Divide => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a / b);
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

    fn push(&mut self, value: f64) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> f64 {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    fn print_stack(&self) {
        print!("          ");
        for v in self.stack[0..self.stack_top].iter() {
            print!("[{:^7.3}]", v);
        }
        println!("");
    }
}
