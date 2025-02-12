use std::{num::TryFromIntError, ops::Index};

use crate::{instruction::Instruction, op_code::OpCode};

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<f64>,
    lines: Vec<LineInfo>,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
struct LineInfo {
    offset: usize,
    line: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to add a constant to the chunk: {}", .0)]
    TooManyConstantsError(TryFromIntError),
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, instruction: &Instruction, line: usize) -> Result<(), Error> {
        let offset = self.code.len();
        match instruction {
            Instruction::Return => {
                self.code.push(OpCode::Return.into());
            }
            Instruction::Constant(v) => {
                let index =
                    u8::try_from(self.constants.len()).map_err(Error::TooManyConstantsError)?;
                self.constants.push(*v);
                self.code.push(OpCode::Constant.into());
                self.code.push(index);
            }
        }
        match self.lines.last() {
            Some(line_info) if line_info.line == line => (),
            _ => self.lines.push(LineInfo { offset, line }),
        }
        Ok(())
    }

    pub fn line_of(&self, offset: usize) -> usize {
        if self.lines.len() == 0 {
            return 1;
        }
        match self
            .lines
            .binary_search_by_key(&offset, |LineInfo { offset, line }| *offset)
        {
            Ok(index) => self.lines[index].line,
            Err(index) => self.lines[index - 1].line,
        }
    }
}

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    if cfg!(debug_assertions) {
        disassemble_chunk_internal(chunk, name);
    }
}

fn disassemble_chunk_internal(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset = 0;
    while offset < chunk.code.len() {
        match TryInto::<OpCode>::try_into(chunk.code[offset]) {
            Ok(op) => {
                print_op(op, offset, chunk);
                offset += op.len();
            }
            Err(e) => println!("{:}", e),
        }
    }
}

fn print_op(op: OpCode, offset: usize, chunk: &Chunk) {
    print!("{:04} ", offset);
    let line = chunk.line_of(offset);
    if offset > 0 && line == chunk.line_of(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", line);
    }
    match op {
        OpCode::Constant => {
            let constant_index = chunk.code[offset + 1];
            println!(
                "{:<16} {:04} '{}'",
                op,
                constant_index,
                chunk.constants[usize::from(constant_index)]
            );
        }
        OpCode::Return => println!("{}", op),
    }
}
