use std::num::TryFromIntError;

use crate::{instruction::Instruction, op_code::OpCode};

pub struct Chunk {
    pub(crate) code: Vec<u8>,
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
            Instruction::Return => self.code.push(OpCode::Return.into()),
            Instruction::Negate => self.code.push(OpCode::Negate.into()),
            Instruction::Add => self.code.push(OpCode::Add.into()),
            Instruction::Subtract => self.code.push(OpCode::Subtract.into()),
            Instruction::Multiply => self.code.push(OpCode::Multiply.into()),
            Instruction::Divide => self.code.push(OpCode::Divide.into()),
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

    pub fn get_constant(&self, index: usize) -> f64 {
        self.constants[index]
    }

    pub(crate) fn line_of(&self, offset: usize) -> usize {
        if self.lines.len() == 0 {
            return 1;
        }
        match self
            .lines
            .binary_search_by_key(&offset, |LineInfo { offset, .. }| *offset)
        {
            Ok(index) => self.lines[index].line,
            Err(index) => self.lines[index - 1].line,
        }
    }
}
