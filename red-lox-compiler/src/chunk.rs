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
    #[error("Failed to add a constant to the chunk since # of constants exeeded the range represented by u8.")]
    TooManyConstantsError,
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
            Instruction::Not => self.code.push(OpCode::Not.into()),
            Instruction::Equal => self.code.push(OpCode::Equal.into()),
            Instruction::Less => self.code.push(OpCode::Less.into()),
            Instruction::Greater => self.code.push(OpCode::Greater.into()),
            Instruction::Constant(v) => {
                let index =
                    u8::try_from(self.constants.len()).map_err(|_| Error::TooManyConstantsError)?;
                self.constants.push(*v);
                self.code.push(OpCode::Constant.into());
                self.code.push(index);
            }
            Instruction::Nil => self.code.push(OpCode::Nil.into()),
            Instruction::Bool(b) => self
                .code
                .push((if *b { OpCode::True } else { OpCode::False }).into()),
            Instruction::Comma => self.code.push(OpCode::Comma.into()),
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
