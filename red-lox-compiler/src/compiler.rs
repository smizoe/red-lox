use std::collections::HashMap;

use red_lox_ast::scanner::{Location, Scanner};

use crate::{
    chunk::Chunk,
    instruction::{Instruction, InstructionWithLocation},
    interned_string::InternedString,
    op_code::OpCode,
    parser::Parser,
    value::Value,
};

pub struct CompilationResult {
    pub chunk: Chunk,
    pub strings: HashMap<InternedString, Option<u8>>,
}

pub struct Compiler<'a> {
    chunk: Chunk,
    parser: Parser<'a>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{location} Failed to add a constant to the chunk since # of constants exeeded the range represented by u8.")]
    TooManyConstantsError { location: Location },
    #[error("{}", .0.into_iter().fold(String::new(), |acc,  e| format!("{}{}\n", acc, e)))]
    CompilationError(Vec<crate::parser::Error>),
}

impl<'a> Compiler<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chunk: Chunk::new(),
            parser: Parser::new(Scanner::new(text)),
        }
    }

    pub fn compile(&mut self) -> Result<(), Error> {
        while let Some(InstructionWithLocation {
            instruction,
            location,
        }) = self.parser.next_instruction()
        {
            self.write(instruction, &location)
                .map_err(|_| Error::TooManyConstantsError { location })?;
        }
        if !self.parser.errors.is_empty() {
            return Err(Error::CompilationError(std::mem::take(
                &mut self.parser.errors,
            )));
        }
        let cur_loc = self.parser.current.location.clone();
        self.write(Instruction::Return, &cur_loc)
            .map_err(|_| Error::TooManyConstantsError {
                location: self.parser.current.location.clone(),
            })?;
        Ok(())
    }

    pub fn finish(self) -> CompilationResult {
        CompilationResult {
            chunk: self.chunk,
            strings: self.parser.strings,
        }
    }

    fn write(&mut self, instruction: Instruction, location: &Location) -> Result<(), Error> {
        let offset = self.chunk.code_len();
        match instruction {
            Instruction::Return => self.chunk.add_code(OpCode::Return.into()),
            Instruction::Negate => self.chunk.add_code(OpCode::Negate.into()),
            Instruction::Add => self.chunk.add_code(OpCode::Add.into()),
            Instruction::Subtract => self.chunk.add_code(OpCode::Subtract.into()),
            Instruction::Multiply => self.chunk.add_code(OpCode::Multiply.into()),
            Instruction::Divide => self.chunk.add_code(OpCode::Divide.into()),
            Instruction::Not => self.chunk.add_code(OpCode::Not.into()),
            Instruction::Pop => self.chunk.add_code(OpCode::Pop.into()),
            Instruction::Equal => self.chunk.add_code(OpCode::Equal.into()),
            Instruction::Less => self.chunk.add_code(OpCode::Less.into()),
            Instruction::Greater => self.chunk.add_code(OpCode::Greater.into()),
            Instruction::Print => self.chunk.add_code(OpCode::Print.into()),
            Instruction::GetLocal(index) => {
                self.chunk.add_code(OpCode::GetLocal.into());
                self.chunk.add_code(index);
            }
            Instruction::SetLocal(index) => {
                self.chunk.add_code(OpCode::SetLocal.into());
                self.chunk.add_code(index);
            }
            Instruction::GetGlobal(id) => {
                let index = self.add_constant(Value::String(id), location)?;
                self.chunk.add_code(OpCode::GetGlobal.into());
                self.chunk.add_code(index);
            }
            Instruction::DefineGlobal(id) => {
                let index = self.add_constant(Value::String(id), location)?;
                self.chunk.add_code(OpCode::DefineGlobal.into());
                self.chunk.add_code(index);
            }
            Instruction::SetGlobal(id) => {
                let index = self.add_constant(Value::String(id), location)?;
                self.chunk.add_code(OpCode::SetGlobal.into());
                self.chunk.add_code(index);
            }
            Instruction::Constant(v) => {
                let index = self.add_constant(Value::Number(v), location)?;
                self.chunk.add_code(OpCode::Constant.into());
                self.chunk.add_code(index);
            }
            Instruction::Nil => self.chunk.add_code(OpCode::Nil.into()),
            Instruction::Bool(b) => self
                .chunk
                .add_code((if b { OpCode::True } else { OpCode::False }).into()),
            Instruction::String(s) => {
                let index = self.add_constant(Value::String(s), location)?;
                self.chunk.add_code(OpCode::Constant.into());
                self.chunk.add_code(index);
            }
            Instruction::Comma => self.chunk.add_code(OpCode::Comma.into()),
        }
        self.chunk.maybe_update_line_info(offset, location.line);
        Ok(())
    }

    fn add_constant(&mut self, value: Value, location: &Location) -> Result<u8, Error> {
        match &value {
            Value::String(s) => match self.parser.strings.get(s) {
                Some(Some(v)) => return Ok(*v),
                _ => {
                    // this is Some(None)
                    let index = self.get_next_index_for_constant(&location)?;
                    self.parser.strings.insert(s.clone(), Some(index));
                    self.chunk.add_constant(value);
                    Ok(index)
                }
            },
            _ => {
                let index = self.get_next_index_for_constant(&location)?;
                self.chunk.add_constant(value);
                Ok(index)
            }
        }
    }

    fn get_next_index_for_constant(&mut self, location: &Location) -> Result<u8, Error> {
        u8::try_from(self.chunk.get_num_constants()).map_err(move |_| {
            Error::TooManyConstantsError {
                location: location.clone(),
            }
        })
    }
}
