use std::collections::HashMap;

use red_lox_ast::scanner::{Location, Scanner};

use crate::{
    chunk::Chunk,
    instruction::{Arguments, InstructionWithLocation},
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
    #[error("{location} Unsupported argument '{args}' is passed to OpCode {op_code}")]
    UnsupportedArgumentError {
        op_code: OpCode,
        args: Arguments,
        location: Location,
    },
    #[error("{}", .0.into_iter().fold(String::new(), |acc,  e| format!("{}{}\n", acc, e)))]
    CompilationError(Vec<crate::parser::Error>),
}

fn try_get_offset_from(instruction: &InstructionWithLocation) -> Result<u8, Error> {
    instruction
        .args
        .to_offset()
        .ok_or_else(|| Error::UnsupportedArgumentError {
            op_code: instruction.op_code,
            args: instruction.args.clone(),
            location: instruction.location.clone(),
        })
}

fn try_get_interned_string_from(
    instruction: &InstructionWithLocation,
) -> Result<InternedString, Error> {
    instruction
        .args
        .to_interned_string()
        .ok_or_else(|| Error::UnsupportedArgumentError {
            op_code: instruction.op_code,
            args: instruction.args.clone(),
            location: instruction.location.clone(),
        })
}

impl<'a> Compiler<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chunk: Chunk::new(),
            parser: Parser::new(Scanner::new(text)),
        }
    }

    pub fn compile(&mut self) -> Result<(), Error> {
        while let Some(instruction) = self.parser.next_instruction() {
            self.write(&instruction)
                .map_err(|_| Error::TooManyConstantsError {
                    location: instruction.location.clone(),
                })?;
        }
        if !self.parser.errors.is_empty() {
            return Err(Error::CompilationError(std::mem::take(
                &mut self.parser.errors,
            )));
        }
        let cur_loc = self.parser.current.location.clone();
        self.write(&InstructionWithLocation {
            op_code: OpCode::Return,
            args: Arguments::None,
            location: cur_loc,
        })
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

    fn write(&mut self, instruction: &InstructionWithLocation) -> Result<(), Error> {
        let InstructionWithLocation {
            op_code,
            args,
            location,
        } = instruction;
        let offset = self.chunk.code_len();
        match op_code {
            OpCode::GetLocal => {
                let index = try_get_offset_from(instruction)?;
                self.chunk.add_code(OpCode::GetLocal.into());
                self.chunk.add_code(index);
            }
            OpCode::SetLocal => {
                let index = try_get_offset_from(instruction)?;
                self.chunk.add_code(OpCode::SetLocal.into());
                self.chunk.add_code(index);
            }
            OpCode::GetGlobal => {
                let id = try_get_interned_string_from(instruction)?;
                let index = self.add_constant(Value::String(id), location)?;
                self.chunk.add_code(OpCode::GetGlobal.into());
                self.chunk.add_code(index);
            }
            OpCode::DefineGlobal => {
                let id = try_get_interned_string_from(instruction)?;
                let index = self.add_constant(Value::String(id), location)?;
                self.chunk.add_code(OpCode::DefineGlobal.into());
                self.chunk.add_code(index);
            }
            OpCode::SetGlobal => {
                let id = try_get_interned_string_from(instruction)?;
                let index = self.add_constant(Value::String(id), location)?;
                self.chunk.add_code(OpCode::SetGlobal.into());
                self.chunk.add_code(index);
            }
            OpCode::Constant => {
                let index = match args {
                    Arguments::Number(v) => self.add_constant(Value::Number(*v), location),
                    Arguments::String(s) => self.add_constant(Value::String(s.clone()), location),
                    _ => Err(Error::UnsupportedArgumentError {
                        op_code: instruction.op_code,
                        args: instruction.args.clone(),
                        location: instruction.location.clone(),
                    }),
                }?;
                self.chunk.add_code(OpCode::Constant.into());
                self.chunk.add_code(index);
            }
            _ => self.chunk.add_code((*op_code).into()),
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
