use std::collections::HashSet;

use red_lox_ast::scanner::Scanner;

use crate::{
    chunk::{self, Chunk},
    instruction::{Instruction, InstructionWithLocation},
    interned_string::InternedString,
    parser::Parser,
};

pub struct CompilationResult {
    pub chunk: Chunk,
    pub strings: HashSet<InternedString>,
}

pub struct Compiler<'a> {
    chunk: Chunk,
    parser: Parser<'a>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{}", .0)]
    ChunkWriteError(chunk::Error),
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
            self.chunk
                .write(instruction, location.line)
                .map_err(Error::ChunkWriteError)?;
        }
        if !self.parser.errors.is_empty() {
            return Err(Error::CompilationError(std::mem::take(
                &mut self.parser.errors,
            )));
        }
        self.chunk
            .write(Instruction::Return, self.parser.current.location.line)
            .map_err(Error::ChunkWriteError)?;
        Ok(())
    }

    pub fn finish(self) -> CompilationResult {
        CompilationResult {
            chunk: self.chunk,
            strings: self.parser.strings,
        }
    }
}
