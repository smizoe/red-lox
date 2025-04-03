use std::{collections::HashMap, rc::Rc};

use red_lox_ast::scanner::{Location, Scanner};

use crate::{
    chunk::Chunk,
    code_location_registry::{BackPatchLocationKey, CodeLocationRegistry, LabelKey, LabelType},
    interned_string::InternedString,
    lox_function::LoxFunction,
    op_code::OpCode,
    parser::Parser,
    value::Value,
    write_action::{Arguments, WriteAction},
};

pub struct CompilationResult {
    pub script: LoxFunction,
    pub strings: HashMap<InternedString, Option<u8>>,
}

pub struct Compiler<'a> {
    functions: Vec<LoxFunction>,
    parser: Parser<'a>,
    code_location_registry: CodeLocationRegistry,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{location} Failed to add a constant to the chunk since # of constants exeeded the range represented by u8.")]
    TooManyConstantsError { location: Location },
    #[error("{location} Too much code to jump over.")]
    TooLongJumpError { location: Location },
    #[error("{location} Unsupported argument '{args}' is passed to OpCode {op_code}")]
    UnsupportedArgumentError {
        op_code: OpCode,
        args: String,
        location: Location,
    },
    #[error("{}", .0.into_iter().fold(String::new(), |acc,  e| format!("{}{}\n", acc, e)))]
    CompilationError(Vec<crate::parser::Error>),
}

fn try_get_offset_from(
    op_code: OpCode,
    args: &Arguments,
    location: &Location,
) -> Result<u8, Error> {
    args.to_offset()
        .ok_or_else(|| Error::UnsupportedArgumentError {
            op_code,
            args: args.to_string(),
            location: location.clone(),
        })
}

fn try_get_interned_string_from(
    op_code: OpCode,
    args: &Arguments,
    location: &Location,
) -> Result<InternedString, Error> {
    args.to_interned_string()
        .ok_or_else(|| Error::UnsupportedArgumentError {
            op_code,
            args: args.to_string(),
            location: location.clone(),
        })
}

impl<'a> Compiler<'a> {
    /// Creates a new compiler that compiles the given `text``.
    pub fn new(text: &'a str) -> Self {
        Self {
            functions: vec![LoxFunction::new(InternedString::get_empty_string(), 0)],
            parser: Parser::new(Scanner::new(text)),
            code_location_registry: CodeLocationRegistry::new(),
        }
    }

    /// Compiles the text passed to `Compiler::new` method.
    /// Returns an Ok value when the compilation was successful.
    /// An error value is returned when there is any error encountered during the compilation.
    pub fn compile(&mut self) -> Result<(), Error> {
        while let Some(write_action) = self.parser.next_write() {
            self.write(&write_action)
                .map_err(|_| Error::TooManyConstantsError {
                    location: write_action.get_location().clone(),
                })?;
        }
        if !self.parser.errors.is_empty() {
            return Err(Error::CompilationError(std::mem::take(
                &mut self.parser.errors,
            )));
        }
        let cur_loc = self.parser.current.location.clone();
        self.write(&WriteAction::OpCodeWrite {
            op_code: OpCode::Return,
            args: Arguments::None,
            location: cur_loc,
        })
        .map_err(|_| Error::TooManyConstantsError {
            location: self.parser.current.location.clone(),
        })?;
        Ok(())
    }

    pub fn finish(mut self) -> CompilationResult {
        CompilationResult {
            script: self.functions.pop().unwrap(),
            strings: self.parser.strings,
        }
    }

    /// Handles a single `WriteAction`.
    /// See each function called internally for what occurs when each variant of the enum arrives.
    fn write(&mut self, write_action: &WriteAction) -> Result<(), Error> {
        match write_action {
            WriteAction::OpCodeWrite {
                op_code,
                args,
                location,
            } => self.write_op_code(*op_code, args, location),
            WriteAction::BackPatchJumpLocation {
                label_type: usage,
                location,
            } => self.back_patch_jump_location(*usage, location.clone()),
            WriteAction::AddLabel {
                label_type: usage,
                location,
            } => {
                self.add_label(*usage, location.clone());
                Ok(())
            }
            WriteAction::FunctionDeclaration { name, arity, .. } => {
                self.functions.push(LoxFunction::new(name.clone(), *arity));
                Ok(())
            }
            WriteAction::FunctionDeclarationEnd {
                is_global,
                location,
            } => {
                let defined = self.functions.pop().unwrap();
                let name = defined.name.clone();
                self.write_op_code(
                    OpCode::Constant,
                    &Arguments::Function(Rc::new(defined)),
                    location,
                )?;
                if *is_global {
                    self.write_op_code(OpCode::DefineGlobal, &Arguments::String(name), location)?;
                }
                Ok(())
            }
        }
    }

    /// Writes bytecodes based on the given OpCode and Argument.
    fn write_op_code(
        &mut self,
        op_code: OpCode,
        args: &Arguments,
        location: &Location,
    ) -> Result<(), Error> {
        let offset = self.current_chunk().code_len();
        match op_code {
            OpCode::GetLocal => {
                let index = try_get_offset_from(op_code, args, location)?;
                self.current_chunk_mut().add_code(OpCode::GetLocal.into());
                self.current_chunk_mut().add_code(index);
            }
            OpCode::SetLocal => {
                let index = try_get_offset_from(op_code, args, location)?;
                self.current_chunk_mut().add_code(OpCode::SetLocal.into());
                self.current_chunk_mut().add_code(index);
            }
            OpCode::GetGlobal => {
                let id = try_get_interned_string_from(op_code, args, location)?;
                let index = self.add_constant(Value::String(id), location)?;
                self.current_chunk_mut().add_code(OpCode::GetGlobal.into());
                self.current_chunk_mut().add_code(index);
            }
            OpCode::DefineGlobal => {
                let id = try_get_interned_string_from(op_code, args, location)?;
                let index = self.add_constant(Value::String(id), location)?;
                self.current_chunk_mut()
                    .add_code(OpCode::DefineGlobal.into());
                self.current_chunk_mut().add_code(index);
            }
            OpCode::SetGlobal => {
                let id = try_get_interned_string_from(op_code, args, location)?;
                let index = self.add_constant(Value::String(id), location)?;
                self.current_chunk_mut().add_code(OpCode::SetGlobal.into());
                self.current_chunk_mut().add_code(index);
            }
            OpCode::Constant => {
                let index = match args {
                    Arguments::Number(v) => self.add_constant(Value::Number(*v), location),
                    Arguments::String(s) => self.add_constant(Value::String(s.clone()), location),
                    Arguments::Function(f) => {
                        self.add_constant(Value::Function(f.clone()), location)
                    }
                    _ => Err(Error::UnsupportedArgumentError {
                        op_code,
                        args: args.to_string(),
                        location: location.clone(),
                    }),
                }?;
                self.current_chunk_mut().add_code(OpCode::Constant.into());
                self.current_chunk_mut().add_code(index);
            }
            OpCode::JumpIfFalse | OpCode::Jump => {
                self.current_chunk_mut().add_code(op_code.into());
                self.add_backpatch_location(
                    op_code,
                    args.to_label_type().unwrap(),
                    location.clone(),
                );
                self.current_chunk_mut().add_code(u8::MAX);
                self.current_chunk_mut().add_code(u8::MAX);
            }
            OpCode::Loop => {
                self.current_chunk_mut().add_code(op_code.into());
                // The backpatch logic is used sa as to support patching multiple location
                // (e.g. the continue statement).
                self.add_backpatch_location(
                    op_code,
                    args.to_label_type().unwrap(),
                    location.clone(),
                );
                self.current_chunk_mut().add_code(u8::MAX);
                self.current_chunk_mut().add_code(u8::MAX);
            }
            _ => self.current_chunk_mut().add_code(op_code.into()),
        }
        self.current_chunk_mut()
            .maybe_update_line_info(offset, location.line);
        Ok(())
    }

    fn add_label(&mut self, usage: LabelType, location: Location) {
        self.code_location_registry.add_label(
            LabelKey::new(usage, location),
            self.current_chunk().code_len(),
        );
    }

    fn add_backpatch_location(&mut self, op_code: OpCode, usage: LabelType, location: Location) {
        self.code_location_registry.add_backpatch_location(
            BackPatchLocationKey::new(usage, location),
            op_code,
            self.current_chunk().code_len(),
        );
    }

    fn back_patch_jump_location(
        &mut self,
        usage: LabelType,
        location: Location,
    ) -> Result<(), Error> {
        let key = BackPatchLocationKey::new(usage, location.clone());
        for (op_code, backpatch_starts) in self
            .code_location_registry
            .remove_backpatch_location(&key)
            .into_iter()
        {
            match op_code {
                OpCode::JumpIfFalse | OpCode::Jump => {
                    for backpatch_start in backpatch_starts {
                        let jump_offset = self.current_chunk().code_len() - backpatch_start - 2;
                        let values = u16::try_from(jump_offset)
                            .map_err(|_| Error::TooLongJumpError {
                                location: location.clone(),
                            })?
                            .to_be_bytes();
                        self.current_chunk_mut()
                            .set_code(backpatch_start, values[0]);
                        self.current_chunk_mut()
                            .set_code(backpatch_start + 1, values[1]);
                    }
                }
                OpCode::Loop => {
                    let jump_target = self
                        .code_location_registry
                        .remove_label(&LabelKey::new(usage, location.clone()))
                        .unwrap();
                    for backpatch_start in backpatch_starts {
                        let jump = backpatch_start - jump_target + 2;
                        let values = u16::try_from(jump)
                            .map_err(|_| Error::TooLongJumpError {
                                location: location.clone(),
                            })?
                            .to_be_bytes();
                        self.current_chunk_mut()
                            .set_code(backpatch_start, values[0]);
                        self.current_chunk_mut()
                            .set_code(backpatch_start + 1, values[1]);
                    }
                }
                _ => unreachable!(),
            }
        }
        self.code_location_registry
            .remove_label(&LabelKey::new(usage, location));
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
                    self.current_chunk_mut().add_constant(value);
                    Ok(index)
                }
            },
            _ => {
                let index = self.get_next_index_for_constant(&location)?;
                self.current_chunk_mut().add_constant(value);
                Ok(index)
            }
        }
    }

    fn get_next_index_for_constant(&mut self, location: &Location) -> Result<u8, Error> {
        u8::try_from(self.current_chunk().get_num_constants()).map_err(move |_| {
            Error::TooManyConstantsError {
                location: location.clone(),
            }
        })
    }

    fn current_chunk(&self) -> &Chunk {
        &self.functions.last().unwrap().chunk
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.functions.last_mut().unwrap().chunk
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::debug::disassemble_chunk_for_testing;

    use super::Compiler;

    fn compile(text: &str) -> Result<String, String> {
        let mut compiler = Compiler::new(text);
        if let Err(e) = compiler.compile() {
            return Err(format!(
                "One or more errors occurred during compilation: {:?}",
                e
            ));
        }
        let result = compiler.finish();
        let mut v = Vec::<u8>::new();
        let mut cursor = Cursor::new(&mut v);
        disassemble_chunk_for_testing(&result.script.chunk, &mut cursor);
        Ok(String::from_utf8(v)
            .map_err(|e| format!("Failed to convert the disassembled code to String {:?}", e))?)
    }

    #[test]
    fn test_while_statement() -> Result<(), String> {
        assert_eq!(
            compile("while (true) { print false; }")?,
            "0000    1 OP_TRUE\n\
             0001    | OP_JUMP_IF_FALSE 0001 -> 10\n\
             0004    | OP_POP\n\
             0005    | OP_FALSE\n\
             0006    | OP_PRINT\n\
             0007    | OP_LOOP 0007 -> 0\n\
             0010    | OP_POP\n\
             0011    | OP_RETURN\n"
        );
        Ok(())
    }

    #[test]
    fn test_logical_and() -> Result<(), String> {
        assert_eq!(
            compile("1 and 2;")?,
            "0000    1 OP_CONSTANT 0000 '1'\n\
             0002    | OP_JUMP_IF_FALSE 0002 -> 8\n\
             0005    | OP_POP\n\
             0006    | OP_CONSTANT 0001 '2'\n\
             0008    | OP_POP\n\
             0009    | OP_RETURN\n"
        );
        Ok(())
    }

    #[test]
    fn test_logical_or() -> Result<(), String> {
        assert_eq!(
            compile("1 or 2;")?,
            "0000    1 OP_CONSTANT 0000 '1'\n\
             0002    | OP_JUMP_IF_FALSE 0002 -> 8\n\
             0005    | OP_JUMP 0005 -> 11\n\
             0008    | OP_POP\n\
             0009    | OP_CONSTANT 0001 '2'\n\
             0011    | OP_POP\n\
             0012    | OP_RETURN\n"
        );
        Ok(())
    }
}
