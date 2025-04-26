use red_lox_ast::scanner::{Location, Scanner};

use crate::{
    common::{
        chunk::{debug::disassemble_chunk, Chunk},
        code_location_registry::{BackPatchLocationKey, CodeLocationRegistry, LabelKey, LabelType},
        function::{Closure, LoxFunction},
        op_code::OpCode,
        value::Value,
        variable_location::UpValueLocation,
        write_action::WriteAction,
        InternedString, InternedStringRegistry,
    },
    parser::Parser,
};

pub struct CompilationResult {
    pub script: Closure,
    pub interned_string_registry: InternedStringRegistry,
}

pub struct Compiler<'a> {
    function: Vec<LoxFunction>,
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

impl<'a> Compiler<'a> {
    /// Creates a new compiler that compiles the given `text``.
    pub fn new(text: &'a str) -> Self {
        Self {
            function: vec![LoxFunction::new(InternedString::get_empty_string(), 0)],
            parser: Parser::new(Scanner::new(text)),
            code_location_registry: CodeLocationRegistry::new(),
        }
    }

    /// Compiles the text passed to `Compiler::new` method.
    /// Returns an Ok value when the compilation was successful.
    /// An error value is returned when there is any error encountered during the compilation.
    pub fn compile(&mut self) -> Result<(), Error> {
        while let Some(write_action) = self.parser.next_write() {
            let location = write_action.get_location().clone();
            self.write(write_action)
                .map_err(move |_| Error::TooManyConstantsError { location })?;
        }
        if !self.parser.errors.is_empty() {
            return Err(Error::CompilationError(std::mem::take(
                &mut self.parser.errors,
            )));
        }
        let cur_loc = self.parser.current.location.clone();
        self.write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Nil,
            location: cur_loc.clone(),
        })?;
        self.write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Return,
            location: cur_loc,
        })?;
        Ok(())
    }

    pub fn finish(mut self) -> CompilationResult {
        CompilationResult {
            script: Closure::new(self.function.pop().unwrap(), 0),
            interned_string_registry: self.parser.interned_string_registry,
        }
    }

    /// Handles a single `WriteAction`.
    /// See each function called internally for what occurs when each variant of the enum arrives.
    fn write(&mut self, write_action: WriteAction) -> Result<(), Error> {
        let offset = self.current_chunk().code_len();
        let location = write_action.get_location().clone();
        match write_action {
            WriteAction::WriteNoArgOpCode { op_code, .. } => {
                self.current_chunk_mut().add_code(op_code.into());
            }
            WriteAction::BackPatchJumpLocation {
                label_type: usage,
                location,
            } => self.back_patch_jump_location(usage, location.clone())?,
            WriteAction::AddLabel {
                label_type: usage,
                location,
            } => {
                self.add_label(usage, location.clone());
            }
            WriteAction::FunctionDeclaration { name, arity, .. } => {
                self.function.push(LoxFunction::new(name.clone(), arity));
            }
            WriteAction::FunctionDeclarationEnd {
                is_global,
                upvalues,
                location,
            } => {
                // Ensure that the function `return`s. If the function already has a
                // return statement, the op codes added here do nothing.
                self.current_chunk_mut().add_code(OpCode::Nil.into());
                self.current_chunk_mut().add_code(OpCode::Return.into());

                let defined = self.function.pop().unwrap();
                let name = defined.name.clone();

                if cfg!(debug_assertions) {
                    disassemble_chunk(defined.chunk(), &format!("function_defined: {}", name));
                }
                let closure = Closure::new(defined, upvalues.len());

                let index = self.add_constant(Value::Closure(Box::new(closure)), &location)?;
                let chunk = self.current_chunk_mut();
                chunk.add_code(OpCode::Closure.into());
                chunk.add_code(index);
                for upvalue in upvalues {
                    match upvalue {
                        UpValueLocation::LocalOfParent(v) => {
                            // Represents that the upvalue is a local of the parent env.
                            chunk.add_code(1);
                            chunk.add_code(v);
                        }
                        UpValueLocation::UpValueOfParent(v) => {
                            chunk.add_code(0);
                            chunk.add_code(v);
                        }
                    }
                }
                if is_global {
                    self.write(WriteAction::WriteOpCodeWithValue {
                        op_code: OpCode::DefineGlobal,
                        value: crate::common::value::Value::String(name),
                        location,
                    })?;
                }
            }
            WriteAction::ClassDeclaration {
                name,
                is_global,
                location,
            } => {
                let v = self.add_constant(Value::String(name.clone()), &location)?;
                self.current_chunk_mut().add_code(OpCode::Class.into());
                self.current_chunk_mut().add_code(v);
                if is_global {
                    self.write(WriteAction::WriteOpCodeWithValue {
                        op_code: OpCode::DefineGlobal,
                        value: crate::common::value::Value::String(name),
                        location,
                    })?;
                }
            }
            WriteAction::WriteOpCodeWithOffset {
                op_code, offset, ..
            } => {
                self.current_chunk_mut().add_code(op_code.into());
                self.current_chunk_mut().add_code(offset);
            }
            WriteAction::WriteOpCodeWithIdentifier {
                op_code,
                identifier,
                location,
            } => {
                let v = self.add_constant(Value::String(identifier), &location)?;
                self.current_chunk_mut().add_code(op_code.into());
                self.current_chunk_mut().add_code(v);
            }
            WriteAction::WriteOpCodeWithValue {
                op_code,
                value,
                location,
            } => {
                let index = self.add_constant(value, &location)?;
                self.current_chunk_mut().add_code(op_code.into());
                self.current_chunk_mut().add_code(index);
            }
            WriteAction::WriteJumpOpCode {
                op_code,
                label_type,
                location,
            } => {
                self.current_chunk_mut().add_code(op_code.into());
                self.add_backpatch_location(op_code, label_type, location.clone());
                self.current_chunk_mut().add_code(u8::MAX);
                self.current_chunk_mut().add_code(u8::MAX);
            }
            WriteAction::WriteOpCodeCall { arg_count, .. } => {
                self.current_chunk_mut().add_code(OpCode::Call.into());
                self.current_chunk_mut().add_code(arg_count);
            }
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
        let index = self.get_next_index_for_constant(&location)?;
        self.current_chunk_mut().add_constant(value);
        Ok(index)
    }

    fn get_next_index_for_constant(&mut self, location: &Location) -> Result<u8, Error> {
        u8::try_from(self.current_chunk().get_num_constants()).map_err(move |_| {
            Error::TooManyConstantsError {
                location: location.clone(),
            }
        })
    }

    fn current_chunk(&self) -> &Chunk {
        &self.function.last().unwrap().chunk()
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        self.function.last_mut().unwrap().chunk_mut()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::common::chunk::debug::disassemble_chunk_for_testing;

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
        disassemble_chunk_for_testing(result.script.fun().chunk(), &mut cursor);
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
             0011    | OP_NIL\n\
             0012    | OP_RETURN\n"
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
             0009    | OP_NIL\n\
             0010    | OP_RETURN\n"
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
             0012    | OP_NIL\n\
             0013    | OP_RETURN\n"
        );
        Ok(())
    }

    #[test]
    fn test_switch_case() -> Result<(), String> {
        assert_eq!(
            compile(
                "var a;
                 switch (a) {
                 case 1:
                     a = 1;
                     break;
                 case 2:
                     a = 2;
                 default:
                     a = 3;
                 }"
            )?,
            "0000    1 OP_NIL\n\
             0001    | OP_DEFINE_GLOBAL 0000 'a'\n\
             0003    2 OP_GET_GLOBAL 0001 'a'\n\
             0005    | OP_CONSTANT 0002 '0'\n\
             0007    3 OP_CONSTANT 0003 '1'\n\
             0009    | OP_GET_LOCAL 0001\n\
             0011    | OP_EQUAL\n\
             0012    | OP_JUMP_IF_FALSE 0012 -> 26\n\
             0015    | OP_POP\n\
             0016    | OP_CONSTANT 0004 '1'\n\
             0018    | OP_GET_LOCAL 0002\n\
             0020    | OP_ADD\n\
             0021    | OP_SET_LOCAL 0002\n\
             0023    | OP_JUMP 0023 -> 29\n\
             0026    | OP_POP\n\
             0027    | OP_GET_LOCAL 0002\n\
             0029    | OP_CONSTANT 0005 '0'\n\
             0031    | OP_GREATER\n\
             0032    | OP_JUMP_IF_FALSE 0032 -> 49\n\
             0035    | OP_POP\n\
             0036    4 OP_CONSTANT 0006 '1'\n\
             0038    | OP_SET_GLOBAL 0007 'a'\n\
             0040    | OP_POP\n\
             0041    5 OP_POP\n\
             0042    | OP_POP\n\
             0043    2 OP_JUMP 0043 -> 98\n\
             0046    5 OP_JUMP 0046 -> 50\n\
             0049    | OP_POP\n\
             0050    6 OP_CONSTANT 0008 '2'\n\
             0052    | OP_GET_LOCAL 0001\n\
             0054    | OP_EQUAL\n\
             0055    | OP_JUMP_IF_FALSE 0055 -> 69\n\
             0058    | OP_POP\n\
             0059    | OP_CONSTANT 0009 '1'\n\
             0061    | OP_GET_LOCAL 0002\n\
             0063    | OP_ADD\n\
             0064    | OP_SET_LOCAL 0002\n\
             0066    | OP_JUMP 0066 -> 72\n\
             0069    | OP_POP\n\
             0070    | OP_GET_LOCAL 0002\n\
             0072    | OP_CONSTANT 0010 '0'\n\
             0074    | OP_GREATER\n\
             0075    | OP_JUMP_IF_FALSE 0075 -> 87\n\
             0078    | OP_POP\n\
             0079    7 OP_CONSTANT 0011 '2'\n\
             0081    | OP_SET_GLOBAL 0012 'a'\n\
             0083    | OP_POP\n\
             0084    | OP_JUMP 0084 -> 88\n\
             0087    | OP_POP\n\
             0088    9 OP_CONSTANT 0013 '3'\n\
             0090    | OP_SET_GLOBAL 0014 'a'\n\
             0092    | OP_POP\n\
             0093    2 OP_POP\n\
             0094    | OP_POP\n\
             0095    | OP_JUMP 0095 -> 98\n\
             0098   10 OP_NIL\n\
             0099    | OP_RETURN\n"
        );
        Ok(())
    }
}
