use crate::common::{self, InternedString};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("compile error")]
    CompileError,
    #[error("[At line {line}] runtime conversion error: {error}")]
    OperationConversionError {
        line: usize,
        error: crate::common::op_code::ConversionError,
    },
    #[error("[At line {line}] runtime operation error: {msg}")]
    InvalidOperandError { line: usize, msg: String },
    #[error(
        "[At line {line}] runtime error: tried to refer to an uninitialized location in a stack"
    )]
    UninitializedStackReferenceError { line: usize },
    #[error(
        "[At line {line}] runtime error: variable '{var_name}' was accessed before it is defined."
    )]
    UndefinedVariableError { line: usize, var_name: String },
    #[error(
        "[At line {line}] a runtime error occurred while executing native function {name}: {error}"
    )]
    NativeFunctionCallError {
        line: usize,
        name: InternedString,
        error: crate::common::function::Error,
    },
    #[error("A runtime error occurred in handling an upvalue: {}", .0)]
    InvalidUpValueOperationError(common::function::Error),
    #[error("A runtime error occurred: an upvalue with index {index} cannot be found.")]
    OpenUpValueNotFoundError { index: usize },
    #[error("A runtime error occured: an undefined property {name} was accessed.")]
    UndefinedPropertyError { name: String },
    #[error(
        "A runtime error occurred: property access is attempted on a value of type {type_str}"
    )]
    InvalidPropertyAccessError { type_str: String },
}

pub type Result<T> = std::result::Result<T, Error>;
