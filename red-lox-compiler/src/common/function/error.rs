use crate::common::InternedString;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Native function {name} takes {arity} arguments but got {args_len} arguments")]
    FunctionArityMismatchError {
        name: InternedString,
        args_len: usize,
        arity: usize,
    },
    #[error("{msg}")]
    NativeFunctionCallError { msg: String },
    #[error("Runtime error: an upvalue is being closed twice.")]
    DoublyClosedUpvalueError,
}

pub type Result<T> = std::result::Result<T, Error>;
