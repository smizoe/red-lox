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
    #[error("An upvalue is being closed twice.")]
    DoublyClosedUpvalueError,
    #[error("A closed upvalue is requested to provide its index into the stack.")]
    ClosedUpValueLocationRequestError,
}

pub type Result<T> = std::result::Result<T, Error>;
