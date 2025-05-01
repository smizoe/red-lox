use red_lox_ast::scanner::{Location, TokenizationError};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("An error occurred while parsing the code: {}", .0)]
    TokenizationError(TokenizationError),
    #[error("{}", .0)]
    UnexpectedTokenError(String),
    #[error("{location} The lhs of assignment is invalid.")]
    InvalidAssignmentError { location: Location },
    #[error("{location} Too many local variables in the current scope.")]
    TooManyLocalVariablesError { location: Location },
    #[error("{location} Too many upvalues in the current scope.")]
    TooManyUpValuesError { location: Location },
    #[error("{location} Variable '{name}' is already defined in the current scope.")]
    DuplicateVariableDeclarationError { location: Location, name: String },
    #[error("{location} Can't read local variable '{name}' in its own initializer.")]
    UninititalizedVariableAccessError { location: Location, name: String },
    #[error("{location} Can't use a break statement outside a loop or a switch statement.")]
    MisplacedBreakStatementError { location: Location },
    #[error(
        "{location} Can't use a continue statement outside a loop or inside a switch statement."
    )]
    MisplacedContinueStatementError { location: Location },
    #[error("{location} A function can't have more than 255 arguments.")]
    TooManyFunctionArgumentsError { location: Location },
    #[error("{location} Cannot return from the top-level code.")]
    ReturnFromTopLevelError { location: Location },
    #[error("{location} Cannot use 'this' outside of a class.")]
    ThisReferenceOutsideClassError { location: Location },
    #[error("{location} Cannot return a value from an initializer method.")]
    InvalidReturnValueFromInitializerError { location: Location },
}

pub type Result<T> = std::result::Result<T, Error>;
