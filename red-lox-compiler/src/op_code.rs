use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Constant = 1,
    Return,
}

impl OpCode {
    // Returns the number of bytes (u8) that a single op uses.
    pub fn len(&self) -> usize {
        use OpCode::*;
        match self {
            Return => 1,
            Constant => 2,
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Constant => write!(f, "OP_CONSTANT"),
            OpCode::Return => write!(f, "OP_RETURN"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Value {from} cannot be convert into OpCode.")]
pub struct ConversionError {
    from: u8,
}

impl TryFrom<u8> for OpCode {
    type Error = ConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use OpCode::*;
        match value {
            value if value == Return as u8 => Ok(Return),
            value if value == Constant as u8 => Ok(Constant),
            _ => Err(ConversionError { from: value }),
        }
    }
}

impl From<OpCode> for u8 {
    fn from(value: OpCode) -> Self {
        value as u8
    }
}
