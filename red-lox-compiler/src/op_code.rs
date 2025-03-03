use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Constant = 1,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Return,
    Comma,
}

impl OpCode {
    // Returns the number of bytes (u8) that a single op uses.
    pub fn len(&self) -> usize {
        use OpCode::*;
        match self {
            Return | Negate | Add | Subtract | Multiply | Divide | Not | Nil | True | False
            | Equal | Greater | Less | Comma => 1,
            Constant => 2,
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Constant => write!(f, "OP_CONSTANT"),
            OpCode::Nil => write!(f, "OP_NIL"),
            OpCode::True => write!(f, "OP_TRUE"),
            OpCode::False => write!(f, "OP_FALSE"),
            OpCode::Equal => write!(f, "OP_EQUAL"),
            OpCode::Greater => write!(f, "OP_GREATER"),
            OpCode::Less => write!(f, "OP_LESS"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
            OpCode::Not => write!(f, "OP_NOT"),
            OpCode::Comma => write!(f, "OP_COMMA"),
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
            value if value == Nil as u8 => Ok(Nil),
            value if value == True as u8 => Ok(True),
            value if value == False as u8 => Ok(False),
            value if value == Negate as u8 => Ok(Negate),
            value if value == Add as u8 => Ok(Add),
            value if value == Subtract as u8 => Ok(Subtract),
            value if value == Multiply as u8 => Ok(Multiply),
            value if value == Divide as u8 => Ok(Divide),
            value if value == Not as u8 => Ok(Not),
            value if value == Equal as u8 => Ok(Equal),
            value if value == Greater as u8 => Ok(Greater),
            value if value == Less as u8 => Ok(Less),
            value if value == Comma as u8 => Ok(Comma),
            _ => Err(ConversionError { from: value }),
        }
    }
}

impl From<OpCode> for u8 {
    fn from(value: OpCode) -> Self {
        value as u8
    }
}
