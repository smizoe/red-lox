use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    Constant = 1,
    Nil,
    True,
    False,
    Pop,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Return,
    Comma,
}

impl OpCode {
    // Returns the number of bytes (u8) that a single op uses.
    pub fn len(&self) -> usize {
        use OpCode::*;
        match self {
            Constant | DefineGlobal => 2,
            _ => 1,
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
            OpCode::Pop => write!(f, "OP_POP"),
            OpCode::GetGlobal => write!(f, "OP_GET_GLOBAL"),
            OpCode::DefineGlobal => write!(f, "OP_DEFINE_GLOBAL"),
            OpCode::SetGlobal => write!(f, "OP_SET_GLOBAL"),
            OpCode::Equal => write!(f, "OP_EQUAL"),
            OpCode::Greater => write!(f, "OP_GREATER"),
            OpCode::Less => write!(f, "OP_LESS"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Print => write!(f, "OP_PRINT"),
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
            value if value == Pop as u8 => Ok(Pop),
            value if value == GetGlobal as u8 => Ok(GetGlobal),
            value if value == DefineGlobal as u8 => Ok(DefineGlobal),
            value if value == SetGlobal as u8 => Ok(SetGlobal),
            value if value == Equal as u8 => Ok(Equal),
            value if value == Greater as u8 => Ok(Greater),
            value if value == Less as u8 => Ok(Less),
            value if value == Comma as u8 => Ok(Comma),
            value if value == Print as u8 => Ok(Print),
            _ => Err(ConversionError { from: value }),
        }
    }
}

impl From<OpCode> for u8 {
    fn from(value: OpCode) -> Self {
        value as u8
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use OpCode::{self, *};

    #[rstest]
    #[case(Constant)]
    #[case(Nil)]
    #[case(True)]
    #[case(False)]
    #[case(Pop)]
    #[case(GetGlobal)]
    #[case(DefineGlobal)]
    #[case(SetGlobal)]
    #[case(Equal)]
    #[case(Greater)]
    #[case(Less)]
    #[case(Add)]
    #[case(Subtract)]
    #[case(Multiply)]
    #[case(Divide)]
    #[case(Not)]
    #[case(Negate)]
    #[case(Print)]
    #[case(Return)]
    #[case(Comma)]
    fn all_op_code_covered_by_try_from(#[case] op: OpCode) {
        let from_u8: Result<OpCode, ConversionError> = OpCode::try_from(op as u8);
        assert!(from_u8.is_ok());
        assert_eq!(from_u8.unwrap(), op);
    }
}
