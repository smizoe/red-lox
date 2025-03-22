use red_lox_ast::scanner::Location;

use crate::{interned_string::InternedString, op_code::OpCode};

#[derive(Debug, Clone)]
pub enum Arguments {
    None,
    String(InternedString),
    Number(f64),
    Offset(u8),
}

impl std::fmt::Display for Arguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Arguments {
    pub fn to_interned_string(&self) -> Option<InternedString> {
        use Arguments::*;
        match self {
            String(s) => Some(s.clone()),
            _ => Option::None,
        }
    }

    pub fn to_offset(&self) -> Option<u8> {
        match self {
            Arguments::Offset(v) => Some(*v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InstructionWithLocation {
    pub op_code: OpCode,
    pub args: Arguments,
    pub location: Location,
}
