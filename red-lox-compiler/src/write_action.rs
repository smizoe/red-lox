use red_lox_ast::scanner::Location;

use crate::{interned_string::InternedString, op_code::OpCode};

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum WriteAction {
    // Writes the op_code into the chunk.
    OpCodeWrite {
        op_code: OpCode,
        args: Arguments,
        location: Location,
    },
    // Applies the back-patching of the jump destination.
    BackPatchJumpLocation {
        op_code: OpCode,
        location: Location,
    },
    // Adds a label to be used as a jump target.
    AddLabel {
        op_code: OpCode,
        location: Location,
    },
}

impl WriteAction {
    pub fn get_location(&self) -> &Location {
        match self {
            WriteAction::OpCodeWrite { location, .. } => location,
            WriteAction::BackPatchJumpLocation { location, .. } => location,
            WriteAction::AddLabel { location, .. } => location,
        }
    }
}
