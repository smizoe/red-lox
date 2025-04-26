use red_lox_ast::scanner::Location;

use crate::{
    common::code_location_registry::LabelType, common::op_code::OpCode, common::InternedString,
};

use super::{value::Value, variable_location::UpValueLocation};

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum WriteAction {
    WriteNoArgOpCode {
        op_code: OpCode,
        location: Location,
    },
    WriteOpCodeWithOffset {
        op_code: OpCode,
        offset: u8,
        location: Location,
    },
    WriteOpCodeWithIdentifier {
        op_code: OpCode,
        identifier: InternedString,
        location: Location,
    },
    WriteOpCodeWithValue {
        op_code: OpCode,
        value: Value,
        location: Location,
    },
    WriteJumpOpCode {
        op_code: OpCode,
        label_type: LabelType,
        location: Location,
    },
    WriteOpCodeCall {
        arg_count: u8,
        location: Location,
    },
    // Applies the back-patching of the jump destination.
    BackPatchJumpLocation {
        label_type: LabelType,
        location: Location,
    },
    // Adds a label to be used as a jump target.
    AddLabel {
        label_type: LabelType,
        location: Location,
    },
    FunctionDeclaration {
        name: InternedString,
        arity: usize,
        location: Location,
    },
    FunctionDeclarationEnd {
        is_global: bool,
        upvalues: Vec<UpValueLocation>,
        location: Location,
    },
    ClassDeclaration {
        name: InternedString,
        is_global: bool,
        location: Location,
    },
}

impl WriteAction {
    pub fn get_location(&self) -> &Location {
        match self {
            WriteAction::BackPatchJumpLocation { location, .. } => location,
            WriteAction::AddLabel { location, .. } => location,
            WriteAction::FunctionDeclaration { location, .. } => location,
            WriteAction::FunctionDeclarationEnd { location, .. } => location,
            WriteAction::ClassDeclaration { location, .. } => location,
            WriteAction::WriteNoArgOpCode { location, .. } => location,
            WriteAction::WriteOpCodeWithOffset { location, .. } => location,
            WriteAction::WriteOpCodeWithIdentifier { location, .. } => location,
            WriteAction::WriteOpCodeWithValue { location, .. } => location,
            WriteAction::WriteJumpOpCode { location, .. } => location,
            WriteAction::WriteOpCodeCall { location, .. } => location,
        }
    }
}
