use std::collections::HashMap;

use red_lox_ast::scanner::Location;

use crate::op_code::OpCode;

/// A registry struct to keep track of the code locations.
/// The use cases are as below:
/// a. patching the size of forward jumps through the following steps:
///    1. Add a new entry to `backpatch_locations` when the compiler reaches the location to jump from.
///    2. When the compiler reaches the destination of the jump, get the locations to patch from
///       `backpatch_locations` and patch them.
/// b. patching the size of backward jumps through the following steps:
///    1. Add a new entry to `labels` at the destination of jumps.
///    2. When the compiler reaches the location to jump from, add an etnry to `backpatch_locations`.
///    3. When the compiler reaches the last location to jump from (and `backpatch_locations`
///       has been updated), get the locations to patch from `backpatch_locations` and patch them.
/// The value of `backpatch_locations` is a vector so as to support patching multiple locations to jump from
/// (e.g., the break statement and the continue statement).
pub(crate) struct CodeLocationRegistry {
    labels: HashMap<LabelKey, usize>,
    backpatch_locations: HashMap<BackPatchLocationKey, Vec<(OpCode, usize)>>,
}

impl CodeLocationRegistry {
    pub(crate) fn new() -> Self {
        Self {
            labels: HashMap::new(),
            backpatch_locations: HashMap::new(),
        }
    }

    pub(crate) fn add_label(&mut self, key: LabelKey, value: usize) -> Option<usize> {
        self.labels.insert(key, value)
    }

    pub(crate) fn remove_label(&mut self, key: &LabelKey) -> Option<usize> {
        self.labels.remove(key)
    }

    pub(crate) fn add_backpatch_location(
        &mut self,
        key: BackPatchLocationKey,
        op_code: OpCode,
        code_location: usize,
    ) {
        self.backpatch_locations
            .entry(key)
            .or_default()
            .push((op_code, code_location))
    }

    pub(crate) fn remove_backpatch_location(
        &mut self,
        key: &BackPatchLocationKey,
    ) -> Vec<(OpCode, usize)> {
        self.backpatch_locations.remove(key).unwrap_or_default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Usage {
    LoopCondition,
    NextLogicExpression,
    EndOfStatement,
    EndOfExpression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct LabelKey {
    usage: Usage,
    // The location of the token that is related to the usage
    // e.g., 'while' and 'for' for LoopCondition, 'else' for EndOfStatement
    location: Location,
}

impl LabelKey {
    pub(crate) fn new(usage: Usage, location: Location) -> Self {
        Self { usage, location }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct BackPatchLocationKey {
    usage: Usage,
    location: Location,
}

impl BackPatchLocationKey {
    pub(crate) fn new(usage: Usage, location: Location) -> Self {
        Self { usage, location }
    }
}
