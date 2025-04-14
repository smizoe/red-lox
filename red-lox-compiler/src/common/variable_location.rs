use crate::common::InternedString;

pub(crate) struct Local {
    name: InternedString,
    pub(crate) depth: i32,
}

impl Local {
    pub fn new(name: InternedString, depth: i32) -> Self {
        Self { name, depth }
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UpValue {
    /// The stored value is an index into the parent's locals.
    LocalOfParent(u8),
    /// The stored value is an index into the parent's upvalues.
    UpValueOfParent(u8),
}
