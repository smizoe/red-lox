use crate::common::InternedString;

#[derive(Debug)]
pub(crate) struct Local {
    name: InternedString,
    pub(crate) depth: i32,
    pub(crate) is_captured: bool,
}

impl Local {
    pub fn new(name: InternedString, depth: i32) -> Self {
        Self {
            name,
            depth,
            is_captured: false,
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UpValueLocation {
    /// The stored value is an index into the parent's locals.
    LocalOfParent(u8),
    /// The stored value is an index into the parent's upvalues.
    UpValueOfParent(u8),
}
