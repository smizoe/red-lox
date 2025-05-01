use std::{
    borrow::Borrow,
    collections::HashSet,
    fmt::Display,
    str::FromStr,
    sync::{Arc, LazyLock},
};

// A struct representing a string that is allocated once and can be used
// multiple times with a smaller overhead. Arc is used to create an empty InternedString.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternedString(Arc<String>);

impl InternedString {
    pub(crate) fn get_empty_string() -> Self {
        static EMPTY: LazyLock<InternedString> =
            LazyLock::new(|| InternedString(Arc::new(String::new())));
        EMPTY.clone()
    }

    pub(crate) fn get_this() -> Self {
        static THIS: LazyLock<InternedString> =
            LazyLock::new(|| InternedString(Arc::new("this".to_string())));
        THIS.clone()
    }

    pub(crate) fn from_str(s: &str) -> InternedString {
        InternedString(Arc::new(s.to_string()))
    }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for InternedString {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct InternedStringRegistry {
    registry: HashSet<InternedString>,
}

impl InternedStringRegistry {
    pub(crate) fn new() -> Self {
        Self {
            registry: Default::default(),
        }
    }

    pub fn intern_string(&mut self, s: &str) -> InternedString {
        self.registry.get(s).cloned().unwrap_or_else(|| {
            let interned = InternedString::from_str(s);
            self.registry.insert(interned.clone());
            interned
        })
    }
}
