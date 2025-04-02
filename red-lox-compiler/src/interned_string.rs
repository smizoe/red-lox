use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt::Display,
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

pub(crate) fn intern_string(
    strings: &mut HashMap<InternedString, Option<u8>>,
    s: &str,
) -> InternedString {
    match strings.get_key_value(s) {
        Some((k, _)) => k.clone(),
        None => {
            let v = InternedString(Arc::new(s.to_string()));
            strings.insert(v.clone(), None);
            v
        }
    }
}
