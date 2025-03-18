use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternedString(Rc<String>);

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        &self.0
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
            let v = InternedString(Rc::new(s.to_string()));
            strings.insert(v.clone(), None);
            v
        }
    }
}
