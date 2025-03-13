use std::{borrow::Borrow, collections::HashSet, fmt::Display, rc::Rc};

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

pub(crate) fn intern_string(strings: &mut HashSet<InternedString>, s: &str) -> InternedString {
    match strings.get(s) {
        Some(v) => v.clone(),
        None => {
            let v = InternedString(Rc::new(s.to_string()));
            strings.insert(v.clone());
            v
        }
    }
}
