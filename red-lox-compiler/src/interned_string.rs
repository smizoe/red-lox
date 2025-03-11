use std::{borrow::Borrow, collections::HashSet, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct InternedString(Rc<String>);

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        &self.0
    }
}

pub(crate) fn intern_string(strings: &mut HashSet<InternedString>, s: &str) -> Rc<String> {
    match strings.get(s) {
        Some(v) => v.0.clone(),
        None => {
            let v = Rc::new(s.to_string());
            strings.insert(InternedString(v.clone()));
            v
        }
    }
}
