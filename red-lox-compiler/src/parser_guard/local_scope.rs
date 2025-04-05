use red_lox_ast::scanner::Location;
use std::ops::{Deref, DerefMut};

use crate::{interned_string::InternedString, parser::Parser};

pub(crate) struct Local {
    pub(crate) name: InternedString,
    pub(crate) depth: i32,
}

pub(crate) struct LocalScope<'a, 'b> {
    parser: &'b mut Parser<'a>,
    left_brace_location: Location,
}

impl<'a, 'b> LocalScope<'a, 'b> {
    pub fn new(parser: &'b mut Parser<'a>) -> Self {
        parser.env.scope_depth += 1;
        let location = parser.prev.location.clone();
        Self {
            parser,
            left_brace_location: location,
        }
    }
}

impl<'a, 'b> Drop for LocalScope<'a, 'b> {
    fn drop(&mut self) {
        self.parser.env.scope_depth -= 1;
        let location = self.left_brace_location.clone();
        let upper = self.upper_bound_of_depth(self.scope_depth());
        for _ in 0..(self.locals().len() - upper) {
            self.write_pop(location.clone());
        }
        self.locals_mut().truncate(upper);
    }
}

impl<'a, 'b> Deref for LocalScope<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> DerefMut for LocalScope<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}
