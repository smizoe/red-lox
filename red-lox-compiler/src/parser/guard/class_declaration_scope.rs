use std::ops::{Deref, DerefMut};

use crate::parser::Parser;

pub(in crate::parser) struct ClassDeclarationScope<'a, 'b> {
    parser: &'b mut Parser<'a>,
}

impl<'a, 'b> ClassDeclarationScope<'a, 'b> {
    pub fn new(parser: &'b mut Parser<'a>) -> Self {
        parser.class_nest_depth += 1;
        Self { parser }
    }
}

impl<'a, 'b> Drop for ClassDeclarationScope<'a, 'b> {
    fn drop(&mut self) {
        self.parser.class_nest_depth -= 1;
    }
}

impl<'a, 'b> Deref for ClassDeclarationScope<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> DerefMut for ClassDeclarationScope<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}
