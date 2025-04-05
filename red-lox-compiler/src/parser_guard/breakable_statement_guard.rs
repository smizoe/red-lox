use std::ops::{Deref, DerefMut};

use red_lox_ast::scanner::Location;

use crate::parser::Parser;

pub(crate) struct BreakableStatementGuard<'a, 'b> {
    parser: &'b mut Parser<'a>,
}

impl<'a, 'b> BreakableStatementGuard<'a, 'b> {
    pub fn new(parser: &'b mut Parser<'a>, stmt_type: StatementType, location: Location) -> Self {
        parser.env.breakable_stmts.push(BreakableStatement {
            statement_type: stmt_type,
            location,
            depth: parser.scope_depth(),
        });
        Self { parser }
    }
}

impl<'a, 'b> Drop for BreakableStatementGuard<'a, 'b> {
    fn drop(&mut self) {
        self.parser.env.breakable_stmts.pop();
    }
}

impl<'a, 'b> Deref for BreakableStatementGuard<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> DerefMut for BreakableStatementGuard<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum StatementType {
    While,
    For,
    Switch,
}

#[derive(Debug, Clone)]
pub(crate) struct BreakableStatement {
    pub(crate) statement_type: StatementType,
    /// Location used to tag the jump location
    pub(crate) location: Location,
    /// The depth of the scope at the statement. The local variables should be popped
    /// if their depth is greater than this value.
    pub(crate) depth: i32,
}
