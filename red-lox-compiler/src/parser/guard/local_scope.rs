use red_lox_ast::scanner::Location;
use std::ops::{Deref, DerefMut};

use crate::{common::op_code::OpCode, common::write_action::Arguments, parser::Parser};

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
        let num_locals = self.locals().len();
        for local in self
            .locals_mut()
            .split_off(num_locals - upper)
            .into_iter()
            .rev()
        {
            if local.is_captured {
                self.append_write(crate::common::write_action::WriteAction::OpCodeWrite {
                    op_code: OpCode::CloseUpValue,
                    args: Arguments::None,
                    location: location.clone(),
                });
            } else {
                self.write_pop(location.clone());
            }
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
