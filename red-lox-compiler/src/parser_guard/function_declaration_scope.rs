use crate::{
    interned_string::InternedString,
    parser::Parser,
    parser_guard::{BreakableStatement, Local},
    write_action::WriteAction,
};
use std::ops::{Deref, DerefMut};

pub(crate) struct FunctionDeclarationScope<'a, 'b> {
    parser: &'b mut Parser<'a>,
    prev_env: FunctionEnv,
}

impl<'a, 'b> FunctionDeclarationScope<'a, 'b> {
    pub fn new(parser: &'b mut Parser<'a>, fun_name: InternedString, arity: usize) -> Self {
        parser.append_write(WriteAction::FunctionDeclaration {
            name: fun_name,
            arity,
            location: parser.prev.location.clone(),
        });
        let mut env = FunctionEnv::new(FunctionType::Function);
        std::mem::swap(&mut parser.env, &mut env);

        Self {
            parser,
            prev_env: env,
        }
    }
}

impl<'a, 'b> Drop for FunctionDeclarationScope<'a, 'b> {
    fn drop(&mut self) {
        let location = self.prev.location.clone();
        let mut prev_env = std::mem::take(&mut self.prev_env);
        let is_global = self.scope_depth() == 0;
        std::mem::swap(&mut self.env, &mut prev_env);
        self.append_write(WriteAction::FunctionDeclarationEnd {
            is_global,
            location,
        });
    }
}

impl<'a, 'b> Deref for FunctionDeclarationScope<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> DerefMut for FunctionDeclarationScope<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

pub(crate) struct FunctionEnv {
    pub(crate) locals: Vec<Local>,
    pub(crate) breakable_stmts: Vec<BreakableStatement>,
    pub(super) scope_depth: i32,
    pub(crate) function_type: FunctionType,
}

impl FunctionEnv {
    pub fn new(function_type: FunctionType) -> Self {
        Self {
            locals: Vec::new(),
            breakable_stmts: Vec::new(),
            scope_depth: 0,
            function_type,
        }
    }

    pub(crate) fn scope_depth(&self) -> i32 {
        self.scope_depth
    }
}

impl Default for FunctionEnv {
    fn default() -> Self {
        Self {
            locals: Default::default(),
            breakable_stmts: Default::default(),
            scope_depth: Default::default(),
            function_type: FunctionType::Script,
        }
    }
}

pub(crate) enum FunctionType {
    Function,
    Script,
}
