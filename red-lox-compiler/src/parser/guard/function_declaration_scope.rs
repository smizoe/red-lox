use red_lox_ast::scanner::Location;

use crate::common::variable_location::Local;
use crate::common::variable_location::UpValueLocation;
use crate::common::write_action::FunctionType;
use crate::{
    common::{write_action::WriteAction, InternedString},
    parser::{guard::BreakableStatement, Parser, Result},
};
use std::ops::{Deref, DerefMut};

pub(in crate::parser) struct FunctionDeclarationScope<'a, 'b> {
    parser: &'b mut Parser<'a>,
}

impl<'a, 'b> FunctionDeclarationScope<'a, 'b> {
    pub fn new(
        parser: &'b mut Parser<'a>,
        fun_name: InternedString,
        arity: usize,
        function_type: FunctionType,
    ) -> Self {
        parser.append_write(WriteAction::FunctionDeclaration {
            name: fun_name.clone(),
            arity,
            location: parser.prev.location.clone(),
        });
        let mut env = Box::new(FunctionEnv::new(fun_name, function_type));
        std::mem::swap(&mut parser.env, &mut env);
        parser.env.prev_env.replace(env);

        Self { parser }
    }
}

impl<'a, 'b> Drop for FunctionDeclarationScope<'a, 'b> {
    fn drop(&mut self) {
        let location = self.prev.location.clone();
        let is_global = self.scope_depth == 0
            && ![FunctionType::Method, FunctionType::Initializer].contains(&self.env.function_type);
        let mut env = self.parser.env.prev_env.take().unwrap();
        std::mem::swap(&mut self.parser.env, &mut env);

        self.append_write(WriteAction::FunctionDeclarationEnd {
            is_global,
            upvalues: env.upvalues,
            location,
            function_type: env.function_type,
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

pub(in crate::parser) struct FunctionEnv {
    pub locals: Vec<Local>,
    pub upvalues: Vec<UpValueLocation>,
    pub breakable_stmts: Vec<BreakableStatement>,
    pub function_type: FunctionType,
    prev_env: Option<Box<FunctionEnv>>,
}

impl FunctionEnv {
    pub fn new(fun_name: InternedString, function_type: FunctionType) -> Self {
        let mut locals = Vec::new();
        if [FunctionType::Method, FunctionType::Initializer].contains(&function_type) {
            locals.push(Local::new(InternedString::get_this(), 0));
        } else {
            locals.push(Local::new(fun_name, 0));
        }
        Self {
            locals,
            upvalues: Vec::new(),
            breakable_stmts: Vec::new(),
            function_type,
            prev_env: None,
        }
    }

    /// Resolves an upvalue. An upvalue is a value on the stack that is referred in a closure.
    /// Returns the following:
    /// - Ok(Some(v)) when a new/existing upvalue is obtained.
    /// - Ok(None) when the value turns out to be stored in a global variable.
    /// - Err(e) when there are too many upvalues stored.
    pub fn resolve_upvalue(&mut self, name: &str, location: &Location) -> Result<Option<u8>> {
        if self.prev_env.is_none() {
            return Ok(None);
        }

        let e = self.prev_env.as_mut().unwrap();
        if let Some(v) = e.resolve_local(name, location)? {
            e.mark_captured(v.into());
            return Ok(Some(
                self.add_upvalue(UpValueLocation::LocalOfParent(v), location)?,
            ));
        }
        match e.resolve_upvalue(name, location)? {
            None => Ok(None),
            Some(v) => Ok(Some(
                self.add_upvalue(UpValueLocation::UpValueOfParent(v), location)?,
            )),
        }
    }

    /// Resolves a local variable named `name` in the current environment.
    /// Returns the following:
    /// - Ok(Some(v)) when there is such a variable.
    /// - Ok(None) when there is none found.
    /// - Err(e) when the variable is found but its initialiation has not completed.
    pub fn resolve_local(&self, name: &str, location: &Location) -> Result<Option<u8>> {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.name() == name {
                if local.depth == -1 {
                    return Err(crate::parser::Error::UninititalizedVariableAccessError {
                        location: location.clone(),
                        name: name.to_string(),
                    });
                }
                return Ok(Some(u8::try_from(i).unwrap()));
            }
        }
        Ok(None)
    }

    fn add_upvalue(&mut self, upvalue: UpValueLocation, location: &Location) -> Result<u8> {
        if let Some((i, _)) = self
            .upvalues
            .iter()
            .enumerate()
            .find(|ent| ent.1 == &upvalue)
        {
            return u8::try_from(i).map_err(|_| crate::parser::Error::TooManyUpValuesError {
                location: location.clone(),
            });
        }

        if self.upvalues.len() == u8::MAX.into() {
            return Err(crate::parser::Error::TooManyUpValuesError {
                location: location.clone(),
            });
        }

        let index = self.upvalues.len();
        self.upvalues.push(upvalue);
        Ok(u8::try_from(index).unwrap())
    }

    fn mark_captured(&mut self, index: usize) {
        self.locals[index].is_captured = true;
    }
}
