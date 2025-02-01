pub mod command;
mod environment;
mod expr;
mod globals;
mod resolver;
mod stmt;

use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::stmt::{Action, Error};
use environment::Environment;
use expr::Value;
use globals::register_globals;
use red_lox_ast::{
    scanner::{Location, Token, TokenWithLocation},
    stmt::{Evaluator, Stmt},
};

pub struct Interpreter<'a, 'b> {
    globals: Rc<Environment>,
    environment: Rc<Environment>,
    out: &'a mut dyn std::io::Write,
    err: &'b mut dyn std::io::Write,
    fn_call_nest: usize,
    locals: HashMap<Location, usize>,
}

pub struct EnvGuard<'a, 'b, 'c> {
    interpreter: &'a mut Interpreter<'b, 'c>,
}

impl<'a, 'b, 'c> EnvGuard<'a, 'b, 'c> {
    fn new(interpreter: &'a mut Interpreter<'b, 'c>) -> Self {
        interpreter.environment.enter();
        Self { interpreter }
    }
}

impl<'a, 'b, 'c> Deref for EnvGuard<'a, 'b, 'c> {
    type Target = Interpreter<'b, 'c>;

    fn deref(&self) -> &Self::Target {
        self.interpreter
    }
}

impl<'a, 'b, 'c> DerefMut for EnvGuard<'a, 'b, 'c> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.interpreter
    }
}

impl<'a, 'b, 'c> Drop for EnvGuard<'a, 'b, 'c> {
    fn drop(&mut self) {
        self.interpreter.environment.exit();
    }
}

pub struct FnCallGuard<'a, 'b, 'c> {
    interpreter: &'a mut Interpreter<'b, 'c>,
    original_env: Rc<Environment>,
}

impl<'a, 'b, 'c> FnCallGuard<'a, 'b, 'c> {
    fn new(interpreter: &'a mut Interpreter<'b, 'c>, mut closure: Rc<Environment>) -> Self {
        interpreter.fn_call_nest += 1;
        std::mem::swap(&mut interpreter.environment, &mut closure);
        Self {
            interpreter: interpreter,
            original_env: closure,
        }
    }
}

impl<'a, 'b, 'c> Deref for FnCallGuard<'a, 'b, 'c> {
    type Target = Interpreter<'b, 'c>;

    fn deref(&self) -> &Self::Target {
        self.interpreter
    }
}

impl<'a, 'b, 'c> DerefMut for FnCallGuard<'a, 'b, 'c> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.interpreter
    }
}

impl<'a, 'b, 'c> Drop for FnCallGuard<'a, 'b, 'c> {
    fn drop(&mut self) {
        self.interpreter.fn_call_nest -= 1;
        std::mem::swap(&mut self.interpreter.environment, &mut self.original_env);
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct ResolverKey {
    name: String,
    location: Location,
}

impl<'a, 'b> Interpreter<'a, 'b> {
    pub fn new(out: &'a mut dyn std::io::Write, err: &'b mut dyn std::io::Write) -> Self {
        let mut globals = Environment::default();
        register_globals(&mut globals);
        let globals = Rc::new(globals);
        Self {
            globals: globals.clone(),
            environment: globals,
            out,
            err,
            fn_call_nest: 0,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Box<Stmt>>) {
        for stmt in stmts {
            match self.execute(&stmt) {
                Ok(_) => (),
                Err(e) => {
                    writeln!(self.err, "{}", e).expect("failed to write to Interpreter's out");
                }
            }
        }
    }

    pub fn enter(&mut self) -> EnvGuard<'_, 'a, 'b> {
        EnvGuard::new(self)
    }

    pub fn start_calling_fn(&mut self, closure: Rc<Environment>) -> FnCallGuard<'_, 'a, 'b> {
        FnCallGuard::new(self, closure)
    }

    pub fn resolve(&mut self, location: Location, depth: usize) {
        self.locals.insert(location, depth);
    }

    fn lookup_variable(&self, token: &TokenWithLocation) -> Result<Value, expr::Error> {
        match self.locals.get(&token.location) {
            None => self.globals.get(token),
            Some(&distance) => self.environment.get_at(distance, token),
        }
    }

    fn handle_side_effect(&mut self, action: Action) {
        match action {
            Action::Print(v) => {
                writeln!(self.out, "{}", v).expect("failed to write to Interpreter's out");
            }
            Action::Eval(_) | Action::Break | Action::Return(_) => (),
            Action::Define(t, v) => {
                let name = match t {
                    Token::Identifier(n) => n,
                    _ => unreachable!(),
                };
                self.environment.define(name, v);
            }
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<Action, Error> {
        let action = self.evaluate_stmt(stmt)?;
        self.handle_side_effect(action.clone());
        Ok(action)
    }

    fn execute_block(&mut self, stmts: &Vec<Box<Stmt>>) -> Result<Action, Error> {
        let mut action = Action::Eval(Value::Nil);
        for stmt in stmts {
            match self.execute(&stmt)? {
                a @ (Action::Break | Action::Return(_)) => {
                    action = a;
                    break;
                }
                _ => (),
            }
        }
        Ok(action)
    }
}
