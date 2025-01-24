pub mod command;
mod environment;
mod expr;
mod stmt;

use std::{cell::RefCell, rc::Rc};

use crate::stmt::{Action, Error};
use environment::Environment;
use expr::Value;
use red_lox_ast::{
    scanner::Token,
    stmt::{Evaluator, Stmt},
};

pub struct Interpreter<'a, 'b> {
    global: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    out: &'a mut dyn std::io::Write,
    err: &'b mut dyn std::io::Write,
}

pub struct EnvGuard<'a, 'b, 'c> {
    interpreter: &'a mut Interpreter<'b, 'c>,
}

impl<'a, 'b, 'c> Drop for EnvGuard<'a, 'b, 'c> {
    fn drop(&mut self) {
        let enclosing = self
            .interpreter
            .environment
            .borrow()
            .get_enclosing()
            .unwrap();
        self.interpreter.environment = enclosing
    }
}

impl<'a, 'b> Interpreter<'a, 'b> {
    pub fn new(out: &'a mut dyn std::io::Write, err: &'b mut dyn std::io::Write) -> Self {
        let mut global = Environment::default();
        Self {
            global: Rc::new(RefCell::new(global)),
            environment: Rc::new(RefCell::new(Environment::default())),
            out,
            err,
        }
    }

    pub fn interpret(&mut self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts.iter() {
            match self.execute(stmt) {
                Ok(_) => (),
                Err(e) => {
                    writeln!(self.err, "{}", e).expect("failed to write to Interpreter's out");
                }
            }
        }
    }

    pub fn enter(&mut self) -> EnvGuard<'_, 'a, 'b> {
        self.environment.borrow_mut().enter();
        EnvGuard { interpreter: self }
    }

    fn handle_side_effect(&mut self, action: Action) {
        match action {
            Action::Print(v) => {
                writeln!(self.out, "{}", v).expect("failed to write to Interpreter's out");
            }
            Action::Eval(_) | Action::Break => (),
            Action::Define(t, v) => {
                let name = match t {
                    Token::Identifier(n) => n,
                    _ => unreachable!(),
                };
                self.environment.borrow_mut().define(name, v);
            }
        }
    }

    // Returns true when the resulting Action was Action::Break.
    fn execute(&mut self, stmt: &Stmt) -> Result<bool, Error> {
        let action = self.evaluate_stmt(stmt)?;
        let is_break = match action {
            Action::Break => true,
            _ => false,
        };
        self.handle_side_effect(action);
        Ok(is_break)
    }

    fn execute_block(&mut self, stmts: &Vec<Box<Stmt>>) -> Result<Action, Error> {
        let guard = self.enter();
        let mut action = Action::Eval(Value::Nil);
        for stmt in stmts {
            if guard.interpreter.execute(&stmt)? {
                action = Action::Break;
                break;
            }
        }
        Ok(action)
    }
}

#[cfg(test)]
mod tests {}
