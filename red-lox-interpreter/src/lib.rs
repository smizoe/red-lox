pub mod command;
mod environment;
mod expr;
mod stmt;

use crate::stmt::{Action, Error};
use environment::Environment;
use red_lox_ast::{
    scanner::Token,
    stmt::{Evaluator, Stmt},
};

pub struct Interpreter<'a, 'b> {
    environment: Box<Environment>,
    out: &'a mut dyn std::io::Write,
    err: &'b mut dyn std::io::Write,
}

impl<'a, 'b> Interpreter<'a, 'b> {
    pub fn new(out: &'a mut dyn std::io::Write, err: &'b mut dyn std::io::Write) -> Self {
        Self {
            environment: Box::new(Environment::default()),
            out,
            err,
        }
    }

    pub fn interpret(&mut self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts.iter() {
            match self.execute(stmt) {
                Ok(()) => (),
                Err(e) => {
                    writeln!(self.err, "{}", e).expect("failed to write to Interpreter's out");
                }
            }
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), Error> {
        let action = self.evaluate_stmt(stmt)?;
        match action {
            Action::Print(v) => {
                writeln!(self.out, "{}", v).expect("failed to write to Interpreter's out");
            }
            Action::Eval(_) => (),
            Action::Define(t, v) => {
                let name = match t {
                    Token::Identifier(n) => n,
                    _ => unreachable!(),
                };
                self.environment.define(name, v);
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {}
