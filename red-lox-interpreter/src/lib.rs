mod environment;
mod expr;
mod stmt;

use crate::stmt::{Action, Error};
use environment::Environment;
use red_lox_ast::{scanner::Token, stmt::Stmt, visitor::Visitor};

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn interpret(&mut self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts.iter() {
            match Visitor::<Result<Action, Error>>::visit_stmt(self, stmt) {
                Ok(a) => match a {
                    Action::Print(v) => println!("{}", v),
                    Action::Eval(_) => (),
                    Action::Define(t, v) => {
                        let name = match t {
                            Token::Identifier(n) => n,
                            _ => unreachable!(),
                        };
                        self.environment.define(name, v);
                    }
                },
                Err(e) => {
                    println!("A runtime error occurred:");
                    println!("{}", e);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {}
