mod expr;
mod stmt;
use crate::stmt::{Action, Error};
use red_lox_ast::{stmt::Stmt, visitor::Visitor};

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts.iter() {
            match Visitor::<Result<Action, Error>>::visit_stmt(self, stmt) {
                Ok(a) => match a {
                    Action::Print(v) => println!("{}", v),
                    Action::Eval(_) => (),
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
