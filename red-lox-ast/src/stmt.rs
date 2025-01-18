use crate::{expr::Expr, scanner::Token};

pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var(Token, Option<Box<Expr>>),
    Block(Vec<Box<Stmt>>),
}
