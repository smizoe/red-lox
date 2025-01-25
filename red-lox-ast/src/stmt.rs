use crate::{
    expr::Expr,
    scanner::{Token, TokenWithLocation},
};

#[derive(Clone)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var(Token, Option<Box<Expr>>),
    Block(Vec<Box<Stmt>>),
    Function {
        name: TokenWithLocation,
        params: Vec<TokenWithLocation>,
        body: Vec<Box<Stmt>>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Break,
}

pub trait Evaluator<R> {
    fn evaluate_stmt(&mut self, stmt: &Stmt) -> R;
}
