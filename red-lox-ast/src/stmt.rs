use crate::{expr::Expr, scanner::TokenWithLocation};

#[derive(Clone)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var(TokenWithLocation, Option<Box<Expr>>),
    Block(Vec<Box<Stmt>>),
    Function {
        name: TokenWithLocation,
        params: Vec<TokenWithLocation>,
        body: Vec<Box<Stmt>>,
    },
    Class {
        name: TokenWithLocation,
        methods: Vec<Box<Stmt>>,
        superclass: Option<Box<Expr>>,
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
    Return(TokenWithLocation, Option<Box<Expr>>),
}
