use crate::expr::Expr;

pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
}
