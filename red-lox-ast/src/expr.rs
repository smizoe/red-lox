use crate::scanner::{Location, TokenWithLocation};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    ExprSeries(Vec<Box<Expr>>),
    Ternary {
        cond: Box<Expr>,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: TokenWithLocation,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: TokenWithLocation,
        arguments: Vec<Box<Expr>>,
    },
    Logical {
        left: Box<Expr>,
        operator: TokenWithLocation,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>, Location),
    LiteralNumber(f64, Location),
    LiteralString(String, Location),
    LiteralBool(bool, Location),
    LiteralNil(Location),
    Unary {
        operator: TokenWithLocation,
        right: Box<Expr>,
    },
    Variable(TokenWithLocation),
    Assign {
        name: TokenWithLocation,
        expr: Box<Expr>,
    },
    Get {
        expr: Box<Expr>,
        name: TokenWithLocation,
    },
    Set {
        lhs: Box<Expr>,
        name: TokenWithLocation,
        rhs: Box<Expr>,
    },
}
