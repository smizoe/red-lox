use crate::scanner::{Location, TokenWithLocation};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary {
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
}
