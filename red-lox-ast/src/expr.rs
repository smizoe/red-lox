use crate::scanner::{Location, TokenWithLocation};

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: TokenWithLocation,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    LiteralNumber(f64, Location),
    LiteralString(String, Location),
    LiteralBool(bool, Location),
    LiteralNil(Location),
    Unary {
        operator: TokenWithLocation,
        right: Box<Expr>,
    },
}

#[cfg(test)]
mod tests {

}
