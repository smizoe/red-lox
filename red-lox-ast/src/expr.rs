use crate::scanner::TokenWithLocation;

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: TokenWithLocation,
    },
    Grouping(Box<Expr>),
    Literal(TokenWithLocation),
    Unary {
        operator: TokenWithLocation,
        right: Box<Expr>,
    },
}

#[cfg(test)]
mod tests {

}