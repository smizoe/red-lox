use crate::{
    expr::Expr,
    scanner::{Location, Token, TokenWithLocation},
};
use thiserror::Error;

#[derive(Error, Debug)]
#[error("ParseError: {msg} at column {} in line {}", .location.column, .location.line)]
pub struct ParseError {
    msg: String,
    location: Location,
}

macro_rules! define_parsing_rule {
    ($cur_rule:tt, $next_rule:ident, $p:pat) => {
        fn $cur_rule(&mut self) -> Result<Box<Expr>, ParseError> {
            use Token::*;
            let mut expr = self.$next_rule()?;
            loop {
                match &self.peek().token {
                    $p => {
                        let operator = self.advance().clone();
                        let right = self.$next_rule()?;
                        expr = Box::new(Expr::Binary {
                            left: expr,
                            operator: operator,
                            right,
                        });
                    }
                    _ => break,
                }
            }
            Ok(expr)
        }
    };
}

pub struct Parser {
    tokens: Vec<TokenWithLocation>,
    current: usize,
}

pub struct ParseResult {
    pub expr: Box<Expr>,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<TokenWithLocation>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParseResult {
        let expr = self.expression();
        match expr {
            Ok(expr) => ParseResult {
                expr: expr,
                errors: vec![],
            },
            Err(e) => ParseResult {
                expr: Box::new(Expr::LiteralNil(Location { column: 0, line: 0 })),
                errors: vec![e],
            },
        }
    }

    fn expression(&mut self) -> Result<Box<Expr>, ParseError> {
        self.equality()
    }

    define_parsing_rule! {equality, comparison, BangEqual | EqualEqual}
    define_parsing_rule! {comparison, term, Greater | GreaterEqual | Less | LessEqual}
    define_parsing_rule! {term, factor, Minus | Plus}
    define_parsing_rule! {factor, unary, Slash | Star}

    fn unary(&mut self) -> Result<Box<Expr>, ParseError> {
        use Token::*;
        match &self.peek().token {
            Bang | Minus => {
                let operator = self.advance().clone();
                let right = self.unary()?;
                Ok(Box::new(Expr::Unary {
                    operator: operator.clone(),
                    right: right,
                }))
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Box<Expr>, ParseError> {
        use Token::*;
        let token = self.advance().clone();
        let expr = match &token.token {
            False => Ok(Box::new(Expr::LiteralBool(false, token.location))),
            True => Ok(Box::new(Expr::LiteralBool(true, token.location))),
            Nil => Ok(Box::new(Expr::LiteralNil(token.location))),
            Number(v) => Ok(Box::new(Expr::LiteralNumber(*v, token.location))),
            String(s) => Ok(Box::new(Expr::LiteralString(s.clone(), token.location))),
            LeftParen => {
                let expr = self.expression()?;
                match self.peek().token {
                    RightParen => {
                        self.advance();
                        Ok(Box::new(Expr::Grouping(expr, token.location.clone())))
                    }
                    _ => Err(ParseError {
                        msg: format!("Expected token ')', found {:?}", self.peek().token),
                        location: self.peek().location.clone(),
                    }),
                }
            }
            Eof => Err(ParseError {
                msg: "Eof reached while parsing an expression".to_string(),
                location: token.location,
            }),
            _ => Err(ParseError {
                msg: format!("Unexpected token {:?} found", token.token),
                location: token.location,
            }),
        };
        expr
    }

    fn advance(&mut self) -> &TokenWithLocation {
        if !self.is_at_end() {
            self.current += 1;
        }
        // checked_sub is used so this does not panic even when self.tokens consists only of Eof.
        &self.tokens[self.current.checked_sub(1).unwrap_or(0)]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token == Token::Eof
    }

    fn peek(&self) -> &TokenWithLocation {
        &self.tokens[self.current]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use googletest::gtest;
    use googletest::prelude::*;
    use rstest::rstest;

    #[rstest]
    #[case(Token::Nil, Expr::LiteralNil(Location::default()))]
    #[case(Token::False, Expr::LiteralBool(false, Location::default()))]
    #[case(Token::True, Expr::LiteralBool(true, Location::default()))]
    #[case(Token::String("string".to_string()), Expr::LiteralString("string".to_string(), Location::default()))]
    #[case(Token::Number(0.5), Expr::LiteralNumber(0.5, Location::default()))]
    fn parses_literals(#[case] token: Token, #[case] expr: Expr) {
        let mut parser = Parser::new(vec![
            TokenWithLocation::new(token, Location::default()),
            TokenWithLocation::new(Token::Eof, Location::default()),
        ]);
        let result = parser.expression();
        assert!(result.is_ok(), "result was {:?}", result.err().unwrap());
        assert_eq!(*result.ok().unwrap(), expr);
    }

    #[gtest]
    fn returns_error_when_encountering_eof() {
        let mut parser = Parser::new(vec![TokenWithLocation::new(
            Token::Eof,
            Location {
                column: 10,
                line: 20,
            },
        )]);
        let result = parser.expression();
        assert!(result.is_err());
        expect_that!(
            result.err().unwrap(),
            matches_pattern!(ParseError {
                msg: contains_substring("Eof reached while parsing an expression"),
                location: eq(&Location {
                    column: 10,
                    line: 20
                })
            })
        );
    }

    #[test]
    fn parses_grouping() {
        let mut parser = Parser::new(vec![
            TokenWithLocation::new(Token::Number(1.), Location::default()),
            TokenWithLocation::new(Token::Slash, Location::default()),
            TokenWithLocation::new(Token::LeftParen, Location::default()),
            TokenWithLocation::new(Token::Number(2.), Location::default()),
            TokenWithLocation::new(Token::Minus, Location::default()),
            TokenWithLocation::new(Token::Number(3.), Location::default()),
            TokenWithLocation::new(Token::RightParen, Location::default()),
            TokenWithLocation::new(Token::Eof, Location::default()),
        ]);
        let result = parser.expression();
        assert!(result.is_ok(), "result was {:?}", result.err().unwrap());
        assert_eq!(
            *result.ok().unwrap(),
            Expr::Binary {
                left: Box::new(Expr::LiteralNumber(1., Location::default())),
                operator: TokenWithLocation::new(Token::Slash, Location::default()),
                right: Box::new(Expr::Grouping(
                    Box::new(Expr::Binary {
                        left: Box::new(Expr::LiteralNumber(2., Location::default())),
                        operator: TokenWithLocation::new(Token::Minus, Location::default()),
                        right: Box::new(Expr::LiteralNumber(3., Location::default())),
                    }),
                    Location::default()
                ))
            }
        );
    }
}
