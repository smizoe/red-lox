use crate::{
    expr::Expr,
    scanner::{Location, Token, TokenWithLocation},
};
use thiserror::Error;

#[derive(Error, Debug)]
#[error("ParseError: {msg} at column {.location.column} in line {.location.line} ")]
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

impl Parser {
    pub fn new(tokens: Vec<TokenWithLocation>) -> Self {
        Self { tokens, current: 0 }
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
                        Ok(expr)
                    }
                    _ => Err(ParseError {
                        msg: format!("Expected token ')', found {:?}", self.peek().token),
                        location: self.peek().location.clone(),
                    }),
                }
            }
            Eof => Err(ParseError {
                msg: String::from_str("Eof reached while parsing an expression"),
                location: token.location,
            }),
            _ => Err(ParseError {
                msg: format!("Unexpected token {:} found", token),
                location: token.location,
            }),
        };
        expr
    }

    fn advance(&mut self) -> &TokenWithLocation {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token == Token::Eof
    }

    fn peek(&self) -> &TokenWithLocation {
        &self.tokens[self.current]
    }
}
