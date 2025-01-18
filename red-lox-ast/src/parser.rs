use crate::{
    expr::Expr,
    scanner::{Location, Token, TokenWithLocation},
    stmt::Stmt,
};
use thiserror::Error;

#[derive(Error, Debug)]
#[error("{location} ParseError: {msg}")]
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

#[derive(Default)]
pub struct ParseResult {
    pub stmts: Vec<Box<Stmt>>,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<TokenWithLocation>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParseResult {
        let mut result = ParseResult::default();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => result.stmts.push(stmt),
                Err(e) => result.errors.push(e),
            }
        }
        result
    }

    fn declaration(&mut self) -> Result<Box<Stmt>, ParseError> {
        let stmt = match self.peek().token {
            Token::Var => {
                self.advance();
                self.var_declaration()
            }
            _ => self.statement(),
        };
        if stmt.is_err() {
            self.synchronize();
        }
        stmt
    }

    fn var_declaration(&mut self) -> Result<Box<Stmt>, ParseError> {
        let token = self
            .consume(
                |t| match t {
                    Token::Identifier(_) => true,
                    _ => false,
                },
                |t: &TokenWithLocation| format!("Expect variable name, found {:?}", t.token),
            )?
            .token
            .clone();
        let expr = if self.peek().token == Token::Equal {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            |t| t == &Token::Semicolon,
            |t| {
                format!(
                    "Expected ';' after variable declaration, found {:?}.",
                    t.token
                )
            },
        )?;
        Ok(Box::new(Stmt::Var(token, expr)))
    }

    fn statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        match self.peek().token {
            Token::Print => {
                self.advance();
                let expr = self.expression()?;
                self.consume(
                    |t| t == &Token::Semicolon,
                    |t: &TokenWithLocation| {
                        format!(
                            "Expected ';' after value to print, found {:?} instead.",
                            t.token
                        )
                    },
                )?;
                Ok(Box::new(Stmt::Print(expr)))
            }
            Token::LeftBrace => {
                self.advance();
                Ok(Box::new(Stmt::Block(self.block()?)))
            }
            _ => {
                let expr = self.expression()?;
                self.consume(
                    |t| t == &Token::Semicolon,
                    |t: &TokenWithLocation| {
                        format!(
                            "Expected ';' after expression, found {:?} instead.",
                            t.token
                        )
                    },
                )?;
                Ok(Box::new(Stmt::Expression(expr)))
            }
        }
    }

    fn block(&mut self) -> Result<Vec<Box<Stmt>>, ParseError> {
        let mut statements = Vec::new();

        while self.peek().token != Token::RightBrace && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        self.consume(
            |t| t == &Token::RightBrace,
            |t| format!("Expected '}}', found {:?}", t),
        )?;
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Box<Expr>, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<Expr>, ParseError> {
        let expr = self.equality()?;
        match self.peek().token {
            Token::Equal => {
                let equals = self.advance().clone();
                let value = self.assignment()?;
                match *expr {
                    Expr::Variable(
                        t @ TokenWithLocation {
                            token: Token::Identifier(_),
                            ..
                        },
                    ) => Ok(Box::new(Expr::Assign {
                        name: t,
                        expr: value,
                    })),
                    _ => Err(ParseError {
                        msg: "Invalid assignment target.".to_string(),
                        location: equals.location,
                    }),
                }
            }
            _ => Ok(expr),
        }
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
            Identifier(_) => Ok(Box::new(Expr::Variable(token))),
            LeftParen => {
                let expr = self.expression()?;
                self.consume(
                    |t| t == &RightParen,
                    |t: &TokenWithLocation| format!("Expected token ')', found {:?}", t.token),
                )?;
                Ok(Box::new(Expr::Grouping(expr, token.location.clone())))
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

    fn consume<P, F>(
        &mut self,
        expected_token_pred: P,
        msg_gen: F,
    ) -> Result<&TokenWithLocation, ParseError>
    where
        P: FnOnce(&Token) -> bool,
        F: FnOnce(&TokenWithLocation) -> String,
    {
        if !expected_token_pred(&self.peek().token) {
            return Err(ParseError {
                msg: msg_gen(self.peek()),
                location: self.peek().location.clone(),
            });
        }
        Ok(self.advance())
    }

    fn synchronize(&mut self) {
        use Token::*;
        while !self.is_at_end() {
            match self.peek().token {
                Semicolon => {
                    self.advance();
                    break;
                }
                Class | For | Fun | If | Print | Return | Var | While => break,
                _ => {
                    self.advance();
                }
            }
        }
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

    fn add_location(ts: Vec<Token>) -> Vec<TokenWithLocation> {
        ts.into_iter()
            .map(|tok| TokenWithLocation::new(tok, Location::default()))
            .collect()
    }

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
        // TODO: Use googletest's ok matcher when it does not require Copy.
        assert!(result.is_ok(), "result was {:?}", result.err().unwrap());
        assert_eq!(*result.ok().unwrap(), expr);
    }

    #[rstest]
    #[case(Token::Plus)]
    #[case(Token::Minus)]
    #[case(Token::Slash)]
    #[case(Token::Star)]
    #[case(Token::BangEqual)]
    #[case(Token::EqualEqual)]
    #[case(Token::Greater)]
    #[case(Token::GreaterEqual)]
    #[case(Token::Less)]
    #[case(Token::LessEqual)]
    fn parses_binary_ops(#[case] token: Token) {
        let mut parser = Parser::new(add_location(vec![
            Token::Number(1.),
            token.clone(),
            Token::Number(2.),
            Token::Eof,
        ]));
        let result = parser.expression();
        assert!(result.is_ok());
        assert_eq!(
            result.ok().unwrap(),
            Box::new(Expr::Binary {
                left: Box::new(Expr::LiteralNumber(1., Location::default())),
                operator: TokenWithLocation::new(token, Location::default()),
                right: Box::new(Expr::LiteralNumber(2., Location::default()))
            })
        )
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
        let mut parser = Parser::new(add_location(vec![
            Token::Number(1.),
            Token::Slash,
            Token::LeftParen,
            Token::Number(2.),
            Token::Minus,
            Token::Number(3.),
            Token::RightParen,
            Token::Eof,
        ]));
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

    #[test]
    fn parses_plus_and_star_ops_with_correct_precedence() {
        let mut parser = Parser::new(add_location(vec![
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Star,
            Token::Number(3.0),
            Token::Eof,
        ]));
        let result = parser.expression();
        assert!(result.is_ok(), "result was {:?}", result.err().unwrap());
        assert_eq!(
            result.ok().unwrap(),
            Box::new(Expr::Binary {
                left: Box::new(Expr::LiteralNumber(1.0, Location::default())),
                operator: TokenWithLocation::new(Token::Plus, Location::default()),
                right: Box::new(Expr::Binary {
                    left: Box::new(Expr::LiteralNumber(2.0, Location::default())),
                    operator: TokenWithLocation::new(Token::Star, Location::default()),
                    right: Box::new(Expr::LiteralNumber(3.0, Location::default()))
                }),
            })
        )
    }

    #[test]
    fn parses_minus_left_associative() {
        let mut parser = Parser::new(add_location(vec![
            Token::Number(1.0),
            Token::Minus,
            Token::Number(2.0),
            Token::Minus,
            Token::Number(3.0),
            Token::Eof,
        ]));
        let result = parser.expression();
        assert!(result.is_ok(), "result was {:?}", result.err().unwrap());
        assert_eq!(
            result.ok().unwrap(),
            Box::new(Expr::Binary {
                left: Box::new(Expr::Binary {
                    left: Box::new(Expr::LiteralNumber(1.0, Location::default())),
                    operator: TokenWithLocation::new(Token::Minus, Location::default()),
                    right: Box::new(Expr::LiteralNumber(2.0, Location::default()))
                }),
                operator: TokenWithLocation::new(Token::Minus, Location::default()),
                right: Box::new(Expr::LiteralNumber(3.0, Location::default()))
            })
        );
    }

    #[test]
    fn parses_unary_minus_right_associative() {
        let mut parser = Parser::new(add_location(vec![
            Token::Number(1.0),
            Token::Slash,
            Token::Minus,
            Token::Number(2.0),
            Token::Eof,
        ]));
        let result = parser.expression();
        assert!(result.is_ok(), "result was {:?}", result.err().unwrap());
        assert_eq!(
            result.ok().unwrap(),
            Box::new(Expr::Binary {
                left: Box::new(Expr::LiteralNumber(1.0, Location::default())),
                operator: TokenWithLocation::new(Token::Slash, Location::default()),
                right: Box::new(Expr::Unary {
                    operator: TokenWithLocation::new(Token::Minus, Location::default()),
                    right: Box::new(Expr::LiteralNumber(2.0, Location::default()))
                })
            })
        );
    }
}
