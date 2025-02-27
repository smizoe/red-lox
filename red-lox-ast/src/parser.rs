use std::ops::{Deref, DerefMut};

use crate::{
    expr::Expr,
    scanner::{Location, Token, TokenWithLocation, IDENTIFIER_TOKEN},
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

struct NestGuard<'a> {
    parser: &'a mut Parser,
}

impl<'a> Drop for NestGuard<'a> {
    fn drop(&mut self) {
        self.parser.nest_level -= 1;
    }
}

impl<'a> Deref for NestGuard<'a> {
    type Target = Parser;

    fn deref(&self) -> &Self::Target {
        self.parser
    }
}

impl<'a> DerefMut for NestGuard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.parser
    }
}

pub struct Parser {
    tokens: Vec<TokenWithLocation>,
    current: usize,
    nest_level: usize,
}

#[derive(Default)]
pub struct ParseResult {
    pub stmts: Vec<Box<Stmt>>,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<TokenWithLocation>) -> Self {
        Self {
            tokens,
            current: 0,
            nest_level: 0,
        }
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

    fn nest(&mut self) -> NestGuard<'_> {
        self.nest_level += 1;
        NestGuard { parser: self }
    }

    fn declaration(&mut self) -> Result<Box<Stmt>, ParseError> {
        let stmt = match self.peek().token {
            Token::Class => {
                self.advance();
                self.class()
            }
            Token::Fun => {
                self.advance();
                self.function("function")
            }
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

    fn class(&mut self) -> Result<Box<Stmt>, ParseError> {
        let name = self
            .consume(Token::is(IDENTIFIER_TOKEN.clone()), |t| {
                format!("Expected class name, found {:?}", t.token)
            })?
            .clone();
        let mut superclass = None;
        if self.peek().token == Token::Less {
            self.advance();
            let superclass_name = self
                .consume(Token::is(IDENTIFIER_TOKEN.clone()), |t| {
                    format!("Expected a superclass name, found {:?}.", t.token)
                })?
                .clone();
            superclass.replace(Box::new(Expr::Variable(superclass_name)));
        }
        self.consume(Token::is(Token::LeftBrace), |t| {
            format!("Expected '{{' before class body, found {:?}", t.token)
        })?;

        let mut methods = Vec::new();
        while self.peek().token != Token::RightBrace && !self.is_at_end() {
            methods.push(self.function("method")?);
        }
        self.consume(Token::is(Token::RightBrace), |t| {
            format!("Expected '}}' after class body, found {:?}", t.token)
        })?;
        Ok(Box::new(Stmt::Class {
            name,
            methods,
            superclass,
        }))
    }

    fn function(&mut self, kind: &str) -> Result<Box<Stmt>, ParseError> {
        let name = self
            .consume(Token::is(IDENTIFIER_TOKEN.clone()), |t| {
                format!("Expected {kind} name, found {:?}", t.token)
            })?
            .clone();
        self.consume(Token::is(Token::LeftParen), |t| {
            format!("Expected '(' after {kind} name, found {:?}", t.token)
        })?;
        let mut params = Vec::new();
        if self.peek().token != Token::RightParen {
            loop {
                params.push(
                    self.consume(Token::is(IDENTIFIER_TOKEN.clone()), |t| {
                        format!("Expected a parameter name, found {:?}", t.token)
                    })?
                    .clone(),
                );
                if self.peek().token != Token::Comma {
                    break;
                }
                self.advance();
            }
        }
        self.consume(Token::is(Token::RightParen), |t| {
            format!("Expected ')' after parameters, found {:?}", t.token)
        })?;
        self.consume(Token::is(Token::LeftBrace), |t| {
            format!("Expected '{{' before {} body, found {:?}", kind, t.token)
        })?;
        let body = self.block()?;
        Ok(Box::new(Stmt::Function { name, params, body }))
    }

    fn var_declaration(&mut self) -> Result<Box<Stmt>, ParseError> {
        let token = self
            .consume(
                Token::is(IDENTIFIER_TOKEN.clone()),
                |t: &TokenWithLocation| format!("Expected variable name, found {:?}", t.token),
            )?
            .clone();
        let expr = if self.peek().token == Token::Equal {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Token::is(Token::Semicolon), |t| {
            format!(
                "Expected ';' after variable declaration, found {:?}.",
                t.token
            )
        })?;
        Ok(Box::new(Stmt::Var(token, expr)))
    }

    fn statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        match self.peek().token {
            Token::If => self.if_stmt(),
            Token::Print => self.print_stmt(),
            Token::Return => self.return_stmt(),
            Token::While => {
                let mut guard = self.nest();
                guard.while_stmt()
            }
            Token::For => {
                let mut guard = self.nest();
                guard.for_stmt()
            }
            Token::LeftBrace => {
                self.advance();
                Ok(Box::new(Stmt::Block(self.block()?)))
            }
            Token::Break => {
                let token = self.advance().clone();
                if self.nest_level > 0 {
                    self.consume(Token::is(Token::Semicolon), |t: &TokenWithLocation| {
                        format!(
                            "Expected ';' after expression, found {:?} instead.",
                            t.token
                        )
                    })?;
                    Ok(Box::new(Stmt::Break))
                } else {
                    Err(ParseError {
                        msg: "Keyword 'break' encountered without a enclosing loop.".to_string(),
                        location: token.location,
                    })
                }
            }
            _ => self.expression_stmt(),
        }
    }

    fn return_stmt(&mut self) -> Result<Box<Stmt>, ParseError> {
        let keyword = self.advance().clone();
        match self.peek().token {
            Token::Semicolon => {
                self.advance();
                Ok(Box::new(Stmt::Return(keyword, None)))
            }
            _ => {
                let expr = self.expression()?;
                self.consume(Token::is(Token::Semicolon), |t| {
                    format!("Expected ';' after return value, found {:?}", t.token)
                })?;
                Ok(Box::new(Stmt::Return(keyword, Some(expr))))
            }
        }
    }

    fn print_stmt(&mut self) -> Result<Box<Stmt>, ParseError> {
        self.advance();
        let expr = self.expression()?;
        self.consume(Token::is(Token::Semicolon), |t: &TokenWithLocation| {
            format!(
                "Expected ';' after value to print, found {:?} instead.",
                t.token
            )
        })?;
        Ok(Box::new(Stmt::Print(expr)))
    }

    fn for_stmt(&mut self) -> Result<Box<Stmt>, ParseError> {
        self.advance();
        self.consume(Token::is(Token::LeftParen), |t| {
            format!("Expected '(' after 'for', found {:?}", t.token)
        })?;

        let initializer = match self.peek().token {
            Token::Semicolon => {
                self.advance();
                None
            }
            Token::Var => {
                self.advance();
                Some(self.var_declaration()?)
            }
            _ => Some(self.expression_stmt()?),
        };

        let condition = match self.peek().token {
            Token::Semicolon => Box::new(Expr::LiteralBool(true, self.peek().location.clone())),
            _ => self.expression()?,
        };

        self.consume(Token::is(Token::Semicolon), |t| {
            format!("Expected ';' after loop condition, found {:?}", t.token)
        })?;

        let increment = if self.peek().token != Token::RightParen {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Token::is(Token::RightParen), |t| {
            format!("Expected ')' after for clauses, found {:?}", t.token)
        })?;

        let mut body = match increment {
            Some(expr) => Box::new(Stmt::Block(vec![
                self.statement()?,
                Box::new(Stmt::Expression(expr)),
            ])),
            None => self.statement()?,
        };

        body = Box::new(Stmt::While { condition, body });

        if let Some(i) = initializer {
            body = Box::new(Stmt::Block(vec![i, body]));
        }

        Ok(body)
    }

    fn while_stmt(&mut self) -> Result<Box<Stmt>, ParseError> {
        self.advance();
        self.consume(Token::is(Token::LeftParen), |t| {
            format!("Expected '(' after 'while', found {:?}", t.token)
        })?;
        let condition = self.expression()?;
        self.consume(Token::is(Token::RightParen), |t| {
            format!("Expected ')' after while condition, found {:?}", t.token)
        })?;
        let body = self.statement()?;
        Ok(Box::new(Stmt::While { condition, body }))
    }

    fn if_stmt(&mut self) -> Result<Box<Stmt>, ParseError> {
        self.advance();
        self.consume(Token::is(Token::LeftParen), |t| {
            format!("Expected '(' after 'if', found {:?}", t.token)
        })?;
        let condition = self.expression()?;
        self.consume(Token::is(Token::RightParen), |t| {
            format!("Expected ')' after if condition, found {:?}", t.token)
        })?;
        let then_branch = self.statement()?;
        let else_branch = match self.peek().token {
            Token::Else => {
                self.advance();
                Some(self.statement()?)
            }
            _ => None,
        };
        Ok(Box::new(Stmt::If {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn expression_stmt(&mut self) -> Result<Box<Stmt>, ParseError> {
        let expr = self.expression()?;
        self.consume(Token::is(Token::Semicolon), |t: &TokenWithLocation| {
            format!(
                "Expected ';' after expression, found {:?} instead.",
                t.token
            )
        })?;
        Ok(Box::new(Stmt::Expression(expr)))
    }

    fn block(&mut self) -> Result<Vec<Box<Stmt>>, ParseError> {
        let mut statements = Vec::new();

        while self.peek().token != Token::RightBrace && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        self.consume(Token::is(Token::RightBrace), |t| {
            format!("Expected '}}', found {:?}", t)
        })?;
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Box<Expr>, ParseError> {
        let expr = self.assignment()?;
        match self.peek().token {
            Token::Comma => {
                let mut exprs = vec![expr];
                while self.peek().token == Token::Comma {
                    self.advance();
                    exprs.push(self.expression()?);
                }
                Ok(Box::new(Expr::ExprSeries(exprs)))
            }
            _ => Ok(expr),
        }
    }

    fn assignment(&mut self) -> Result<Box<Expr>, ParseError> {
        let expr = self.or()?;
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
                    Expr::Get { expr, name } => Ok(Box::new(Expr::Set {
                        lhs: expr,
                        name,
                        rhs: value,
                    })),
                    _ => Err(ParseError {
                        msg: "Invalid assignment target.".to_string(),
                        location: equals.location,
                    }),
                }
            }
            Token::Question => {
                self.advance();
                let left = self.expression()?;
                self.consume(Token::is(Token::Colon), |t| {
                    format!("Expect ':', found {:?}", t)
                })?;
                let right = self.assignment()?;
                Ok(Box::new(Expr::Ternary {
                    cond: expr,
                    left,
                    right,
                }))
            }
            _ => Ok(expr),
        }
    }

    define_parsing_rule! {equality, comparison, BangEqual | EqualEqual}
    define_parsing_rule! {comparison, term, Greater | GreaterEqual | Less | LessEqual}
    define_parsing_rule! {term, factor, Minus | Plus}
    define_parsing_rule! {factor, unary, Slash | Star}

    fn or(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.and()?;

        while self.peek().token == Token::Or {
            let token = self.advance().clone();
            let right = self.and()?;
            expr = Box::new(Expr::Logical {
                left: expr,
                operator: token,
                right,
            });
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.equality()?;
        while self.peek().token == Token::And {
            let token = self.advance().clone();
            let right = self.and()?;
            expr = Box::new(Expr::Logical {
                left: expr,
                operator: token,
                right,
            });
        }
        Ok(expr)
    }

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
            _ => self.call(),
        }
    }

    fn call(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr: Box<Expr> = self.primary()?;

        loop {
            match self.peek().token {
                Token::LeftParen => {
                    self.advance();
                    expr = self.finish_call(expr)?;
                }
                Token::Dot => {
                    self.advance();
                    let name = self
                        .consume(Token::is(IDENTIFIER_TOKEN.clone()), |_t| {
                            format!("Expected a property name after '.'.")
                        })?
                        .clone();
                    expr = Box::new(Expr::Get { expr, name });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Box<Expr>) -> Result<Box<Expr>, ParseError> {
        let mut arguments = Vec::new();
        if self.peek().token != Token::RightParen {
            loop {
                arguments.push(self.assignment()?);
                match self.peek().token {
                    Token::Comma => {
                        self.advance();
                    }
                    _ => break,
                }
            }
        }

        let paren = self
            .consume(Token::is(Token::RightParen), |t| {
                format!("Expect ')' after arguments, found {:?}", t.token)
            })?
            .clone();

        Ok(Box::new(Expr::Call {
            callee,
            paren,
            arguments,
        }))
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
                self.consume(Token::is(RightParen), |t: &TokenWithLocation| {
                    format!("Expected token ')', found {:?}", t.token)
                })?;
                Ok(Box::new(Expr::Grouping(expr, token.location.clone())))
            }
            This => Ok(Box::new(Expr::This(token))),
            Eof => Err(ParseError {
                msg: "Eof reached while parsing an expression".to_string(),
                location: token.location,
            }),
            Super => {
                self.consume(
                    Token::is(Dot),
                    |t| format! {"Expected '.' after super, found {:?}.", t.token},
                )?;
                let method = self.consume(Token::is(IDENTIFIER_TOKEN.clone()), |t| {
                    format!("Expected superclass method name, found {:?}.", t.token)
                })?;
                Ok(Box::new(Expr::Super {
                    keyword: token,
                    method: method.clone(),
                }))
            }
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
