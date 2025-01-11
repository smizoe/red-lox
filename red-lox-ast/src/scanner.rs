use core::fmt;
use phf::phf_map;
use std::{
    fmt::{Display, Formatter},
    str::FromStr,
};
use thiserror::Error;

const KEYWORDS: phf::Map<&'static [u8], Token> = phf_map! {
    b"and" => Token::And,
    b"class" => Token::Class,
    b"else" => Token::Else,
    b"false" => Token::False,
    b"fun" => Token::Fun,
    b"for" => Token::For,
    b"if" => Token::If,
    b"nil" => Token::Nil,
    b"or" => Token::Or,
    b"print" => Token::Print,
    b"return" => Token::Return,
    b"super" => Token::Super,
    b"this" => Token::This,
    b"true" => Token::True,
    b"var" => Token::Var,
    b"while" => Token::While,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "at column {} in line {}", self.column, self.line)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithLocation {
    pub token: Token,
    pub location: Location,
}

impl TokenWithLocation {
    fn new(token: Token, location: Location) -> Self {
        Self { token, location }
    }
}

pub struct Scanner<'a> {
    text: &'a str,
    start: usize,
    current: usize, // yet-to-be consumed char location
    line_no: usize, // starting from 1. Must be in sync with `current`.
    char_no: usize, // starting from 1. char location in a line. Must be in sync with `current`.
}

#[derive(Debug, Default)]
pub struct ScanResult {
    pub errors: Vec<TokenizationError>,
    pub tokens: Vec<TokenWithLocation>,
}

#[derive(Error, Debug)]
pub enum TokenizationError {
    #[error("Error: unexpected character {character} {location}")]
    UnexpectedCharacterError { character: u8, location: Location },
    #[error("Error: unexpected EOF while tokenizing string {location}")]
    UnexpectedEofInStringError { location: Location },
}

impl<'a> Scanner<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text,
            start: 0,
            current: 0,
            line_no: 1,
            char_no: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> ScanResult {
        let mut result = ScanResult::default();
        self.junk();
        while !self.is_at_end() {
            match self.scan_token() {
                Ok(tok) => result.tokens.push(tok),
                Err(e) => result.errors.push(e),
            }
            self.junk();
        }
        result.tokens.push(TokenWithLocation::new(
            Token::Eof,
            Location {
                line: self.line_no,
                column: self.char_no,
            },
        ));
        result
    }

    fn scan_token(&mut self) -> Result<TokenWithLocation, TokenizationError> {
        use Token::*;

        let original_location = Location {
            line: self.line_no,
            column: self.char_no,
        };

        let token = match self.advance() {
            b'(' => Ok(LeftParen),
            b')' => Ok(RightParen),
            b'{' => Ok(LeftBrace),
            b'}' => Ok(RightBrace),
            b',' => Ok(Comma),
            b'.' => Ok(Dot),
            b'-' => Ok(Minus),
            b'+' => Ok(Plus),
            b';' => Ok(Semicolon),
            b'*' => Ok(Star),
            b'!' => Ok(if self.is_next(b'=') { BangEqual } else { Bang }),
            b'=' => Ok(if self.is_next(b'=') {
                EqualEqual
            } else {
                Equal
            }),
            b'<' => Ok(if self.is_next(b'=') { LessEqual } else { Less }),
            b'>' => Ok(if self.is_next(b'=') {
                GreaterEqual
            } else {
                Greater
            }),
            b'/' => Ok(Slash),
            b'"' => self.string(&original_location),
            b'0'..=b'9' => Ok(self.number()),
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => Ok(self.identifier()),
            _ => Err(self.report_unexpected_char(&original_location)),
        };

        token.map(move |t| TokenWithLocation::new(t, original_location))
    }

    fn junk(&mut self) {
        loop {
            match self.peek() {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'\n' => {
                    self.advance();
                    self.line_no += 1;
                    self.char_no = 1;
                }
                b'/' if self.peek_next() == b'/' => {
                    self.advance();
                    self.advance();
                    while self.peek() != b'\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                _ => break,
            }
        }
        self.start = self.current;
    }

    fn is_at_end(&self) -> bool {
        self.text.len() <= self.current
    }

    fn peek(&self) -> u8 {
        if self.is_at_end() {
            return 0;
        }
        return self.text.as_bytes()[self.current];
    }

    fn peek_next(&self) -> u8 {
        if self.current + 1 >= self.text.len() {
            return 0;
        }
        return self.text.as_bytes()[self.current + 1];
    }

    fn is_next(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.text.as_bytes().get(self.current).cloned() != Some(expected) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn advance(&mut self) -> u8 {
        let value = self.text.as_bytes().get(self.current).cloned().unwrap();
        self.current += 1;
        self.char_no += 1;
        value
    }

    fn string(&mut self, original_location: &Location) -> Result<Token, TokenizationError> {
        while self.peek() != b'"' && !self.is_at_end() {
            let b = self.advance();
            if b == b'\n' {
                self.line_no += 1;
                self.char_no = 1;
            }
        }

        if self.is_at_end() {
            return Err(self.report_eof_while_string(original_location));
        }

        // consumes the matching '"'
        self.advance();

        Ok(Token::String(
            String::from_utf8_lossy(&self.text.as_bytes()[(self.start + 1)..(self.current - 1)])
                .to_string(),
        ))
    }

    fn number(&mut self) -> Token {
        while (b'0'..=b'9').contains(&self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' && (b'0'..=b'9').contains(&self.peek_next()) {
            self.advance();
            while (b'0'..=b'9').contains(&self.peek()) {
                self.advance();
            }
        }

        Token::Number(
            f64::from_str(
                String::from_utf8_lossy(&self.text.as_bytes()[self.start..self.current]).as_ref(),
            )
            .unwrap(),
        )
    }

    fn identifier(&mut self) -> Token {
        loop {
            let b = self.peek();
            match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'0'..=b'9' => {
                    self.advance();
                }
                _ => break,
            }
        }
        let id = &self.text.as_bytes()[self.start..self.current];
        match KEYWORDS.get(id) {
            Some(t) => t.clone(),
            None => Token::Identifier(String::from_utf8_lossy(id).to_string()),
        }
    }

    fn report_eof_while_string(&self, original_location: &Location) -> TokenizationError {
        TokenizationError::UnexpectedEofInStringError {
            location: original_location.clone(),
        }
    }

    fn report_unexpected_char(&self, original_location: &Location) -> TokenizationError {
        TokenizationError::UnexpectedCharacterError {
            character: self.text.as_bytes()[self.start],
            location: original_location.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_tokens(result: &ScanResult) -> Vec<Token> {
        result.tokens.iter().map(|t| t.token.clone()).collect()
    }

    #[test]
    fn scanner_adds_eof_token() {
        let mut scanner = Scanner::new("");
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(get_tokens(&result), vec![Token::Eof]);
    }

    #[test]
    fn scanner_handles_spaces() {
        let mut scanner = Scanner::new(" \r\n ");
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(get_tokens(&result), vec![Token::Eof]);
    }

    #[test]
    fn scanner_tokenizes_single_char_tokens_corrcetly() {
        use Token::*;
        let mut scanner = Scanner::new("(){},.-+;*");
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(
            get_tokens(&result),
            vec![
                LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot, Minus, Plus, Semicolon,
                Star, Eof
            ]
        );
    }

    #[test]
    fn scanner_tokenizes_tokens_with_a_one_char_look_ahead() {
        use Token::*;
        let mut scanner = Scanner::new(
            "=!<> // foo bar comment
        /<=>===!=",
        );
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(
            get_tokens(&result),
            vec![
                Equal,
                Bang,
                Less,
                Greater,
                Slash,
                LessEqual,
                GreaterEqual,
                EqualEqual,
                BangEqual,
                Eof
            ]
        );
    }

    #[test]
    fn scanner_tokenizes_numbers_and_strings() {
        use Token::*;
        let mut scanner = Scanner::new(
            " 1. \"\" 1.234 \"foo\" \"test
 case\"",
        );
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(
            get_tokens(&result),
            vec![
                Number(1.0),
                Dot,
                String("".to_string()),
                Number(1.234),
                String("foo".to_string()),
                String("test\n case".to_string()),
                Eof
            ]
        );
    }

    #[test]
    fn scanner_tokenizes_reserved_words() {
        use Token::*;
        let mut scanner = Scanner::new(
            "and class else false for fun if nil
            or print return super this true var while",
        );
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(
            get_tokens(&result),
            vec![
                And, Class, Else, False, For, Fun, If, Nil, Or, Print, Return, Super, This, True,
                Var, While, Eof
            ]
        );
    }

    #[test]
    fn scanner_tokenizes_identifiers() {
        use Token::*;
        let mut scanner = Scanner::new(" x y a1b2c3_d4");
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(
            get_tokens(&result),
            vec![
                Identifier("x".to_string()),
                Identifier("y".to_string()),
                Identifier("a1b2c3_d4".to_string()),
                Eof
            ]
        );
    }

    #[test]
    fn scanner_returns_token_with_location_information() {
        let mut scanner = Scanner::new(
            " x  y and
  z \" foo bar\"",
        );
        let result = scanner.scan_tokens();
        assert!(result.errors.is_empty());
        assert_eq!(
            result.tokens,
            vec![
                TokenWithLocation::new(
                    Token::Identifier("x".to_string()),
                    Location { line: 1, column: 2 }
                ),
                TokenWithLocation::new(
                    Token::Identifier("y".to_string()),
                    Location { line: 1, column: 5 }
                ),
                TokenWithLocation::new(Token::And, Location { line: 1, column: 7 }),
                TokenWithLocation::new(
                    Token::Identifier("z".to_string()),
                    Location { line: 2, column: 3 }
                ),
                TokenWithLocation::new(
                    Token::String(" foo bar".to_string()),
                    Location { line: 2, column: 5 }
                ),
                TokenWithLocation::new(
                    Token::Eof,
                    Location {
                        line: 2,
                        column: 15
                    }
                )
            ]
        );
    }
}
