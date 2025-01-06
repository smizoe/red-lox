use anyhow::anyhow;
use phf::phf_map;
use std::str::FromStr;

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

#[derive(Clone, Debug)]
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

#[derive(Debug, Clone)]
pub struct TokenInfo {
    token: Token,
    line_no: usize,
    char_no: usize,
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
    errors: Vec<anyhow::Error>,
    tokens: Vec<TokenInfo>,
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
        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(tok) => result.tokens.push(tok),
                Err(e) => result.errors.push(e),
            }
        }
        result
    }

    fn scan_token(&mut self) -> anyhow::Result<TokenInfo> {
        use Token::*;

        self.junk();
        if self.is_at_end() {
            return Ok(self.generate_token_info(Eof));
        }

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
            b'"' => self.string(),
            b'0'..=b'9' => Ok(self.number()),
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => Ok(self.identifier()),
            _ => Err(self.report_unexpected_char()),
        };

        token.map(|t| self.generate_token_info(t))
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
    }

    fn generate_token_info(&self, token: Token) -> TokenInfo {
        TokenInfo {
            token,
            line_no: self.line_no,
            char_no: self.char_no,
        }
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

    fn string(&mut self) -> anyhow::Result<Token> {
        while self.peek() != b'"' && !self.is_at_end() {
            let b = self.advance();
            if b == b'\n' {
                self.line_no += 1;
                self.char_no = 1;
            }
        }

        if self.is_at_end() {
            return Err(self.report_eof_while_string());
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

    fn report_eof_while_string(&self) -> anyhow::Error {
        anyhow!(
            "Error: unexpected EOF while tokenizing string at around char {} in line {}",
            self.char_no - 1,
            self.line_no
        )
    }

    fn report_unexpected_char(&self) -> anyhow::Error {
        anyhow!(
            "Error: unexpected character {} at char {} in line {}",
            self.text.as_bytes()[self.current],
            // since char_no is in sync with `current`, char_no is past the unexpected char.
            self.char_no - 1,
            self.line_no
        )
    }
}
