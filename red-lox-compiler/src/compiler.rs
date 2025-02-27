use std::{collections::VecDeque, mem::discriminant};

use red_lox_ast::scanner::{Location, Scanner, Token, TokenWithLocation, TokenizationError};

use crate::{
    chunk::{self, Chunk},
    instruction::Instruction,
};

#[derive(Debug, Clone)]
struct InstructionWithLocation {
    instruction: Instruction,
    location: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
    Highest,
}

impl Precedence {
    fn plusone(&self) -> Self {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => Highest,
            Highest => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum NextExpressionType {
    None,
    Grouping,
    Unary,
    Binary,
    Number,
}

#[derive(Debug, Clone)]
struct Rule {
    precedence: Precedence,
    prefix: NextExpressionType,
    infix: NextExpressionType,
}

impl Default for Rule {
    fn default() -> Self {
        Self {
            precedence: Precedence::None,
            prefix: NextExpressionType::None,
            infix: NextExpressionType::None,
        }
    }
}

fn get_rule(token: &Token) -> Rule {
    use NextExpressionType::*;
    match token {
        Token::LeftParen => Rule {
            precedence: Precedence::None,
            prefix: Grouping,
            infix: None,
        },
        Token::RightParen => Rule::default(),
        Token::LeftBrace => Rule::default(),
        Token::RightBrace => Rule::default(),
        Token::Comma => Rule::default(),
        Token::Dot => Rule::default(),
        Token::Minus => Rule {
            precedence: Precedence::Term,
            prefix: Unary,
            infix: Binary,
        },
        Token::Plus => Rule {
            precedence: Precedence::Term,
            prefix: None,
            infix: Binary,
        },
        Token::Colon => Rule::default(),
        Token::Question => Rule::default(),
        Token::Semicolon => Rule::default(),
        Token::Slash => Rule {
            precedence: Precedence::Factor,
            prefix: None,
            infix: Binary,
        },
        Token::Star => Rule {
            precedence: Precedence::Factor,
            prefix: None,
            infix: Binary,
        },
        Token::Bang => Rule::default(),
        Token::BangEqual => Rule::default(),
        Token::Equal => Rule::default(),
        Token::EqualEqual => Rule::default(),
        Token::Greater => Rule::default(),
        Token::GreaterEqual => Rule::default(),
        Token::Less => Rule::default(),
        Token::LessEqual => Rule::default(),
        Token::Identifier(_) => Rule::default(),
        Token::String(_) => Rule::default(),
        Token::Number(_) => Rule {
            precedence: Precedence::None,
            prefix: Number,
            infix: None,
        },
        Token::And => Rule::default(),
        Token::Break => Rule::default(),
        Token::Class => Rule::default(),
        Token::Else => Rule::default(),
        Token::False => Rule::default(),
        Token::Fun => Rule::default(),
        Token::For => Rule::default(),
        Token::If => Rule::default(),
        Token::Nil => Rule::default(),
        Token::Or => Rule::default(),
        Token::Print => Rule::default(),
        Token::Return => Rule::default(),
        Token::Super => Rule::default(),
        Token::This => Rule::default(),
        Token::True => Rule::default(),
        Token::Var => Rule::default(),
        Token::While => Rule::default(),
        Token::Eof => Rule::default(),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("An error occurred while parsing the code: {}", .0)]
    TokenizationError(TokenizationError),
    #[error("{}", .0)]
    UnexpectedTokenError(String),
    #[error("{}", .0)]
    ChunkWriteError(chunk::Error),
}

struct Parser<'a> {
    scanner: Scanner<'a>,
    instructions: VecDeque<InstructionWithLocation>,
    errors: Vec<Error>,
    current: TokenWithLocation,
    prev: TokenWithLocation,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        let mut parser = Self {
            scanner,
            instructions: VecDeque::new(),
            errors: Vec::new(),
            current: TokenWithLocation {
                token: Token::Eof,
                location: Location::default(),
            },
            prev: TokenWithLocation {
                token: Token::Eof,
                location: Location::default(),
            },
        };
        parser.advance().expect("No token available in Scanner.");
        parser
    }

    pub fn next_instruction(&mut self) -> Option<InstructionWithLocation> {
        if self.instructions.is_empty() && !self.scanner.is_at_end() {
            if let Err(e) = self.expression() {
                self.errors.push(e);
            }
        }
        self.instructions.pop_front()
    }

    fn advance(&mut self) -> Result<(), Error> {
        let token = self
            .scanner
            .next_token()
            .map_err(Error::TokenizationError)?;
        std::mem::swap(&mut self.prev, &mut self.current);
        self.current = token;
        Ok(())
    }

    fn consume<F>(&mut self, t: Token, msg_gen: F) -> Result<TokenWithLocation, Error>
    where
        F: FnOnce(&TokenWithLocation) -> String,
    {
        if std::mem::discriminant(&t) == std::mem::discriminant(&self.current.token) {
            Ok(self.current.clone())
        } else {
            Err(Error::UnexpectedTokenError(msg_gen(&self.current)))
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), Error> {
        self.advance()?;
        let rule = get_rule(&self.prev.token);
        self.parse_next_expr(rule.prefix)?;

        while precedence <= get_rule(&self.current.token).precedence {
            self.advance()?;
            self.parse_next_expr(get_rule(&self.prev.token).infix)?;
        }
        Ok(())
    }

    fn expression(&mut self) -> Result<(), Error> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_next_expr(&mut self, next_expr: NextExpressionType) -> Result<(), Error> {
        match next_expr {
            NextExpressionType::None => Ok(()),
            NextExpressionType::Grouping => self.grouping(),
            NextExpressionType::Unary => self.unary(),
            NextExpressionType::Binary => self.binary(),
            NextExpressionType::Number => self.number(),
        }
    }

    fn grouping(&mut self) -> Result<(), Error> {
        self.expression()?;
        self.consume(Token::RightParen, |t| {
            format!(
                "{} Expected ')' after an expression, found {:?}",
                t.location, t.token
            )
        })?;
        Ok(())
    }

    fn number(&mut self) -> Result<(), Error> {
        let v = match &self.prev.token {
            Token::Number(v) => *v,
            _ => unreachable!(),
        };
        self.instructions.push_back(InstructionWithLocation {
            instruction: Instruction::Constant(v),
            location: self.prev.location.clone(),
        });
        Ok(())
    }

    fn unary(&mut self) -> Result<(), Error> {
        let instruction = InstructionWithLocation {
            instruction: match &self.prev.token {
                Token::Minus => Instruction::Negate,
                _ => unreachable!(),
            },
            location: self.prev.location.clone(),
        };
        self.parse_precedence(Precedence::Unary)?;
        self.instructions.push_back(instruction);
        Ok(())
    }

    fn binary(&mut self) -> Result<(), Error> {
        let instruction = InstructionWithLocation {
            instruction: match &self.prev.token {
                Token::Minus => Instruction::Subtract,
                Token::Plus => Instruction::Add,
                Token::Slash => Instruction::Divide,
                Token::Star => Instruction::Multiply,
                _ => unreachable!(),
            },
            location: self.current.location.clone(),
        };
        self.parse_precedence(get_rule(&self.prev.token).precedence.plusone())?;
        self.instructions.push_back(instruction);
        Ok(())
    }
}

pub struct Compiler<'a> {
    chunk: Chunk,
    parser: Parser<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chunk: Chunk::new(),
            parser: Parser::new(Scanner::new(text)),
        }
    }

    pub fn compile(&mut self) -> Result<(), Error> {
        while let Some(InstructionWithLocation {
            instruction,
            location,
        }) = self.parser.next_instruction()
        {
            if let Some(e) = self.parser.errors.pop() {
                return Err(e);
            }
            self.chunk
                .write(&instruction, location.line)
                .map_err(Error::ChunkWriteError)?;
        }
        self.chunk
            .write(&Instruction::Return, self.parser.current.location.line)
            .map_err(Error::ChunkWriteError)?;
        Ok(())
    }

    pub fn finish(self) -> Chunk {
        return self.chunk;
    }
}
