use std::{
    collections::{HashMap, VecDeque},
    ops::{Deref, DerefMut},
};

use crate::{
    instruction::{Arguments, WriteAction},
    interned_string::{intern_string, InternedString},
    op_code::OpCode,
};

use red_lox_ast::scanner::{
    Location, Scanner, Token, TokenWithLocation, TokenizationError, IDENTIFIER_TOKEN,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None = 0,
    Comma,
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
            None => Comma,
            Comma => Assignment,
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
    String,
    Literal,
    Variable,
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
        Token::Comma => Rule {
            precedence: Precedence::Comma,
            prefix: None,
            infix: Binary,
        },
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
        Token::Bang => Rule {
            precedence: Precedence::None,
            prefix: Unary,
            infix: None,
        },
        Token::BangEqual => Rule {
            precedence: Precedence::Equality,
            prefix: None,
            infix: Binary,
        },
        Token::Equal => Rule::default(),
        Token::EqualEqual => Rule {
            precedence: Precedence::Equality,
            prefix: None,
            infix: Binary,
        },
        Token::Greater => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::GreaterEqual => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::Less => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::LessEqual => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::Identifier(_) => Rule {
            precedence: Precedence::None,
            prefix: Variable,
            infix: None,
        },
        Token::String(_) => Rule {
            precedence: Precedence::None,
            prefix: String,
            infix: None,
        },
        Token::Number(_) => Rule {
            precedence: Precedence::None,
            prefix: Number,
            infix: None,
        },
        Token::And => Rule::default(),
        Token::Break => Rule::default(),
        Token::Class => Rule::default(),
        Token::Else => Rule::default(),
        Token::False => Rule {
            precedence: Precedence::None,
            prefix: Literal,
            infix: None,
        },
        Token::Fun => Rule::default(),
        Token::For => Rule::default(),
        Token::If => Rule::default(),
        Token::Nil => Rule {
            precedence: Precedence::None,
            prefix: Literal,
            infix: None,
        },
        Token::Or => Rule::default(),
        Token::Print => Rule::default(),
        Token::Return => Rule::default(),
        Token::Super => Rule::default(),
        Token::This => Rule::default(),
        Token::True => Rule {
            precedence: Precedence::None,
            prefix: Literal,
            infix: None,
        },
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
    #[error("{location} The lhs of assignment is invalid.")]
    InvalidAssignmentError { location: Location },
    #[error("{location} Too many local variables in the current scope.")]
    TooManyLocalVariablesError { location: Location },
    #[error("{location} Variable '{name}' is already defined in the current scope.")]
    DuplicateVariableDeclarationError { location: Location, name: String },
    #[error("{location} Can't read local variable '{name}' in its own initializer.")]
    UninititalizedVariableAccessError { location: Location, name: String },
}

struct Local {
    name: InternedString,
    depth: i32,
}

pub(crate) struct LocalScope<'a, 'b> {
    parser: &'b mut Parser<'a>,
    left_brace_location: Location,
}

impl<'a, 'b> LocalScope<'a, 'b> {
    pub fn new(parser: &'b mut Parser<'a>) -> Self {
        parser.scope_depth += 1;
        let location = parser.prev.location.clone();
        Self {
            parser,
            left_brace_location: location,
        }
    }
}

impl<'a, 'b> Drop for LocalScope<'a, 'b> {
    fn drop(&mut self) {
        self.parser.scope_depth -= 1;
        let location = self.left_brace_location.clone();

        let mut upper = self.locals.len();
        for i in (0..self.locals.len()).rev() {
            if self.locals[i].depth <= self.scope_depth() {
                break;
            }
            self.write_pop(location.clone());
            upper = i;
        }
        self.locals.truncate(upper);
    }
}

impl<'a, 'b> Deref for LocalScope<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> DerefMut for LocalScope<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

struct BackPatchToken {
    op_code: OpCode,
    location: Location,
}

impl BackPatchToken {
    pub fn new(op_code: OpCode, location: Location) -> Self {
        Self { op_code, location }
    }

    pub fn patch(self, writes: &mut VecDeque<WriteAction>) {
        writes.push_back(WriteAction::BackPatchJumpLocation {
            op_code: self.op_code,
            location: self.location,
        });
    }
}

pub(crate) struct Parser<'a> {
    scanner: Scanner<'a>,
    pending_writes: VecDeque<WriteAction>,
    pub(crate) errors: Vec<Error>,
    pub(crate) current: TokenWithLocation,
    pub(crate) prev: TokenWithLocation,
    pub(crate) strings: HashMap<InternedString, Option<u8>>,
    locals: Vec<Local>,
    scope_depth: i32,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        let mut parser = Self {
            scanner,
            pending_writes: VecDeque::new(),
            errors: Vec::new(),
            current: TokenWithLocation {
                token: Token::Eof,
                location: Location::default(),
            },
            prev: TokenWithLocation {
                token: Token::Eof,
                location: Location::default(),
            },
            strings: HashMap::new(),
            locals: Vec::new(),
            scope_depth: 0,
        };
        parser.advance().expect("No token available in Scanner.");
        parser
    }

    pub fn scope_depth(&self) -> i32 {
        self.scope_depth
    }

    pub fn next_write(&mut self) -> Option<WriteAction> {
        if self.pending_writes.is_empty() && !self.scanner.is_at_end() {
            if let Err(e) = self.declaration() {
                self.errors.push(e);
                if let Err(e) = self.synchronize() {
                    self.errors.push(e);
                }
            }
        }
        self.pending_writes.pop_front()
    }

    fn enter(&mut self) -> LocalScope<'a, '_> {
        LocalScope::new(self)
    }

    fn synchronize(&mut self) -> Result<(), Error> {
        while self.current.token != Token::Eof {
            if self.prev.token == Token::Semicolon {
                return Ok(());
            }

            match self.current.token {
                Token::Class
                | Token::Fun
                | Token::Var
                | Token::For
                | Token::If
                | Token::While
                | Token::Print
                | Token::Return => return Ok(()),
                _ => {
                    self.advance()?;
                }
            }
        }
        Ok(())
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

    fn next_token_is(&mut self, t: &Token) -> Result<bool, Error> {
        let is_same_type = self.check(t);
        if is_same_type {
            self.advance()?;
        }
        Ok(is_same_type)
    }

    fn check(&self, t: &Token) -> bool {
        std::mem::discriminant(t) == std::mem::discriminant(&self.current.token)
    }

    fn consume<F>(&mut self, t: Token, msg_gen: F) -> Result<TokenWithLocation, Error>
    where
        F: FnOnce(&TokenWithLocation) -> String,
    {
        if std::mem::discriminant(&t) == std::mem::discriminant(&self.current.token) {
            self.advance()?;
            Ok(self.prev.clone())
        } else {
            Err(Error::UnexpectedTokenError(msg_gen(&self.current)))
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), Error> {
        self.advance()?;
        let rule = get_rule(&self.prev.token);
        let can_assign = precedence <= Precedence::Assignment;
        self.parse_next_expr(rule.prefix, can_assign)?;

        while precedence <= get_rule(&self.current.token).precedence {
            self.advance()?;
            self.parse_next_expr(get_rule(&self.prev.token).infix, /*unused*/ can_assign)?;
        }

        if can_assign && self.next_token_is(&Token::Equal)? {
            return Err(Error::InvalidAssignmentError {
                location: self.prev.location.clone(),
            });
        }
        Ok(())
    }

    fn declaration(&mut self) -> Result<(), Error> {
        if self.next_token_is(&Token::Var)? {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<(), Error> {
        let mut writes = Vec::with_capacity(2);
        let ident = self.consume(IDENTIFIER_TOKEN, |t| {
            format!(
                "{} Expected a variable name, found {:?}.",
                t.location, t.token
            )
        })?;

        let name = intern_string(&mut self.strings, ident.token.id_name());
        if self.scope_depth > 0 {
            self.declare_local(name.clone(), &ident.location)?;
        }

        if self.next_token_is(&Token::Equal)? {
            self.expression()?;
        } else {
            writes.push(WriteAction::OpCodeWrite {
                op_code: OpCode::Nil,
                args: Arguments::None,
                location: ident.location.clone(),
            });
        }
        self.consume(Token::Semicolon, |t| {
            format!(
                "{} Expected ';' after variable declaration, found {:?}.",
                t.location, t.token
            )
        })?;

        if self.scope_depth == 0 {
            writes.push(WriteAction::OpCodeWrite {
                op_code: OpCode::DefineGlobal,
                args: Arguments::String(name),
                location: ident.location.clone(),
            });
        } else {
            self.locals.last_mut().unwrap().depth = self.scope_depth;
        }
        self.pending_writes.extend(writes);
        Ok(())
    }

    fn declare_local(
        &mut self,
        name: InternedString,
        ident_location: &Location,
    ) -> Result<(), Error> {
        if self.locals.len() == usize::from(u8::MAX) {
            return Err(Error::TooManyLocalVariablesError {
                location: ident_location.clone(),
            });
        }

        self.locals.push(Local {
            name: name.clone(),
            depth: -1,
        });

        for local in self.locals.iter().rev() {
            if local.depth < self.scope_depth {
                break;
            }

            if local.name == name {
                return Err(Error::DuplicateVariableDeclarationError {
                    location: ident_location.clone(),
                    name: name.to_string(),
                });
            }
        }

        self.locals.last_mut().unwrap().depth = self.scope_depth;
        Ok(())
    }

    fn resolve_local(&self, name: &str, location: &Location) -> Result<Option<u8>, Error> {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.depth == -1 {
                return Err(Error::UninititalizedVariableAccessError {
                    location: location.clone(),
                    name: name.to_string(),
                });
            }
            if local.name.as_ref() == name {
                return Ok(Some(u8::try_from(i).unwrap()));
            }
        }
        Ok(None)
    }

    fn statement(&mut self) -> Result<(), Error> {
        if self.next_token_is(&Token::Print)? {
            self.print_statement()
        } else if self.next_token_is(&Token::If)? {
            self.if_statement()
        } else if self.next_token_is(&Token::LeftBrace)? {
            let mut scope = self.enter();
            scope.block()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<(), Error> {
        let location = self.prev.location.clone();
        self.expression()?;
        self.consume(Token::Semicolon, |t| {
            format!(
                "{} Expected ';' after value, found {:?}",
                t.location, t.token
            )
        })?;
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code: OpCode::Print,
            args: Arguments::None,
            location,
        });
        Ok(())
    }

    fn if_statement(&mut self) -> Result<(), Error> {
        let if_token_location = self.prev.location.clone();
        self.consume(Token::LeftParen, |t| {
            format!("{} Expect '(' after 'if', found {:?}", t.location, t.token)
        })?;
        self.expression()?;
        self.consume(Token::RightParen, |t| {
            format!(
                "{} Expect ')' after a condition, found {:?}",
                t.location, t.token
            )
        })?;

        let if_jump_token = self.emit_jump(OpCode::JumpIfFalse, if_token_location.clone());
        self.write_pop(if_token_location.clone());
        self.statement()?;

        let skip_else_jump_token = self.emit_jump(OpCode::Jump, if_token_location.clone());
        if_jump_token.patch(&mut self.pending_writes);
        self.write_pop(if_token_location.clone());

        if self.next_token_is(&Token::Else)? {
            self.statement()?;
        }
        skip_else_jump_token.patch(&mut self.pending_writes);
        Ok(())
    }

    fn emit_jump(&mut self, op_code: OpCode, location: Location) -> BackPatchToken {
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code,
            args: Arguments::None,
            location: location.clone(),
        });
        BackPatchToken::new(op_code, location)
    }

    fn write_pop(&mut self, location: Location) {
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code: OpCode::Pop,
            args: Arguments::None,
            location,
        });
    }

    fn block(&mut self) -> Result<(), Error> {
        while !self.check(&Token::RightBrace) && !self.check(&Token::Eof) {
            self.declaration()?;
        }
        self.consume(Token::RightBrace, |t| {
            format!(
                "{} Expected '}}' after a block, found {:?}.",
                t.location, t.token
            )
        })?;
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), Error> {
        let location = self.prev.location.clone();
        self.expression()?;
        self.consume(Token::Semicolon, |t| {
            format!(
                "{} Expected ';' after expression, found {:?}",
                t.location, t.token
            )
        })?;
        self.write_pop(location);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), Error> {
        self.parse_precedence(Precedence::Comma)
    }

    fn assignment(&mut self) -> Result<(), Error> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_next_expr(
        &mut self,
        next_expr: NextExpressionType,
        can_assign: bool,
    ) -> Result<(), Error> {
        match next_expr {
            NextExpressionType::None => Ok(()),
            NextExpressionType::Grouping => self.grouping(),
            NextExpressionType::Unary => self.unary(),
            NextExpressionType::Binary => self.binary(),
            NextExpressionType::Number => self.number(),
            NextExpressionType::String => self.string(),
            NextExpressionType::Literal => self.literal(),
            NextExpressionType::Variable => self.variable(can_assign),
        }
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), Error> {
        let id = intern_string(&mut self.strings, self.prev.token.id_name());
        let (set_op, get_op, args) = match self.resolve_local(id.as_ref(), &self.prev.location)? {
            Some(v) => (OpCode::SetLocal, OpCode::GetLocal, Arguments::Offset(v)),
            None => (OpCode::SetGlobal, OpCode::GetGlobal, Arguments::String(id)),
        };
        let instruction = if can_assign && self.next_token_is(&Token::Equal)? {
            self.assignment()?;
            WriteAction::OpCodeWrite {
                op_code: set_op,
                args,
                location: self.prev.location.clone(),
            }
        } else {
            WriteAction::OpCodeWrite {
                op_code: get_op,
                args,
                location: self.prev.location.clone(),
            }
        };
        self.pending_writes.push_back(instruction);
        Ok(())
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
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code: OpCode::Constant,
            args: Arguments::Number(v),
            location: self.prev.location.clone(),
        });
        Ok(())
    }

    fn string(&mut self) -> Result<(), Error> {
        let s = match &self.prev.token {
            Token::String(s) => s,
            _ => unreachable!(),
        };
        let v = intern_string(&mut self.strings, s);
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code: OpCode::Constant,
            args: Arguments::String(v),
            location: self.prev.location.clone(),
        });
        Ok(())
    }

    fn literal(&mut self) -> Result<(), Error> {
        let op_code = match &self.prev.token {
            Token::Nil => OpCode::Nil,
            Token::True => OpCode::True,
            Token::False => OpCode::False,
            _ => unreachable!(),
        };
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code,
            args: Arguments::None,
            location: self.prev.location.clone(),
        });
        Ok(())
    }

    fn unary(&mut self) -> Result<(), Error> {
        let instruction = WriteAction::OpCodeWrite {
            op_code: match &self.prev.token {
                Token::Minus => OpCode::Negate,
                Token::Bang => OpCode::Not,
                _ => unreachable!(),
            },
            args: Arguments::None,
            location: self.prev.location.clone(),
        };
        self.parse_precedence(Precedence::Unary)?;
        self.pending_writes.push_back(instruction);
        Ok(())
    }

    fn binary(&mut self) -> Result<(), Error> {
        let op_codes: &[OpCode] = match &self.prev.token {
            Token::Minus => &[OpCode::Subtract],
            Token::Plus => &[OpCode::Add],
            Token::Slash => &[OpCode::Divide],
            Token::Star => &[OpCode::Multiply],
            Token::EqualEqual => &[OpCode::Equal],
            Token::Greater => &[OpCode::Greater],
            Token::Less => &[OpCode::Less],
            Token::BangEqual => &[OpCode::Equal, OpCode::Not],
            Token::GreaterEqual => &[OpCode::Less, OpCode::Not],
            Token::LessEqual => &[OpCode::Greater, OpCode::Not],
            Token::Comma => &[OpCode::Comma],
            _ => unreachable!(),
        };
        let location = self.current.location.clone();
        self.parse_precedence(get_rule(&self.prev.token).precedence.plusone())?;
        for &op_code in op_codes {
            self.pending_writes.push_back(WriteAction::OpCodeWrite {
                op_code,
                args: Arguments::None,
                location: location.clone(),
            });
        }
        Ok(())
    }
}
