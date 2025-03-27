use std::{
    collections::{HashMap, VecDeque},
    ops::{Deref, DerefMut},
};

use crate::{
    code_location_registry::LabelType,
    interned_string::{intern_string, InternedString},
    op_code::OpCode,
    parsing_rule::*,
    write_action::{Arguments, WriteAction},
};

use red_lox_ast::scanner::{
    Location, Scanner, Token, TokenWithLocation, TokenizationError, IDENTIFIER_TOKEN,
};

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
    #[error("{location} Can't use a break statement outside a loop or a switch statement.")]
    MisplacedBreakStatementError { location: Location },
}

pub type Result<T> = std::result::Result<T, Error>;

struct Local {
    name: InternedString,
    depth: i32,
}

struct LocalScope<'a, 'b> {
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
        let upper = self.upper_bound_of_depth(self.scope_depth());
        for _ in 0..(self.locals.len() - upper) {
            self.write_pop(location.clone());
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

struct BreakableStatementGuard<'a, 'b> {
    parser: &'b mut Parser<'a>,
}

impl<'a, 'b> BreakableStatementGuard<'a, 'b> {
    pub fn new(parser: &'b mut Parser<'a>, stmt_type: StatementType, location: Location) -> Self {
        parser.breakable_stmts.push(BreakableStatement {
            statement_type: stmt_type,
            location,
            depth: parser.scope_depth(),
        });
        Self { parser }
    }
}

impl<'a, 'b> Drop for BreakableStatementGuard<'a, 'b> {
    fn drop(&mut self) {
        self.parser.breakable_stmts.pop();
    }
}

impl<'a, 'b> Deref for BreakableStatementGuard<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> DerefMut for BreakableStatementGuard<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

struct BackPatchToken {
    label_type: LabelType,
    location: Location,
}

impl BackPatchToken {
    pub fn new(label_type: LabelType, location: Location) -> Self {
        Self {
            label_type,
            location,
        }
    }

    // Requests for filling the placeholders tied to the (label_type, location) pair.
    pub fn patch(self, writes: &mut VecDeque<WriteAction>) {
        writes.push_back(WriteAction::BackPatchJumpLocation {
            label_type: self.label_type,
            location: self.location,
        });
    }
}

#[derive(Debug, Clone, PartialEq)]
enum StatementType {
    While,
    For,
    Switch,
}

#[derive(Debug, Clone)]
struct BreakableStatement {
    statement_type: StatementType,
    /// Location used to tag the jump location
    location: Location,
    /// The depth of the scope at the statement. The local variables should be popped
    /// if their depth is greater than this value.
    depth: i32,
}

pub(crate) struct Parser<'a> {
    scanner: Scanner<'a>,
    pending_writes: VecDeque<WriteAction>,
    pub(crate) errors: Vec<Error>,
    pub(crate) current: TokenWithLocation,
    pub(crate) prev: TokenWithLocation,
    pub(crate) strings: HashMap<InternedString, Option<u8>>,
    locals: Vec<Local>,
    breakable_stmts: Vec<BreakableStatement>,
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
            breakable_stmts: Vec::new(),
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

    fn upper_bound_of_depth(&self, d: i32) -> usize {
        let mut upper = self.locals.len();
        for i in (0..self.locals.len()).rev() {
            if self.locals[i].depth <= d {
                break;
            }
            upper = i;
        }
        upper
    }

    fn synchronize(&mut self) -> Result<()> {
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

    fn advance(&mut self) -> Result<()> {
        let token = self
            .scanner
            .next_token()
            .map_err(Error::TokenizationError)?;
        std::mem::swap(&mut self.prev, &mut self.current);
        self.current = token;
        Ok(())
    }

    fn next_token_is(&mut self, t: &Token) -> Result<bool> {
        let is_same_type = self.check(t);
        if is_same_type {
            self.advance()?;
        }
        Ok(is_same_type)
    }

    fn check(&self, t: &Token) -> bool {
        std::mem::discriminant(t) == std::mem::discriminant(&self.current.token)
    }

    fn consume<F>(&mut self, t: Token, msg_gen: F) -> Result<TokenWithLocation>
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

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
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

    fn declaration(&mut self) -> Result<()> {
        if self.next_token_is(&Token::Var)? {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<()> {
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

    fn declare_local(&mut self, name: InternedString, ident_location: &Location) -> Result<()> {
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

    fn resolve_local(&self, name: &str, location: &Location) -> Result<Option<u8>> {
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

    fn statement(&mut self) -> Result<()> {
        if self.next_token_is(&Token::Print)? {
            self.print_statement()
        } else if self.next_token_is(&Token::For)? {
            self.for_statement()
        } else if self.next_token_is(&Token::If)? {
            self.if_statement()
        } else if self.next_token_is(&Token::While)? {
            self.while_statement()
        } else if self.next_token_is(&Token::LeftBrace)? {
            let mut scope = self.enter();
            scope.block()
        } else if self.next_token_is(&Token::Break)? {
            self.break_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<()> {
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

    fn for_statement(&mut self) -> Result<()> {
        let for_location = self.prev.location.clone();
        let mut scope = self.enter();
        let mut guard =
            BreakableStatementGuard::new(&mut scope, StatementType::For, for_location.clone());
        guard.consume(Token::LeftParen, |t| {
            format!(
                "{} Expected '(' after 'for', found {:?}",
                t.location, t.token
            )
        })?;

        // Handles the initializer
        if guard.next_token_is(&Token::Semicolon)? {
            // no initializer
        } else if guard.next_token_is(&Token::Var)? {
            guard.var_declaration()?;
        } else {
            guard.expression_statement()?;
        }

        // Handles the condition clause
        guard.add_label(LabelType::ConditionClause, for_location.clone());
        let mut dest_label_after_loop = LabelType::ConditionClause;
        if guard.next_token_is(&Token::Semicolon)? {
            // Write OpCode::True to ensure we have some value on the stack.
            // This is to support the break statement in the following case:
            // for (;;) { break; }
            guard.pending_writes.push_back(WriteAction::OpCodeWrite {
                op_code: OpCode::True,
                args: Arguments::None,
                location: for_location.clone(),
            });
        } else {
            guard.expression()?;
            guard.consume(Token::Semicolon, |t| {
                format!(
                    "{} Expected ';' after loop condition, found {:?}",
                    t.location, t.token
                )
            })?;
        }
        let exit_jump = guard.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::EndOfStatement,
            for_location.clone(),
        );
        guard.write_pop(for_location.clone());

        if !guard.next_token_is(&Token::RightParen)? {
            let body_jump = guard.emit_jump(
                OpCode::Jump,
                LabelType::StartOfStatement,
                for_location.clone(),
            );
            guard.add_label(LabelType::IncrementClause, for_location.clone());
            guard.expression()?;
            guard.write_pop(for_location.clone());
            guard.consume(Token::RightParen, |t| {
                format!(
                    "{} Expected ')' after for clauses, found {:?}",
                    t.location, t.token
                )
            })?;
            guard
                .emit_jump(
                    OpCode::Loop,
                    LabelType::ConditionClause,
                    for_location.clone(),
                )
                .patch(&mut guard.pending_writes);
            dest_label_after_loop = LabelType::IncrementClause;
            body_jump.patch(&mut guard.pending_writes);
        }
        guard.statement()?;
        guard
            .emit_jump(OpCode::Loop, dest_label_after_loop, for_location.clone())
            .patch(&mut guard.pending_writes);
        exit_jump.patch(&mut guard.pending_writes);
        guard.write_pop(for_location);

        Ok(())
    }

    fn if_statement(&mut self) -> Result<()> {
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

        let if_jump_token = self.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::EndOfStatement,
            if_token_location.clone(),
        );
        self.write_pop(if_token_location.clone());
        self.statement()?;

        // the condition of the if stmt is active; we skip the else part if one exist.
        let skip_else_jump_token = self.emit_jump(
            OpCode::Jump,
            LabelType::EndOfStatement,
            if_token_location.clone(),
        );
        if_jump_token.patch(&mut self.pending_writes);
        self.write_pop(if_token_location.clone());

        if self.next_token_is(&Token::Else)? {
            self.statement()?;
        }
        skip_else_jump_token.patch(&mut self.pending_writes);
        Ok(())
    }

    // Emits a WriteAction::OpCodeWrite to pending_writes. The emitted value results in writing the op_code
    // with two one-byte placeholders to be filled. They will be filled with the address corresponding to
    // the (label_type, location) pair when the patch method is called on the returned BackPatchToken.
    fn emit_jump(
        &mut self,
        op_code: OpCode,
        label_type: LabelType,
        location: Location,
    ) -> BackPatchToken {
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code,
            args: Arguments::LabelType(label_type),
            location: location.clone(),
        });
        BackPatchToken::new(label_type, location)
    }

    fn add_label(&mut self, label_type: LabelType, location: Location) {
        self.pending_writes.push_back(WriteAction::AddLabel {
            label_type,
            location,
        });
    }

    fn write_pop(&mut self, location: Location) {
        self.pending_writes.push_back(WriteAction::OpCodeWrite {
            op_code: OpCode::Pop,
            args: Arguments::None,
            location,
        });
    }

    fn while_statement(&mut self) -> Result<()> {
        let while_token_location = self.prev.location.clone();
        let mut guard =
            BreakableStatementGuard::new(self, StatementType::While, while_token_location.clone());
        guard.consume(Token::LeftParen, |t| {
            format!(
                "{} Expected '(' after 'while', found {:?}",
                t.location, t.token
            )
        })?;
        guard.add_label(LabelType::ConditionClause, while_token_location.clone());
        guard.expression()?;
        guard.consume(Token::RightParen, |t| {
            format!(
                "{} Expected ')' after a condition, found {:?}",
                t.location, t.token
            )
        })?;

        let exit_jump = guard.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::EndOfStatement,
            while_token_location.clone(),
        );
        guard.write_pop(while_token_location.clone());
        guard.statement()?;
        let loop_jump = guard.emit_jump(
            OpCode::Loop,
            LabelType::ConditionClause,
            while_token_location.clone(),
        );

        loop_jump.patch(&mut guard.pending_writes);
        exit_jump.patch(&mut guard.pending_writes);
        guard.write_pop(while_token_location);
        Ok(())
    }

    fn break_statement(&mut self) -> Result<()> {
        match self.breakable_stmts.last().cloned() {
            None => Err(Error::MisplacedBreakStatementError {
                location: self.prev.location.clone(),
            }),
            Some(stmt) => {
                let to_pop = self.locals.len() - self.upper_bound_of_depth(stmt.depth);
                for _ in 0..to_pop {
                    self.write_pop(self.prev.location.clone());
                }
                // The current implementation jump to the end of the statement tied to the loop.
                // This means that the next operation is Pop, which is expected to drop the expression
                // generated in the condition clause.
                // The following push of Nil is to offset the Pop operation.
                self.pending_writes.push_back(WriteAction::OpCodeWrite {
                    op_code: OpCode::Nil,
                    args: Arguments::None,
                    location: self.prev.location.clone(),
                });
                self.emit_jump(
                    OpCode::Jump,
                    LabelType::EndOfStatement,
                    stmt.location.clone(),
                );
                self.consume(Token::Semicolon, |t| {
                    format!(
                        "{} Expected ';' after break, found {:?}",
                        t.location, t.token
                    )
                })?;
                Ok(())
            }
        }
    }

    fn block(&mut self) -> Result<()> {
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

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(Token::Semicolon, |t| {
            format!(
                "{} Expected ';' after expression, found {:?}",
                t.location, t.token
            )
        })?;
        self.write_pop(self.prev.location.clone());
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Comma)
    }

    fn assignment(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_next_expr(&mut self, next_expr: NextExpressionType, can_assign: bool) -> Result<()> {
        match next_expr {
            NextExpressionType::None => Ok(()),
            NextExpressionType::Grouping => self.grouping(),
            NextExpressionType::Unary => self.unary(),
            NextExpressionType::Binary => self.binary(),
            NextExpressionType::Number => self.number(),
            NextExpressionType::String => self.string(),
            NextExpressionType::Literal => self.literal(),
            NextExpressionType::Variable => self.variable(can_assign),
            NextExpressionType::And => self.and(),
            NextExpressionType::Or => self.or(),
        }
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
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

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(Token::RightParen, |t| {
            format!(
                "{} Expected ')' after an expression, found {:?}",
                t.location, t.token
            )
        })?;
        Ok(())
    }

    fn and(&mut self) -> Result<()> {
        let and_token_location = self.prev.location.clone();
        let end_jump = self.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::EndOfExpression,
            and_token_location.clone(),
        );
        self.write_pop(and_token_location);
        self.parse_precedence(Precedence::And)?;
        end_jump.patch(&mut self.pending_writes);
        Ok(())
    }

    fn or(&mut self) -> Result<()> {
        let or_token_location = self.prev.location.clone();
        let else_jump = self.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::NextLogicExpression,
            or_token_location.clone(),
        );
        let end_jump = self.emit_jump(
            OpCode::Jump,
            LabelType::EndOfExpression,
            or_token_location.clone(),
        );

        else_jump.patch(&mut self.pending_writes);
        self.write_pop(or_token_location);

        self.parse_precedence(Precedence::Or)?;
        end_jump.patch(&mut self.pending_writes);
        Ok(())
    }

    fn number(&mut self) -> Result<()> {
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

    fn string(&mut self) -> Result<()> {
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

    fn literal(&mut self) -> Result<()> {
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

    fn unary(&mut self) -> Result<()> {
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

    fn binary(&mut self) -> Result<()> {
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
