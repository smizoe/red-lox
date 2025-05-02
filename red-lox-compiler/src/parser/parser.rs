use std::collections::VecDeque;

use crate::{
    common::{
        code_location_registry::LabelType,
        op_code::OpCode,
        variable_location::Local,
        write_action::{FunctionType, WriteAction},
        InternedString, InternedStringRegistry,
    },
    parser::{
        guard::{
            BreakableStatement, BreakableStatementGuard, FunctionDeclarationScope, FunctionEnv,
            LocalScope, StatementType,
        },
        Error, Result,
    },
};

use red_lox_ast::scanner::{Location, Scanner, Token, TokenWithLocation, IDENTIFIER_TOKEN};

use super::{guard::ClassDeclarationScope, parsing_rule::*};

const SWITCH_EXPR_BEING_MATCHED: &'static str = "?switch_expr";
const SWITCH_MATCH_COUNTER: &'static str = "?switch_match";

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

pub(crate) struct Parser<'a> {
    scanner: Scanner<'a>,
    pending_writes: VecDeque<WriteAction>,
    pub(crate) errors: Vec<Error>,
    pub(crate) current: TokenWithLocation,
    pub(crate) prev: TokenWithLocation,
    pub(crate) interned_string_registry: InternedStringRegistry,
    pub(in crate::parser) env: Box<FunctionEnv>,
    pub(in crate::parser) scope_depth: i32,
    pub(in crate::parser) class_nest_depth: i32,
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
            interned_string_registry: InternedStringRegistry::new(),
            env: Box::new(FunctionEnv::new(
                InternedString::get_empty_string(),
                FunctionType::Script,
            )),
            scope_depth: 0,
            class_nest_depth: 0,
        };
        parser.advance().expect("No token available in Scanner.");
        parser
    }

    pub(crate) fn append_write(&mut self, write_action: WriteAction) {
        self.pending_writes.push_back(write_action);
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

    pub(in crate::parser) fn locals(&self) -> &Vec<Local> {
        &self.env.locals
    }

    pub(in crate::parser) fn locals_mut(&mut self) -> &mut Vec<Local> {
        &mut self.env.locals
    }

    fn breakable_stmts(&self) -> &Vec<BreakableStatement> {
        &self.env.breakable_stmts
    }

    pub(crate) fn upper_bound_of_depth(&self, d: i32) -> usize {
        let mut upper = self.locals().len();
        for i in (0..self.locals().len()).rev() {
            if self.locals()[i].depth <= d {
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
            if self.prev.token == Token::Question {
                self.ternary()?;
            } else {
                self.parse_next_expr(get_rule(&self.prev.token).infix, /*unused*/ can_assign)?;
            }
        }

        if can_assign && self.next_token_is(&Token::Equal)? {
            return Err(Error::InvalidAssignmentError {
                location: self.prev.location.clone(),
            });
        }
        Ok(())
    }

    fn declaration(&mut self) -> Result<()> {
        if self.next_token_is(&Token::Class)? {
            self.class_declaration()
        } else if self.next_token_is(&Token::Fun)? {
            self.fun_declaration()
        } else if self.next_token_is(&Token::Var)? {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> Result<()> {
        let mut scope = ClassDeclarationScope::new(self);
        let ident = scope.consume(IDENTIFIER_TOKEN, |t| {
            format!(
                "{} Expected class name after 'class', found {:?}",
                t.location, t.token
            )
        })?;
        let name = scope
            .interned_string_registry
            .intern_string(ident.token.id_name());

        if scope.scope_depth > 0 {
            scope.declare_local(name.clone(), &ident.location)?;
        }

        let is_global = scope.scope_depth == 0;
        scope.append_write(WriteAction::ClassDeclaration {
            name,
            is_global,
            location: ident.location.clone(),
        });

        if scope.scope_depth > 0 {
            scope.mark_most_recent_local_initialized();
        }

        scope.variable(false)?; // load the class onto the stack.

        scope.consume(Token::LeftBrace, |t| {
            format!(
                "{} Expected '{{' after the class name, found {:?}",
                t.location, t.token
            )
        })?;

        while !scope.check(&Token::RightBrace) && !scope.check(&Token::Eof) {
            scope.method()?;
        }

        scope.consume(Token::RightBrace, |t| {
            format!(
                "{} Expected '}}' at the end of class declaration, found {:?}",
                t.location, t.token
            )
        })?;
        scope.write_pop(ident.location);
        Ok(())
    }

    fn method(&mut self) -> Result<()> {
        let method_name = self.consume(IDENTIFIER_TOKEN, |t| {
            format!("{} Expected a method name, found {:?}", t.location, t.token)
        })?;
        let identifier = self
            .interned_string_registry
            .intern_string(method_name.token.id_name());
        let func_type = if identifier.as_ref() == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(identifier.clone(), func_type)?;
        self.append_write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Method,
            location: method_name.location,
        });
        Ok(())
    }

    fn fun_declaration(&mut self) -> Result<()> {
        let ident = self.consume(IDENTIFIER_TOKEN, |t| {
            format!(
                "{} Expected a function name, found {:?}.",
                t.location, t.token
            )
        })?;
        let fun_name = self
            .interned_string_registry
            .intern_string(ident.token.id_name());
        if self.scope_depth > 0 {
            let fun_location = self.prev.location.clone();
            self.declare_local(fun_name.clone(), &fun_location)?;
            self.mark_most_recent_local_initialized();
        }
        self.function(fun_name, FunctionType::Function)?;
        Ok(())
    }

    fn function(&mut self, fun_name: InternedString, function_type: FunctionType) -> Result<()> {
        let args = self.get_fun_args()?;
        let mut fun_scope =
            FunctionDeclarationScope::new(self, fun_name, args.len(), function_type);
        {
            let mut scope: LocalScope<'_, '_> = fun_scope.enter();
            for arg in args {
                let name = scope
                    .interned_string_registry
                    .intern_string(arg.token.id_name());
                scope.declare_local(name, &arg.location)?;
                scope.mark_most_recent_local_initialized();
            }
            scope.consume(Token::LeftBrace, |t| {
                format!(
                    "{} Expected '{{' before the function body, found {:?}",
                    t.location, t.token
                )
            })?;
            scope.block()?;
        }
        Ok(())
    }

    fn get_fun_args(&mut self) -> Result<Vec<TokenWithLocation>> {
        let mut args = Vec::new();
        self.consume(Token::LeftParen, |t| {
            format!(
                "{} Expected '(' after the function name, found {:?}",
                t.location, t.token
            )
        })?;
        if !self.check(&Token::RightParen) {
            loop {
                args.push(self.consume(IDENTIFIER_TOKEN.clone(), |t| {
                    format!(
                        "{} Expected an identifier token as a function parameter, found {:?}",
                        t.location, t.token
                    )
                })?);
                if !self.next_token_is(&Token::Comma)? {
                    break;
                }
            }
        }
        self.consume(Token::RightParen, |t| {
            format!(
                "{} Expected ')' after the function parameters, found {:?}",
                t.location, t.token
            )
        })?;
        Ok(args)
    }

    fn var_declaration(&mut self) -> Result<()> {
        let mut writes = Vec::with_capacity(2);
        let ident = self.consume(IDENTIFIER_TOKEN, |t| {
            format!(
                "{} Expected a variable name, found {:?}.",
                t.location, t.token
            )
        })?;

        let name = self
            .interned_string_registry
            .intern_string(ident.token.id_name());
        if self.scope_depth > 0 {
            self.declare_local(name.clone(), &ident.location)?;
        }

        if self.next_token_is(&Token::Equal)? {
            self.expression()?;
        } else {
            writes.push(WriteAction::WriteNoArgOpCode {
                op_code: OpCode::Nil,
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
            writes.push(WriteAction::WriteOpCodeWithIdentifier {
                op_code: OpCode::DefineGlobal,
                identifier: name,
                location: ident.location.clone(),
            });
        } else {
            self.mark_most_recent_local_initialized();
        }
        self.pending_writes.extend(writes);
        Ok(())
    }

    fn declare_local(&mut self, name: InternedString, ident_location: &Location) -> Result<()> {
        if self.locals().len() == usize::from(u8::MAX) {
            return Err(Error::TooManyLocalVariablesError {
                location: ident_location.clone(),
            });
        }

        self.locals_mut().push(Local::new(name.clone(), -1));

        for local in self.locals().iter().rev() {
            if local.depth < self.scope_depth {
                break;
            }

            if local.name() == name.as_ref() {
                return Err(Error::DuplicateVariableDeclarationError {
                    location: ident_location.clone(),
                    name: name.to_string(),
                });
            }
        }

        Ok(())
    }

    fn resolve_local(&self, name: &str, location: &Location) -> Result<Option<u8>> {
        self.env.resolve_local(name, location)
    }

    fn resolve_upvalue(&mut self, name: &str, location: &Location) -> Result<Option<u8>> {
        self.env.resolve_upvalue(name, location)
    }

    fn statement(&mut self) -> Result<()> {
        if self.next_token_is(&Token::Print)? {
            self.print_statement()
        } else if self.next_token_is(&Token::For)? {
            self.for_statement()
        } else if self.next_token_is(&Token::If)? {
            self.if_statement()
        } else if self.next_token_is(&Token::Return)? {
            self.return_statement()
        } else if self.next_token_is(&Token::While)? {
            self.while_statement()
        } else if self.next_token_is(&Token::Switch)? {
            self.switch_statement()
        } else if self.next_token_is(&Token::LeftBrace)? {
            let mut scope = self.enter();
            scope.block()
        } else if self.next_token_is(&Token::Break)? {
            self.break_statement()
        } else if self.next_token_is(&Token::Continue)? {
            self.continue_statement()
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
        self.pending_writes
            .push_back(WriteAction::WriteNoArgOpCode {
                op_code: OpCode::Print,
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
        if guard.next_token_is(&Token::Semicolon)? {
            // Write OpCode::True to ensure we have some value on the stack.
            // This is to support the break statement in the following case:
            // for (;;) { break; }
            guard
                .pending_writes
                .push_back(WriteAction::WriteNoArgOpCode {
                    op_code: OpCode::True,
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
        let body_jump = guard.emit_jump(
            OpCode::Jump,
            LabelType::StartOfStatement,
            for_location.clone(),
        );
        guard.add_label(LabelType::IncrementClause, for_location.clone());
        if !guard.next_token_is(&Token::RightParen)? {
            guard.expression()?;
            guard.write_pop(for_location.clone());
            guard.consume(Token::RightParen, |t| {
                format!(
                    "{} Expected ')' after for clauses, found {:?}",
                    t.location, t.token
                )
            })?;
        }
        guard
            .emit_jump(
                OpCode::Loop,
                LabelType::ConditionClause,
                for_location.clone(),
            )
            .patch(&mut guard.pending_writes);
        body_jump.patch(&mut guard.pending_writes);
        guard.statement()?;
        guard
            .emit_jump(
                OpCode::Loop,
                LabelType::IncrementClause,
                for_location.clone(),
            )
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
            self.prev.location.clone(), // '}' of the end of the if stmt
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
        self.pending_writes.push_back(WriteAction::WriteJumpOpCode {
            op_code,
            label_type,
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

    pub(crate) fn write_pop(&mut self, location: Location) {
        self.append_write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Pop,
            location,
        });
    }

    fn return_statement(&mut self) -> Result<()> {
        let location = self.prev.location.clone();
        if self.env.function_type == FunctionType::Script {
            return Err(Error::ReturnFromTopLevelError { location });
        }

        if self.function_type() == FunctionType::Initializer {
            if !self.check(&Token::Semicolon) {
                return Err(Error::InvalidReturnValueFromInitializerError { location });
            }
            // Returning `this` is handled when the method's FunctionDeclarationScope is closed.
        } else {
            if self.check(&Token::Semicolon) {
                self.append_write(WriteAction::WriteNoArgOpCode {
                    op_code: OpCode::Nil,
                    location: location.clone(),
                });
            } else {
                self.expression()?;
            }
            self.write_return(location);
        }

        self.consume(Token::Semicolon, |t| {
            format!(
                "{} Expected ';' after the return statement, found {:?}",
                t.location, t.token
            )
        })?;
        Ok(())
    }

    fn write_return(&mut self, location: Location) {
        self.append_write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Return,
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

    fn switch_statement(&mut self) -> Result<()> {
        // Idea:
        // - Store the value of the expression being matched as a local
        // - Store a number (zero) on top of the value above
        // - Use the boolean value to decide if we make a jump to the next case branch
        //   (i.e., if zero, we skip the branch)
        // - Each case branch updates the boolean value by doing the following:
        //   * load the value being matched
        //   * evaluate the expression of the branch
        //   * check for the equality of the two
        //   * if they are equal, increment the number, otherwise we skip the increment
        let switch_location = self.prev.location.clone();
        let mut breakable_stmt_scope =
            BreakableStatementGuard::new(self, StatementType::Switch, switch_location.clone());
        breakable_stmt_scope.consume(Token::LeftParen, |t| {
            format!(
                "{} Expected '(' after switch, found {:?}",
                t.location, t.token
            )
        })?;
        breakable_stmt_scope.expression()?;
        breakable_stmt_scope.consume(Token::RightParen, |t| {
            format!(
                "{} Expected ')' after the expression for a switch statment, found {:?}",
                t.location, t.token
            )
        })?;
        breakable_stmt_scope.consume(Token::LeftBrace, |t| {
            format!(
                "{} Expected '{{' at the beginning of switch block, found {:?}",
                t.location, t.token
            )
        })?;
        {
            let mut local_scope = LocalScope::new(&mut breakable_stmt_scope);
            let left_paren_location = local_scope.prev.location.clone();
            let (switch_expr_being_matched_str, switch_match_counter_str) =
                local_scope.prepare_local_values_for_switch_statement()?;
            let switch_base_expr_index = local_scope
                .resolve_local(switch_expr_being_matched_str.as_ref(), &left_paren_location)?
                .unwrap();
            let switch_match_counter_index = local_scope
                .resolve_local(switch_match_counter_str.as_ref(), &left_paren_location)?
                .unwrap();

            while local_scope.next_token_is(&Token::Case)? {
                local_scope.switch_case(switch_base_expr_index, switch_match_counter_index)?;
            }
            if local_scope.next_token_is(&Token::Default)? {
                local_scope.default_case()?;
            }
            local_scope.consume(Token::RightBrace, |t| {
                format!(
                    "{} Expected '}}' at the end of switch block, found {:?}",
                    t.location, t.token
                )
            })?;
        }
        // HACK: To backfill the jump location, we (meaninglessly) jump to the next instruction and fill the holes.
        breakable_stmt_scope
            .emit_jump(OpCode::Jump, LabelType::EndOfStatement, switch_location)
            .patch(&mut breakable_stmt_scope.pending_writes);
        Ok(())
    }

    fn prepare_local_values_for_switch_statement(
        &mut self,
    ) -> Result<(InternedString, InternedString)> {
        let left_brace_location = self.prev.location.clone();

        let switch_expr_being_matched = self
            .interned_string_registry
            .intern_string(SWITCH_EXPR_BEING_MATCHED);
        self.declare_local(switch_expr_being_matched.clone(), &left_brace_location)?;
        self.mark_most_recent_local_initialized();

        let switch_match_counter = self
            .interned_string_registry
            .intern_string(SWITCH_MATCH_COUNTER);
        self.append_write(WriteAction::WriteOpCodeWithValue {
            op_code: OpCode::Constant,
            value: crate::common::value::Value::Number(0.0),
            location: left_brace_location.clone(),
        });
        self.declare_local(switch_match_counter.clone(), &left_brace_location)?;
        self.mark_most_recent_local_initialized();

        Ok((switch_expr_being_matched, switch_match_counter))
    }

    // Post-condition: the top of the stack contains a boolean that corresponds to
    // whether the branch was run
    fn switch_case(
        &mut self,
        switch_base_expr_index: u8,
        switch_match_counter_index: u8,
    ) -> Result<()> {
        // switch (e) { ... case e_b: ... }
        self.expression()?;
        self.consume(Token::Colon, |t| format!("{} Expected ':' after the matching expression for a switch-case branch, found {:?}", t.location, t.token))?;
        let colon_location = self.prev.location.clone();
        // Update the boolean value that activates the current branch
        self.append_write(WriteAction::WriteOpCodeWithOffset {
            op_code: OpCode::GetLocal,
            offset: switch_base_expr_index,
            location: colon_location.clone(),
        });
        self.append_write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Equal,
            location: colon_location.clone(),
        });
        // the stack top is the result of e == e_b
        // If not equal, we skip incrementing the local
        let mismatch_jump = self.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::SwitchCaseSkipIncrement,
            colon_location.clone(),
        );
        self.write_pop(colon_location.clone());
        self.append_write(WriteAction::WriteOpCodeWithValue {
            op_code: OpCode::Constant,
            value: crate::common::value::Value::Number(1.0),
            location: colon_location.clone(),
        });
        self.append_write(WriteAction::WriteOpCodeWithOffset {
            op_code: OpCode::GetLocal,
            offset: switch_match_counter_index,
            location: colon_location.clone(),
        });
        self.append_write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Add,
            location: colon_location.clone(),
        });
        self.append_write(WriteAction::WriteOpCodeWithOffset {
            op_code: OpCode::SetLocal,
            offset: switch_match_counter_index,
            location: colon_location.clone(),
        });
        // Skip popping the result of e == e_b in the `false` branch
        let match_jump = self.emit_jump(
            OpCode::Jump,
            LabelType::SwitchCaseSkipPop,
            colon_location.clone(),
        );
        mismatch_jump.patch(&mut self.pending_writes);
        self.write_pop(colon_location.clone());
        self.append_write(WriteAction::WriteOpCodeWithOffset {
            op_code: OpCode::GetLocal,
            offset: switch_match_counter_index,
            location: colon_location.clone(),
        });
        match_jump.patch(&mut self.pending_writes);

        // The top of the stack contains the # of matches so far.
        // Check if any branch has matched successfully.
        self.append_write(WriteAction::WriteOpCodeWithValue {
            op_code: OpCode::Constant,
            value: crate::common::value::Value::Number(0.0),
            location: colon_location.clone(),
        });
        self.append_write(WriteAction::WriteNoArgOpCode {
            op_code: OpCode::Greater,
            location: colon_location.clone(),
        });
        let skip_branch_jump = self.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::SwitchCaseSkipBranchStatements,
            colon_location.clone(),
        );
        self.write_pop(colon_location.clone());
        while !self.check(&Token::Case)
            && !self.check(&Token::Default)
            && !self.check(&Token::RightBrace)
        {
            self.statement()?;
        }
        let skip_pop_jump = self.emit_jump(
            OpCode::Jump,
            LabelType::SwitchCaseSkipPop,
            self.prev.location.clone(),
        );
        skip_branch_jump.patch(&mut self.pending_writes);
        let location = self.prev.location.clone();
        self.write_pop(location);
        skip_pop_jump.patch(&mut self.pending_writes);
        Ok(())
    }

    fn default_case(&mut self) -> Result<()> {
        self.consume(Token::Colon, |t| {
            format!(
                "{} Expected ':' after the default switch-case branch, found {:?}",
                t.location, t.token
            )
        })?;
        while !self.check(&Token::RightBrace) {
            self.statement()?;
        }
        Ok(())
    }

    fn break_statement(&mut self) -> Result<()> {
        match self.breakable_stmts().last().cloned() {
            None => Err(Error::MisplacedBreakStatementError {
                location: self.prev.location.clone(),
            }),
            Some(stmt) => {
                let to_pop = self.locals().len() - self.upper_bound_of_depth(stmt.depth);
                for _ in 0..to_pop {
                    self.write_pop(self.prev.location.clone());
                }
                match stmt.statement_type {
                    StatementType::For | StatementType::While => {
                        // The current implementation jump to the end of the statement tied to the loop.
                        // This means that the next operation is Pop, which is expected to drop the expression
                        // generated in the condition clause.
                        // The following push of Nil is to offset the Pop operation.
                        self.pending_writes
                            .push_back(WriteAction::WriteNoArgOpCode {
                                op_code: OpCode::Nil,
                                location: self.prev.location.clone(),
                            });
                    }
                    _ => (),
                }
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

    fn continue_statement(&mut self) -> Result<()> {
        match self.breakable_stmts().last().cloned() {
            Some(stmt)
                if stmt.statement_type == StatementType::For
                    || stmt.statement_type == StatementType::While =>
            {
                let to_pop = self.locals().len() - self.upper_bound_of_depth(stmt.depth);
                for _ in 0..to_pop {
                    self.write_pop(self.prev.location.clone());
                }
                let label_type = match stmt.statement_type {
                    StatementType::While => LabelType::ConditionClause,
                    StatementType::For => LabelType::IncrementClause,
                    StatementType::Switch => {
                        return Err(Error::MisplacedBreakStatementError {
                            location: self.prev.location.clone(),
                        })
                    }
                };
                self.emit_jump(OpCode::Loop, label_type, stmt.location.clone());
                self.consume(Token::Semicolon, |t| {
                    format!(
                        "{} Expected ';' after continue, found {:?}",
                        t.location, t.token
                    )
                })?;
                Ok(())
            }
            _ => Err(Error::MisplacedContinueStatementError {
                location: self.prev.location.clone(),
            }),
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

    fn dot(&mut self, can_assign: bool) -> Result<()> {
        let ident = self.consume(IDENTIFIER_TOKEN, |t| {
            format!(
                "{} Expected a property name after '.', found {:?}",
                t.location, t.token
            )
        })?;
        let id_name = self
            .interned_string_registry
            .intern_string(ident.token.id_name());
        if can_assign && self.next_token_is(&Token::Equal)? {
            self.expression()?;
            self.append_write(WriteAction::WriteOpCodeWithIdentifier {
                op_code: OpCode::SetProperty,
                identifier: id_name,
                location: ident.location.clone(),
            });
        } else if self.next_token_is(&Token::LeftParen)? {
            let arg_count = self.argument_list()?;
            self.append_write(WriteAction::WriteInvoke {
                name: id_name,
                arg_count,
                location: ident.location.clone(),
            });
        } else {
            self.append_write(WriteAction::WriteOpCodeWithIdentifier {
                op_code: OpCode::GetProperty,
                identifier: id_name,
                location: ident.location.clone(),
            });
        }
        Ok(())
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
            NextExpressionType::Call => self.call(),
            NextExpressionType::Dot => self.dot(can_assign),
            NextExpressionType::This => self.this(),
        }
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let var_location = self.prev.location.clone();
        let id = self
            .interned_string_registry
            .intern_string(self.prev.token.id_name());

        enum OffsetOrId {
            Offset(u8),
            Id(InternedString),
        }

        let (set_op, get_op, args) =
            if let Some(v) = self.resolve_local(id.as_ref(), &var_location)? {
                (OpCode::SetLocal, OpCode::GetLocal, OffsetOrId::Offset(v))
            } else if let Some(v) = self.resolve_upvalue(id.as_ref(), &var_location)? {
                (
                    OpCode::SetUpValue,
                    OpCode::GetUpValue,
                    OffsetOrId::Offset(v),
                )
            } else {
                (OpCode::SetGlobal, OpCode::GetGlobal, OffsetOrId::Id(id))
            };

        let location = self.prev.location.clone();
        let instruction = if can_assign && self.next_token_is(&Token::Equal)? {
            self.assignment()?;
            match args {
                OffsetOrId::Offset(offset) => WriteAction::WriteOpCodeWithOffset {
                    op_code: set_op,
                    offset,
                    location,
                },
                OffsetOrId::Id(identifier) => WriteAction::WriteOpCodeWithIdentifier {
                    op_code: set_op,
                    identifier,
                    location,
                },
            }
        } else {
            match args {
                OffsetOrId::Offset(offset) => WriteAction::WriteOpCodeWithOffset {
                    op_code: get_op,
                    offset,
                    location,
                },
                OffsetOrId::Id(identifier) => WriteAction::WriteOpCodeWithIdentifier {
                    op_code: get_op,
                    identifier,
                    location,
                },
            }
        };
        self.pending_writes.push_back(instruction);
        Ok(())
    }

    fn this(&mut self) -> Result<()> {
        if !self.is_class_scope() {
            return Err(Error::ThisReferenceOutsideClassError {
                location: self.prev.location.clone(),
            });
        }
        self.variable(false)
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

    fn call(&mut self) -> Result<()> {
        let call_location = self.prev.location.clone();
        let arg_count = self.argument_list()?;
        self.append_write(WriteAction::WriteOpCodeCall {
            arg_count,
            location: call_location,
        });
        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8> {
        let mut count = 0;
        if !self.check(&Token::RightParen) {
            loop {
                // call assignment() to avoid using the parsing rule for the comma expression
                self.assignment()?;
                if count == u8::MAX {
                    return Err(Error::TooManyFunctionArgumentsError {
                        location: self.prev.location.clone(),
                    });
                }
                count += 1;
                if !self.next_token_is(&Token::Comma)? {
                    break;
                }
            }
        }
        self.consume(Token::RightParen, |t| {
            format!(
                "{} Expected ')' after function arguments, found {:?}",
                t.location, t.token
            )
        })?;
        Ok(count)
    }

    fn number(&mut self) -> Result<()> {
        let v = match &self.prev.token {
            Token::Number(v) => *v,
            _ => unreachable!(),
        };
        self.pending_writes
            .push_back(WriteAction::WriteOpCodeWithValue {
                op_code: OpCode::Constant,
                value: crate::common::value::Value::Number(v),
                location: self.prev.location.clone(),
            });
        Ok(())
    }

    fn string(&mut self) -> Result<()> {
        let s = match &self.prev.token {
            Token::String(s) => s,
            _ => unreachable!(),
        };
        let v = self.interned_string_registry.intern_string(s);
        self.pending_writes
            .push_back(WriteAction::WriteOpCodeWithValue {
                op_code: OpCode::Constant,
                value: crate::common::value::Value::String(v),
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
        self.pending_writes
            .push_back(WriteAction::WriteNoArgOpCode {
                op_code,
                location: self.prev.location.clone(),
            });
        Ok(())
    }

    fn unary(&mut self) -> Result<()> {
        let instruction = WriteAction::WriteNoArgOpCode {
            op_code: match &self.prev.token {
                Token::Minus => OpCode::Negate,
                Token::Bang => OpCode::Not,
                _ => unreachable!(),
            },
            location: self.prev.location.clone(),
        };
        self.parse_precedence(Precedence::Unary)?;
        self.pending_writes.push_back(instruction);
        Ok(())
    }

    fn ternary(&mut self) -> Result<()> {
        let dst_registerer = self.emit_jump(
            OpCode::JumpIfFalse,
            LabelType::EndOfExpression,
            self.prev.location.clone(),
        );
        self.expression()?;
        self.consume(Token::Colon, |t| {
            format!(
                "{} Expected ':' after the true branch of the ternary op, found {:?}",
                t.location, t.token
            )
        })?;
        let jump_after_true = self.emit_jump(
            OpCode::Jump,
            LabelType::EndOfExpression,
            self.prev.location.clone(),
        );
        dst_registerer.patch(&mut self.pending_writes);
        self.parse_precedence(Precedence::Assignment)?;
        jump_after_true.patch(&mut self.pending_writes);
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
            self.pending_writes
                .push_back(WriteAction::WriteNoArgOpCode {
                    op_code,
                    location: location.clone(),
                });
        }
        Ok(())
    }

    fn mark_most_recent_local_initialized(&mut self) {
        self.locals_mut().last_mut().unwrap().depth = self.scope_depth;
    }

    fn is_class_scope(&self) -> bool {
        self.class_nest_depth > 0
    }

    fn function_type(&self) -> FunctionType {
        self.env.function_type.clone()
    }
}
