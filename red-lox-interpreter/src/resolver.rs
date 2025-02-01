use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use red_lox_ast::scanner::TokenWithLocation;

use crate::Interpreter;

pub struct Resolver<'a, 'b, 'c> {
    interpreter: &'a mut Interpreter<'b, 'c>,
    scopes: Vec<HashMap<String, bool>>,
    function_type: FunctionType,
}

#[derive(PartialEq)]
enum FunctionType {
    None,
    Function,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{} Cannot read local variable {:?} in its own initializer.", .0.location, .0.token)]
    InvalidInitializationError(TokenWithLocation),
    #[error("{} Already a variable with name {} in this scope.", .0.location, .0.token.id_name())]
    DuplicateVariableDeclarationError(TokenWithLocation),
    #[error("{} Cannot return from top-level code.", .0.location)]
    TopLevelReturnError(TokenWithLocation),
}

struct ScopeGuard<'a, 'b, 'c, 'd> {
    resolver: &'a mut Resolver<'b, 'c, 'd>,
    prev_function_type: Option<FunctionType>,
}

impl<'a, 'b, 'c, 'd> ScopeGuard<'a, 'b, 'c, 'd> {
    fn new(
        resolver: &'a mut Resolver<'b, 'c, 'd>,
        mut function_type: Option<FunctionType>,
    ) -> Self {
        resolver.scopes.push(HashMap::new());
        if let Some(ft) = function_type.as_mut() {
            std::mem::swap(&mut resolver.function_type, ft);
        }
        Self {
            resolver,
            prev_function_type: function_type,
        }
    }
}

impl<'a, 'b, 'c, 'd> Deref for ScopeGuard<'a, 'b, 'c, 'd> {
    type Target = Resolver<'b, 'c, 'd>;

    fn deref(&self) -> &Self::Target {
        self.resolver
    }
}

impl<'a, 'b, 'c, 'd> DerefMut for ScopeGuard<'a, 'b, 'c, 'd> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.resolver
    }
}

impl<'a, 'b, 'c, 'd> Drop for ScopeGuard<'a, 'b, 'c, 'd> {
    fn drop(&mut self) {
        self.resolver.scopes.pop();
        if let Some(ft) = self.prev_function_type.as_mut() {
            std::mem::swap(&mut self.resolver.function_type, ft);
        }
    }
}

impl<'a, 'b, 'c> Resolver<'a, 'b, 'c> {
    pub fn new(interpreter: &'a mut Interpreter<'b, 'c>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            function_type: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &Vec<Box<red_lox_ast::stmt::Stmt>>) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();
        for stmt in stmts {
            if let Err(e) = self.resolve_stmt(stmt) {
                errors.push(e);
            }
        }
        if errors.len() == 0 {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn begin_scope(&mut self) -> ScopeGuard<'_, 'a, 'b, 'c> {
        ScopeGuard::new(self, None)
    }

    fn begin_function_scope(&mut self) -> ScopeGuard<'_, 'a, 'b, 'c> {
        ScopeGuard::new(self, Some(FunctionType::Function))
    }

    fn declare(&mut self, token: &TokenWithLocation) -> Result<(), Error> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(token.token.id_name()) {
                return Err(Error::DuplicateVariableDeclarationError(token.clone()));
            }
            scope.insert(token.token.id_name().to_string(), false);
        }
        Ok(())
    }

    fn define(&mut self, token: &TokenWithLocation) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(token.token.id_name().to_string(), true);
        }
    }

    fn resolve_stmt(&mut self, stmt: &red_lox_ast::stmt::Stmt) -> Result<(), Error> {
        match stmt {
            red_lox_ast::stmt::Stmt::Expression(expr) => self.resolve_expr(expr),
            red_lox_ast::stmt::Stmt::Print(expr) => self.resolve_expr(expr),
            red_lox_ast::stmt::Stmt::Var(token, expr) => {
                self.declare(token)?;
                if let Some(expr) = expr.as_ref() {
                    self.resolve_expr(expr)?;
                }
                self.define(token);
                Ok(())
            }
            red_lox_ast::stmt::Stmt::Block(stmts) => {
                let mut guard = self.begin_scope();
                for stmt in stmts {
                    guard.resolve_stmt(stmt)?;
                }
                Ok(())
            }
            red_lox_ast::stmt::Stmt::Function { name, params, body } => {
                self.declare(&name)?;
                self.define(&name);
                let mut guard = self.begin_function_scope();
                for param in params {
                    guard.declare(&param)?;
                    guard.define(&param);
                }
                for stmt in body {
                    guard.resolve_stmt(stmt)?;
                }
                Ok(())
            }
            red_lox_ast::stmt::Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(&condition)?;
                self.resolve_stmt(&then_branch)?;
                if let Some(stmt) = else_branch.as_ref() {
                    self.resolve_stmt(&stmt)?;
                }
                Ok(())
            }
            red_lox_ast::stmt::Stmt::While { condition, body } => {
                self.resolve_expr(&condition)?;
                self.resolve_stmt(&body)
            }
            red_lox_ast::stmt::Stmt::Break => Ok(()),
            red_lox_ast::stmt::Stmt::Return(token, expr) => {
                if self.function_type == FunctionType::None {
                    return Err(Error::TopLevelReturnError(token.clone()));
                }
                self.resolve_expr(expr)
            }
        }
    }

    fn resolve_expr(&mut self, expr: &red_lox_ast::expr::Expr) -> Result<(), Error> {
        match expr {
            red_lox_ast::expr::Expr::ExprSeries(exprs) => {
                for expr in exprs {
                    self.resolve_expr(&expr)?;
                }
                Ok(())
            }
            red_lox_ast::expr::Expr::Ternary { cond, left, right } => {
                self.resolve_expr(&cond)?;
                self.resolve_expr(&left)?;
                self.resolve_expr(&right)
            }
            red_lox_ast::expr::Expr::Binary {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(&left)?;
                self.resolve_expr(&right)
            }
            red_lox_ast::expr::Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                self.resolve_expr(&callee)?;
                for arg in arguments {
                    self.resolve_expr(&arg)?;
                }
                Ok(())
            }
            red_lox_ast::expr::Expr::Logical {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(&left)?;
                self.resolve_expr(&right)
            }
            red_lox_ast::expr::Expr::Grouping(expr, _) => self.resolve_expr(expr),
            red_lox_ast::expr::Expr::Unary { operator: _, right } => self.resolve_expr(&right),
            red_lox_ast::expr::Expr::Variable(token) => {
                if let Some(scope) = self.scopes.last() {
                    if !scope.get(token.token.id_name()).cloned().unwrap_or(true) {
                        return Err(Error::InvalidInitializationError(token.clone()));
                    }
                    self.resolve_local(token);
                }
                Ok(())
            }
            red_lox_ast::expr::Expr::Assign { name, expr } => {
                self.resolve_expr(expr)?;
                self.resolve_local(name);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn resolve_local(&mut self, token: &TokenWithLocation) {
        for i in (0..self.scopes.len()).rev() {
            if let Some(_) = self.scopes[i].get(token.token.id_name()) {
                self.interpreter
                    .resolve(token.location.clone(), self.scopes.len() - 1 - i);
                return;
            }
        }
    }
}
