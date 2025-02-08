use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use red_lox_ast::scanner::{
    Location, TokenWithLocation, SUPER, SUPER_LOCATION, THIS, THIS_LOCATION,
};

use crate::Interpreter;

pub struct Resolver<'a, 'b, 'c> {
    interpreter: &'a mut Interpreter<'b, 'c>,
    scopes: Vec<HashMap<String, VariableInfo>>,
    function_type: FunctionType,
    class_type: ClassType,
    errors: Vec<Error>,
}

struct VariableInfo {
    state: VariableState,
    location: Location,
    function_type: FunctionType,
}

#[derive(PartialEq)]
enum VariableState {
    Uninitialized,
    Initialized,
    Used,
}

#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(PartialEq, Clone, Copy)]
enum ClassType {
    None,
    Class,
    Subclass,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{} Cannot read a local variable in its own initializer.", .0.location)]
    InvalidInitializationError(TokenWithLocation),
    #[error("{} Already a variable with name '{}' in this scope.", .0.location, .0.token.id_name())]
    DuplicateVariableDeclarationError(TokenWithLocation),
    #[error("{} Cannot return from top-level code.", .0.location)]
    TopLevelReturnError(TokenWithLocation),
    #[error("{location} Varable '{name}' is defined but unused.")]
    UnusedLocalVariableError { name: String, location: Location },
    #[error("{} Keyword 'this' appeared outside of a class", .0)]
    ThisKeywordOutsideClassContextError(Location),
    #[error("{} Cannot return a value from an initializer.", .0)]
    ReturnValueFromInitializerError(Location),
    #[error("{} A class cannot inherit itself.", .0)]
    InvalidInheritanceError(Location),
    #[error("{} Cannot use super outside of a class", .0)]
    SuperKeywordOutsideClassContextError(Location),
    #[error("{} Cannot use super in a class with no superclass", .0)]
    SuperKeywordInClassWithoutSuperclassError(Location),
}

struct ScopeGuard<'a, 'b, 'c, 'd> {
    resolver: &'a mut Resolver<'b, 'c, 'd>,
}

impl<'a, 'b, 'c, 'd> ScopeGuard<'a, 'b, 'c, 'd> {
    fn new(resolver: &'a mut Resolver<'b, 'c, 'd>) -> Self {
        resolver.scopes.push(HashMap::new());
        Self { resolver }
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
        if let Some(scope) = self.resolver.scopes.pop() {
            for (
                k,
                VariableInfo {
                    state,
                    location,
                    function_type,
                },
            ) in scope.into_iter()
            {
                if state != VariableState::Used && function_type != FunctionType::Method {
                    self.errors
                        .push(Error::UnusedLocalVariableError { name: k, location });
                }
            }
        }
    }
}

struct FunctionTypeGuard<'a, 'b, 'c, 'd> {
    resolver: &'a mut Resolver<'b, 'c, 'd>,
    prev_function_type: FunctionType,
}

impl<'a, 'b, 'c, 'd> FunctionTypeGuard<'a, 'b, 'c, 'd> {
    fn new(resolver: &'a mut Resolver<'b, 'c, 'd>, function_type: FunctionType) -> Self {
        let prev_function_type = resolver.function_type;
        resolver.function_type = function_type;
        Self {
            resolver,
            prev_function_type,
        }
    }
}

impl<'a, 'b, 'c, 'd> Deref for FunctionTypeGuard<'a, 'b, 'c, 'd> {
    type Target = Resolver<'b, 'c, 'd>;

    fn deref(&self) -> &Self::Target {
        self.resolver
    }
}

impl<'a, 'b, 'c, 'd> DerefMut for FunctionTypeGuard<'a, 'b, 'c, 'd> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.resolver
    }
}

impl<'a, 'b, 'c, 'd> Drop for FunctionTypeGuard<'a, 'b, 'c, 'd> {
    fn drop(&mut self) {
        self.resolver.function_type = self.prev_function_type;
    }
}

struct ClassTypeGuard<'a, 'b, 'c, 'd> {
    resolver: &'a mut Resolver<'b, 'c, 'd>,
    prev_class_type: ClassType,
}

impl<'a, 'b, 'c, 'd> ClassTypeGuard<'a, 'b, 'c, 'd> {
    fn new(resolver: &'a mut Resolver<'b, 'c, 'd>, class_type: ClassType) -> Self {
        let prev_class_type = resolver.class_type;
        resolver.class_type = class_type;
        Self {
            resolver,
            prev_class_type,
        }
    }
}

impl<'a, 'b, 'c, 'd> Deref for ClassTypeGuard<'a, 'b, 'c, 'd> {
    type Target = Resolver<'b, 'c, 'd>;

    fn deref(&self) -> &Self::Target {
        self.resolver
    }
}

impl<'a, 'b, 'c, 'd> DerefMut for ClassTypeGuard<'a, 'b, 'c, 'd> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.resolver
    }
}

impl<'a, 'b, 'c, 'd> Drop for ClassTypeGuard<'a, 'b, 'c, 'd> {
    fn drop(&mut self) {
        self.resolver.class_type = self.prev_class_type;
    }
}

impl<'a, 'b, 'c> Resolver<'a, 'b, 'c> {
    pub fn new(interpreter: &'a mut Interpreter<'b, 'c>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            function_type: FunctionType::None,
            class_type: ClassType::None,
            errors: Vec::new(),
        }
    }

    pub fn resolve(&mut self, stmts: &Vec<Box<red_lox_ast::stmt::Stmt>>) -> Result<(), Vec<Error>> {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
        let errors = std::mem::take(&mut self.errors);
        if errors.len() == 0 {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn add_kwd_to_scope(&mut self, kwd: &str, location: Location) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                kwd.to_string(),
                VariableInfo {
                    state: VariableState::Used,
                    location,
                    function_type: FunctionType::None,
                },
            );
        }
    }

    fn begin_scope(&mut self) -> ScopeGuard<'_, 'a, 'b, 'c> {
        ScopeGuard::new(self)
    }

    fn begin_class(&mut self, class_type: ClassType) -> ClassTypeGuard<'_, 'a, 'b, 'c> {
        ClassTypeGuard::new(self, class_type)
    }

    fn begin_function(&mut self, function_type: FunctionType) -> FunctionTypeGuard<'_, 'a, 'b, 'c> {
        FunctionTypeGuard::new(self, function_type)
    }

    fn declare(&mut self, token: &TokenWithLocation) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(token.token.id_name()) {
                self.errors
                    .push(Error::DuplicateVariableDeclarationError(token.clone()));
                return;
            }
            scope.insert(
                token.token.id_name().to_string(),
                VariableInfo {
                    state: VariableState::Uninitialized,
                    location: token.location.clone(),
                    function_type: self.function_type,
                },
            );
        }
    }

    fn define(&mut self, token: &TokenWithLocation) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.entry(token.token.id_name().to_string()).and_modify(
                |VariableInfo { state, .. }| {
                    *state = VariableState::Initialized;
                },
            );
        }
    }

    fn resolve_function(
        &mut self,
        params: &Vec<TokenWithLocation>,
        body: &Vec<Box<red_lox_ast::stmt::Stmt>>,
    ) {
        let mut guard = self.begin_scope();
        for param in params {
            guard.declare(&param);
            guard.define(&param);
        }
        for stmt in body {
            guard.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &red_lox_ast::stmt::Stmt) {
        match stmt {
            red_lox_ast::stmt::Stmt::Expression(expr) => self.resolve_expr(expr),
            red_lox_ast::stmt::Stmt::Print(expr) => self.resolve_expr(expr),
            red_lox_ast::stmt::Stmt::Var(token, expr) => {
                self.declare(token);
                if let Some(expr) = expr.as_ref() {
                    self.resolve_expr(expr);
                }
                self.define(token);
            }
            red_lox_ast::stmt::Stmt::Block(stmts) => {
                let mut guard = self.begin_scope();
                for stmt in stmts {
                    guard.resolve_stmt(stmt);
                }
            }
            red_lox_ast::stmt::Stmt::Function { name, params, body } => {
                self.declare(&name);
                self.define(&name);
                self.begin_function(FunctionType::Function)
                    .resolve_function(params, body);
            }
            red_lox_ast::stmt::Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                let mut class_type_guard = self.begin_class(ClassType::Class);
                class_type_guard.declare(name);
                class_type_guard.define(name);
                match superclass {
                    Some(expr) => {
                        match expr.as_ref() {
                            red_lox_ast::expr::Expr::Variable(t) if t.token == name.token => {
                                class_type_guard
                                    .errors
                                    .push(Error::InvalidInheritanceError(t.location.clone()));
                            }
                            _ => (),
                        }
                        let mut subclass_type_guard =
                            class_type_guard.begin_class(ClassType::Subclass);
                        subclass_type_guard.resolve_expr(expr);
                        let mut scope_with_super = subclass_type_guard.begin_scope();
                        scope_with_super.add_kwd_to_scope(SUPER, SUPER_LOCATION.clone());
                        scope_with_super.resolve_methods(methods);
                    }
                    None => {
                        class_type_guard.resolve_methods(methods);
                    }
                }
            }
            red_lox_ast::stmt::Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(&condition);
                self.resolve_stmt(&then_branch);
                if let Some(stmt) = else_branch.as_ref() {
                    self.resolve_stmt(&stmt);
                }
            }
            red_lox_ast::stmt::Stmt::While { condition, body } => {
                self.resolve_expr(&condition);
                self.resolve_stmt(&body);
            }
            red_lox_ast::stmt::Stmt::Break => (),
            red_lox_ast::stmt::Stmt::Return(token, expr) => {
                if self.function_type == FunctionType::None {
                    self.errors.push(Error::TopLevelReturnError(token.clone()));
                    return;
                }
                if let Some(e) = expr.as_ref() {
                    if self.function_type == FunctionType::Initializer {
                        self.errors.push(Error::ReturnValueFromInitializerError(
                            token.location.clone(),
                        ));
                    }
                    self.resolve_expr(e);
                }
            }
        }
    }

    // Called only in Class { .. } branch of resolve_stmt
    fn resolve_methods(&mut self, methods: &Vec<Box<red_lox_ast::stmt::Stmt>>) {
        let mut scope_guard = self.begin_scope();
        scope_guard.add_kwd_to_scope(THIS, THIS_LOCATION);
        for method in methods {
            match method.as_ref() {
                red_lox_ast::stmt::Stmt::Function { name, params, body } => {
                    let method_type = if name.token.id_name() == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    scope_guard
                        .begin_function(method_type)
                        .resolve_function(params, body);
                }
                _ => unreachable!(),
            };
        }
    }

    fn resolve_expr(&mut self, expr: &red_lox_ast::expr::Expr) {
        match expr {
            red_lox_ast::expr::Expr::ExprSeries(exprs) => {
                for expr in exprs {
                    self.resolve_expr(&expr);
                }
            }
            red_lox_ast::expr::Expr::Ternary { cond, left, right } => {
                self.resolve_expr(&cond);
                self.resolve_expr(&left);
                self.resolve_expr(&right);
            }
            red_lox_ast::expr::Expr::Binary {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(&left);
                self.resolve_expr(&right);
            }
            red_lox_ast::expr::Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                self.resolve_expr(&callee);
                for arg in arguments {
                    self.resolve_expr(&arg);
                }
            }
            red_lox_ast::expr::Expr::Logical {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(&left);
                self.resolve_expr(&right);
            }
            red_lox_ast::expr::Expr::Grouping(expr, _) => self.resolve_expr(expr),
            red_lox_ast::expr::Expr::Unary { operator: _, right } => self.resolve_expr(&right),
            red_lox_ast::expr::Expr::Variable(token) => {
                if let Some(scope) = self.scopes.last() {
                    if scope
                        .get(token.token.id_name())
                        .map(|VariableInfo { state, .. }| state)
                        .unwrap_or(&VariableState::Initialized)
                        == &VariableState::Uninitialized
                    {
                        self.errors
                            .push(Error::InvalidInitializationError(token.clone()));
                        return;
                    }
                    self.resolve_local(token);
                }
            }
            red_lox_ast::expr::Expr::Assign { name, expr } => {
                self.resolve_expr(expr);
                self.resolve_local(name);
            }
            red_lox_ast::expr::Expr::Get { expr, .. } => {
                self.resolve_expr(expr);
            }
            red_lox_ast::expr::Expr::Set { lhs, name: _, rhs } => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            red_lox_ast::expr::Expr::This(t) => {
                if self.class_type == ClassType::None {
                    self.errors.push(Error::ThisKeywordOutsideClassContextError(
                        t.location.clone(),
                    ));
                    return;
                }
                self.resolve_local(t);
            }
            red_lox_ast::expr::Expr::Super { keyword, .. } => {
                match self.class_type {
                    ClassType::None => {
                        self.errors
                            .push(Error::SuperKeywordOutsideClassContextError(
                                keyword.location.clone(),
                            ))
                    }
                    ClassType::Class => {
                        self.errors
                            .push(Error::SuperKeywordInClassWithoutSuperclassError(
                                keyword.location.clone(),
                            ))
                    }
                    ClassType::Subclass => (),
                }
                self.resolve_local(keyword);
            }
            _ => (),
        }
    }

    fn resolve_local(&mut self, token: &TokenWithLocation) {
        for i in (0..self.scopes.len()).rev() {
            if let Some(VariableInfo { state, .. }) = self.scopes[i].get_mut(token.token.id_name())
            {
                *state = VariableState::Used;
                self.interpreter
                    .resolve(token.location.clone(), self.scopes.len() - 1 - i);
                return;
            }
        }
    }
}
