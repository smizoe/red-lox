use crate::{expr::Expr, stmt::Stmt};

pub trait Visitor<R> {
    fn visit_expr(&mut self, expr: &Expr) -> R;
    fn visit_stmt(&mut self, stmt: &Stmt) -> R;
}