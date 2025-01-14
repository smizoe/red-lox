use crate::{expr::Expr, stmt::Stmt};

pub trait Visitor<R> {
    fn visit_expr(&self, expr: &Expr) -> R;

    fn visit_stmt(&self, stmt: &Stmt) -> R;
}