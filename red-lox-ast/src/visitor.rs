use crate::expr::Expr;

pub trait Visitor<R> {
    fn visit_expr(&mut self, expr: &Expr) -> R;
}