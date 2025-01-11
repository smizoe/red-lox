use crate::expr::Expr;

trait Visitor<R> {
    fn visit_expr(&mut self, expr: &Expr) -> R;
}