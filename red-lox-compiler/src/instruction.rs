#[derive(Debug, Clone)]
pub enum Instruction {
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Constant(f64),
}
