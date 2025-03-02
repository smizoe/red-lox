#[derive(Debug, Clone)]
pub enum Instruction {
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Less,
    Greater,
    Not,
    Negate,
    Constant(f64),
    Nil,
    Bool(bool),
}
