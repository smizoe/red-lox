use std::rc::Rc;


#[derive(Debug, Clone)]
pub enum Instruction {
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Pop,
    Equal,
    Less,
    Greater,
    Not,
    Comma,
    Negate,
    Print,
    Constant(f64),
    Nil,
    Bool(bool),
    String(Rc<String>),
}
