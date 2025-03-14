use crate::interned_string::InternedString;


#[derive(Debug, Clone)]
pub(crate) enum Instruction {
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Pop,
    GetGlobal(InternedString),
    DefineGlobal(InternedString),
    SetGlobal(InternedString),
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
    String(InternedString),
}
