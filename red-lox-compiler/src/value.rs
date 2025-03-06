use std::{fmt::{Display, Formatter}, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<String>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

impl Value {
    pub fn is_nil(&self) -> bool {
        use Value::*;
        match self {
            Nil => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        use Value::*;
        match self {
            Number(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        use Value::*;
        match self {
            Bool(_) => true,
            _ => false,
        }
    }

    pub fn to_number(self) -> f64 {
        use Value::*;
        match self {
            Number(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn is_falsy(self) -> bool {
        use Value::*;
        match self {
            Nil => true,
            Bool(false) => true,
            _ => false,
        }
    }

    pub fn to_type_str(&self) -> &'static str {
        use Value::*;
        match self {
            Nil => "nil",
            Bool(_) => "boolean",
            Number(_) => "number",
            String(_) => "string",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn test_value_size_is_that_of_usize() {
        assert_eq!(size_of::<Value>(), size_of::<usize>() * 2);
    }
}
