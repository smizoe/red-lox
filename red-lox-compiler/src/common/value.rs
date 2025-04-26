use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::common::function::Closure;
use crate::common::function::NativeFunction;
use crate::common::InternedString;

use super::class::{Class, Instance};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(InternedString),
    // not Rc<Closure> to create an empty upvalues vector upon cloning.
    Closure(Box<Closure>),
    NativeFunction(Rc<NativeFunction>),
    Class(Rc<Class>),
    Instance(Box<Instance>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, ""),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(s) => write!(f, "{}", s),
            Value::Closure(c) => write!(f, "{}", c.fun()),
            Value::NativeFunction(_) => write!(f, "<native fn>"),
            Value::Class(c) => write!(f, "{}", c),
            Value::Instance(i) => write!(f, "{}", i),
        }
    }
}

impl Value {
    pub fn is_number(&self) -> bool {
        use Value::*;
        match self {
            Number(_) => true,
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

    pub fn is_falsy(&self) -> bool {
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
            Closure(_) => "closure",
            NativeFunction(_) => "native function",
            Class(_) => "class",
            Instance(_) => "instance",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn test_value_size_is_twice_that_of_u64() {
        assert_eq!(size_of::<Value>(), size_of::<u64>() * 2);
    }
}
