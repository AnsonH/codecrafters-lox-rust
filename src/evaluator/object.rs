use std::fmt::Display;

use crate::ast::Literal;

/// Represents a value of the Lox language.
///
/// This is inspired by Java where every value has a base `Object` class.
#[derive(Debug, Clone, Default)]
pub enum Object {
    Boolean(bool),
    #[default]
    Nil,
    Number(f64),
    String(String),
}

impl Object {
    #[inline]
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Object::Boolean(false) | Object::Nil)
    }

    pub(crate) fn unwrap_number(self) -> f64 {
        match self {
            Object::Number(value) => value,
            obj => panic!("called `Object::unwrap_number()` on an non-number Object: '{obj:?}'"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Nil => write!(f, "nil"),
            Object::Boolean(b) => write!(f, "{b}"),
            Object::Number(n) => write!(f, "{n}"),
            Object::String(s) => write!(f, "{s}"),
        }
    }
}

impl From<Literal<'_>> for Object {
    fn from(value: Literal<'_>) -> Self {
        match value {
            Literal::Boolean(b) => Object::Boolean(b),
            Literal::Nil => Object::Nil,
            Literal::Number(n) => Object::Number(n),
            Literal::String(s) => Object::String(s.to_string()),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        use Object::*;
        match (self, other) {
            (Boolean(left), Boolean(right)) => left == right,
            (Nil, Nil) => true,
            (Number(left), Number(right)) => left == right,
            (String(left), String(right)) => left == right,
            _ => false,
        }
    }
}
