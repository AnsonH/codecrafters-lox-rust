use std::fmt::Display;

use super::operator::{BinaryOperator, UnaryOperator};

/// Expression produces a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    /// Binary expression (e.g. `1 + 2`)
    Binary(Box<Expr<'src>>, BinaryOperator, Box<Expr<'src>>),
    /// A grouped expression using parenthesis (e.g. `("foo")`)
    Grouping(Box<Expr<'src>>),
    Identifier(&'src str),
    Literal(Literal<'src>),
    /// Unary expression (e.g. `-5`, `!true`)
    Unary(UnaryOperator, Box<Expr<'src>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Boolean(bool),
    Nil,
    Number(f64),
    String(&'src str),
}

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, operator, right) => write!(f, "({operator} {left} {right})"),
            Expr::Identifier(name) => write!(f, "{name}"),
            Expr::Grouping(expression) => write!(f, "(group {expression})"),
            Expr::Literal(literal) => write!(f, "{literal}"),
            Expr::Unary(operator, right) => write!(f, "({operator} {right})"),
        }
    }
}

impl<'src> Display for Literal<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Nil => write!(f, "nil"),
            Literal::Number(value) => {
                if value.fract() == 0_f64 {
                    // Tests requires integers to be print as N.0
                    write!(f, "{value}.0")
                } else {
                    write!(f, "{value}")
                }
            }
            Literal::String(s) => write!(f, "{s}"),
        }
    }
}
