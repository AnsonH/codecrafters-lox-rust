//! The Abstract Syntax Tree (AST) of Lox.

use std::fmt::Display;

use crate::token::TokenKind;

/// Root AST node that represents the whole program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Stmt>,
}

/// Statement does not produce a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {}

/// Expression produces a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Literal(Literal<'src>),
    /// A grouped expression using parenthesis (e.g. `("foo")`)
    Grouping(Box<Expr<'src>>),
    /// Unary expression (e.g. `-5`, `!true`)
    Unary(UnaryOperator, Box<Expr<'src>>),
    /// Binary expression (e.g. `1 + 2`)
    Binary(Box<Expr<'src>>, BinaryOperator, Box<Expr<'src>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Boolean(bool),
    Nil,
    Number(f64),
    String(&'src str),
}

#[derive(Debug, Clone, Copy, PartialEq, strum::Display)]
pub enum UnaryOperator {
    #[strum(to_string = "!")]
    LogicalNot,
    #[strum(to_string = "-")]
    UnaryMinus,
}

#[derive(Debug, Clone, Copy, PartialEq, strum::Display)]
pub enum BinaryOperator {
    #[strum(to_string = "+")]
    Add,
    #[strum(to_string = "-")]
    Subtract,
    #[strum(to_string = "*")]
    Multiply,
    #[strum(to_string = "/")]
    Divide,
}

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(literal) => write!(f, "{literal}"),
            Expr::Grouping(expression) => write!(f, "(group {expression})"),
            Expr::Unary(operator, right) => write!(f, "({operator} {right})"),
            Expr::Binary(left, operator, right) => write!(f, "({operator} {left} {right})"),
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

impl From<TokenKind> for BinaryOperator {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Plus => BinaryOperator::Add,
            TokenKind::Minus => BinaryOperator::Subtract,
            TokenKind::Star => BinaryOperator::Multiply,
            TokenKind::Slash => BinaryOperator::Divide,
            _ => unreachable!("Expected binary operator, got {kind}"),
        }
    }
}
