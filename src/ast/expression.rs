use std::fmt::Display;

use super::operator::{BinaryOperator, UnaryOperator};

/// Expression produces a value.
///
/// # Optimization
///
/// This enum's size is optimized by `Box`-ing all enum variants.
/// See the [oxc AST guide](https://oxc.rs/docs/learn/parser_in_rust/ast.html#enum-size).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Binary(Box<Binary<'src>>),
    Grouping(Box<Grouping<'src>>),
    Identifier(Box<Identifier<'src>>),
    Literal(Box<LiteralExpr<'src>>),
    Unary(Box<Unary<'src>>),
}

/// Binary expression (e.g. `1 + 2`)
#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'src> {
    pub left: Expr<'src>,
    pub operator: BinaryOperator,
    pub right: Expr<'src>,
}

/// Grouped expression using parenthesis (e.g. `("foo")`)
#[derive(Debug, Clone, PartialEq)]
pub struct Grouping<'src> {
    pub expression: Expr<'src>,
}

/// Identifier (e.g. variable, function name)
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier<'src> {
    pub name: &'src str,
}

/// Literal expression
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr<'src> {
    pub value: Literal<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Boolean(bool),
    Nil,
    Number(f64),
    String(&'src str),
}

/// Unary expression (e.g. `-5`)
#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'src> {
    pub operator: UnaryOperator,
    pub right: Expr<'src>,
}

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(b) => {
                write!(f, "({} {} {})", b.operator, b.left, b.right)
            }
            Expr::Identifier(i) => write!(f, "{}", i.name),
            Expr::Grouping(g) => write!(f, "(group {})", g.expression),
            Expr::Literal(l) => write!(f, "{}", l.value),
            Expr::Unary(u) => write!(f, "({} {})", u.operator, u.right),
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
