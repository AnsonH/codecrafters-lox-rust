//! The Abstract Syntax Tree (AST) of Lox.

pub mod expression;
pub mod operator;
pub mod printer;
pub mod statement;
pub mod utils;

// TODO: `pub use` more symbols
pub use expression::{Expr, Literal};
pub use operator::{BinaryOperator, UnaryOperator};
pub use statement::Stmt;

use crate::span::Span;

/// Root AST node that represents the whole program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'src> {
    pub body: Vec<Stmt<'src>>,
    pub span: Span,
}
