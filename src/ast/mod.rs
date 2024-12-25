//! The Abstract Syntax Tree (AST) of Lox.

pub mod expression;
pub mod operator;
pub mod printer;
pub mod statement;
pub mod utils;

pub use self::expression::*;
pub use self::operator::*;
pub use self::statement::*;

//////////////////////////////////////////////

use crate::span::Span;

/// Root AST node that represents the whole program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Stmt>,
    pub span: Span,
}
