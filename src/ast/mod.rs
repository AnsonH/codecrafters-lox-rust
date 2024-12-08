//! The Abstract Syntax Tree (AST) of Lox.

pub mod expression;
pub mod operator;
pub mod printer;
pub mod statement;

pub use expression::{Expr, Literal};
pub use operator::{BinaryOperator, UnaryOperator};
pub use statement::Stmt;

/// Root AST node that represents the whole program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Stmt>,
}
