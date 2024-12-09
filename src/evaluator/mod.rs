//! Evaluator for the Lox language.

mod expression;
mod object;

use miette::Result;
use object::Object;

use crate::ast::Expr;

pub struct Evaluator;

impl Evaluator {
    /// Public entry point for evaluating an expression.
    pub fn evaluate_expression(&mut self, expr: &Expr) -> Result<Object> {
        expr.accept(self)
    }
}
