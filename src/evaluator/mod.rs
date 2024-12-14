//! Evaluator for the Lox language.

mod expression;
mod object;
mod statement;

use miette::Result;
use object::Object;

use crate::ast::{Expr, Program};

pub struct Evaluator;

impl Evaluator {
    /// Entry point to evaluate a program.
    pub fn evaluate_program(&mut self, program: &Program) -> Result<()> {
        for stmt in &program.body {
            stmt.accept(self)?;
        }
        Ok(())
    }

    /// Entry point to evaluate an expression.
    pub fn evaluate_expression(&mut self, expr: &Expr) -> Result<Object> {
        expr.accept(self)
    }
}
