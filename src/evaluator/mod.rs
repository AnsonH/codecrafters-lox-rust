//! Evaluator for the Lox language.

mod environment;
mod expression;
mod object;
mod statement;

use std::{cell::RefCell, rc::Rc};

use environment::Environment;
use miette::Result;
use object::Object;

use crate::ast::{Expr, Program};

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

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

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}
