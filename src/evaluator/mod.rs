//! Evaluator for the Lox language.

mod environment;
mod expression;
mod object;
mod statement;

use std::{cell::RefCell, rc::Rc};

use environment::Environment;
use miette::Result;
use object::Object;

use crate::ast::{Expr, Program, Stmt};

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

    /// Executes a block of statements under a new [Environment] of `env_new`.
    ///
    /// If evaluating a statement has `Err`, the block evaluation early exits and
    /// returns with that `Err`.
    fn execute_block(
        &mut self,
        statements: &[Stmt],
        env_new: Rc<RefCell<Environment>>,
    ) -> Result<()> {
        let env_previous = Rc::clone(&self.env);

        self.env = env_new; // Temporarily set to new environment
        let result = statements.iter().try_for_each(|stmt| stmt.accept(self));
        self.env = env_previous; // Restore back to old environment

        result
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}
