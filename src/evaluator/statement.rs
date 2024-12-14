use miette::Result;

use crate::ast::statement::{PrintStmt, StmtVisitor};

use super::Evaluator;

impl StmtVisitor for Evaluator {
    type Value = Result<()>;

    fn visit_print_stmt(&mut self, expr: &PrintStmt) -> Self::Value {
        println!("{}", expr.expression.accept(self)?);
        Ok(())
    }
}
