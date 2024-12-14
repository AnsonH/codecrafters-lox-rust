use miette::Result;

use crate::ast::statement::*;

use super::Evaluator;

impl StmtVisitor for Evaluator {
    type Value = Result<()>;

    fn visit_expression_stmt(&mut self, expr: &ExpressionStmt) -> Self::Value {
        expr.expression.accept(self)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &PrintStmt) -> Self::Value {
        println!("{}", expr.expression.accept(self)?);
        Ok(())
    }
}