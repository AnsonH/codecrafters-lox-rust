use miette::Result;

use crate::ast::statement::*;

use super::{object::Object, Evaluator};

impl StmtVisitor for Evaluator {
    type Value = Result<()>;

    fn visit_expression_stmt(&mut self, expr: &ExpressionStatement) -> Self::Value {
        expr.expression.accept(self)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &PrintStatement) -> Self::Value {
        println!("{}", expr.expression.accept(self)?);
        Ok(())
    }

    fn visit_var_stmt(&mut self, expr: &VarStatement) -> Self::Value {
        let value = expr
            .initializer
            .as_ref()
            .map_or(Ok(Object::Nil), |expr| expr.accept(self))?;

        self.env.borrow_mut().define(expr.ident.name.into(), value);
        Ok(())
    }
}
