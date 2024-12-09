use miette::Result;

use crate::ast::expression::{Binary, ExprVisitor, Grouping, Identifier, LiteralExpr, Unary};

use super::{object::Object, Evaluator};

impl ExprVisitor for Evaluator {
    type Value = Result<Object>;

    fn visit_binary_expr(&mut self, expr: &Binary) -> Self::Value {
        todo!()
    }

    fn visit_grouping_expr(&mut self, expr: &Grouping) -> Self::Value {
        todo!()
    }

    fn visit_identifier_expr(&mut self, expr: &Identifier) -> Self::Value {
        todo!()
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Value {
        Ok(Object::from(expr.value.clone()))
    }

    fn visit_unary_expr(&mut self, expr: &Unary) -> Self::Value {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert(input: &'static str, expected: &Object) {
        let parser = Parser::new(input);
        let expr = parser.parse_expression().unwrap_or_else(|report| {
            panic!(
                "Encountered error while parsing\n{:?}",
                report.with_source_code(input)
            )
        });
        let mut evaluator = Evaluator;
        match evaluator.evaluate_expression(&expr) {
            Ok(obj) => assert_eq!(obj, *expected),
            Err(report) => panic!(
                "Encountered error while evaluation\n{:?}",
                report.with_source_code(input)
            ),
        }
    }

    #[test]
    fn test_literal() {
        assert("true", &Object::Boolean(true));
        assert("false", &Object::Boolean(false));
        assert("nil", &Object::Nil);
    }
}
