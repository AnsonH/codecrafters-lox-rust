use miette::Result;

use crate::ast::{
    expression::{Binary, ExprVisitor, Grouping, Identifier, LiteralExpr, Unary},
    UnaryOperator,
};

use super::{object::Object, Evaluator};

impl ExprVisitor for Evaluator {
    type Value = Result<Object>;

    fn visit_binary_expr(&mut self, expr: &Binary) -> Self::Value {
        todo!()
    }

    fn visit_grouping_expr(&mut self, expr: &Grouping) -> Self::Value {
        expr.expression.accept(self)
    }

    fn visit_identifier_expr(&mut self, expr: &Identifier) -> Self::Value {
        todo!()
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Value {
        Ok(Object::from(expr.value.clone()))
    }

    fn visit_unary_expr(&mut self, expr: &Unary) -> Self::Value {
        let right = expr.right.accept(self)?;
        match expr.operator {
            UnaryOperator::LogicalNot => Ok(Object::Boolean(!right.is_truthy())),
            UnaryOperator::UnaryMinus => {
                if let Object::Number(value) = right {
                    Ok(Object::Number(-value))
                } else {
                    todo!("handle operand not number")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    use super::Object::*;
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
        assert("true", &Boolean(true));
        assert("false", &Boolean(false));
        assert("nil", &Nil);
        assert("10", &Number(10.0));
        assert("10.40", &Number(10.4));
        assert(r#""hello world!""#, &String("hello world!".into()));
    }

    #[test]
    fn test_grouping() {
        assert(r#"("hello world!")"#, &String("hello world!".into()));
        assert("(true)", &Boolean(true));
        assert("(10.40)", &Number(10.4));
        assert("((false))", &Boolean(false));
    }

    #[test]
    fn test_unary_expression() {
        assert("-73", &Number(-73.0));
        assert("--73", &Number(73.0));

        assert("!true", &Boolean(false));
        assert("!((false))", &Boolean(true));
        assert("!!false", &Boolean(false));

        // Truthy and falsey values
        assert("!nil", &Boolean(true));
        assert("!false", &Boolean(true));
        assert("!0", &Boolean(false));
        assert("!10.40", &Boolean(false));
        assert(r#"!"""#, &Boolean(false));
        assert(r#"!"hello""#, &Boolean(false));
    }
}
