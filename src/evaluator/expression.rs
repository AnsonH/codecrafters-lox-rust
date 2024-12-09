use miette::Result;

use crate::ast::{
    expression::{Binary, ExprVisitor, Grouping, Identifier, LiteralExpr, Unary},
    BinaryOperator, UnaryOperator,
};

use super::{object::Object, Evaluator};

impl ExprVisitor for Evaluator {
    type Value = Result<Object>;

    fn visit_binary_expr(&mut self, expr: &Binary) -> Self::Value {
        let left = expr.left.accept(self)?;
        let right = expr.right.accept(self)?;

        if let (Object::String(lhs), Object::String(rhs)) = (&left, &right) {
            return Ok(Object::String(format!("{lhs}{rhs}")));
        }

        // TODO: Check for LHS & RHS types
        let left = left.unwrap_number();
        let right = right.unwrap_number();

        match expr.operator {
            BinaryOperator::Add => Ok(Object::Number(left + right)),
            BinaryOperator::Subtract => Ok(Object::Number(left - right)),
            BinaryOperator::Multiply => Ok(Object::Number(left * right)),
            BinaryOperator::Divide => Ok(Object::Number(left / right)),
            BinaryOperator::GreaterThan => Ok(Object::Boolean(left > right)),
            BinaryOperator::GreaterEqualThan => Ok(Object::Boolean(left >= right)),
            BinaryOperator::LessThan => Ok(Object::Boolean(left < right)),
            BinaryOperator::LessEqualThan => Ok(Object::Boolean(left <= right)),
            BinaryOperator::Equal => Ok(Object::Boolean(left == right)),
            BinaryOperator::NotEqual => Ok(Object::Boolean(left != right)),
        }
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

    #[test]
    fn test_arithmetic() {
        assert("1 + 2", &Number(3.0));
        assert("1 - 2", &Number(-1.0));
        assert("1 * 2", &Number(2.0));
        assert("1 / 2", &Number(0.5));
        assert("(1 + 2) / -3.0 + 4 * -(5 + 6.5)", &Number(-47.0));
    }

    #[test]
    fn test_string_concatenation() {
        assert(r#""hello" + " world!""#, &String("hello world!".into()));
    }

    #[test]
    fn test_comparison() {
        assert("1 < 2", &Boolean(true));
        assert("1 <= 2", &Boolean(true));
        assert("1 > 2", &Boolean(false));
        assert("1 >= 2", &Boolean(false));
        assert("(-20 + 70) >= (25 * 2)", &Boolean(true));
    }
}
