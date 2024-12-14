use miette::Result;

use crate::{
    ast::{
        expression::{Binary, ExprVisitor, Grouping, Identifier, LiteralExpr, Unary},
        BinaryOperator, UnaryOperator,
    },
    error::RuntimeError,
};

use super::{object::Object, Evaluator};

impl ExprVisitor for Evaluator {
    type Value = Result<Object>;

    fn visit_binary_expr(&mut self, expr: &Binary) -> Self::Value {
        use BinaryOperator::*;

        let left = expr.left.accept(self)?;
        let right = expr.right.accept(self)?;

        match expr.operator {
            Equal => return Ok(Object::Boolean(left == right)),
            NotEqual => return Ok(Object::Boolean(left != right)),
            _ => (),
        }

        if expr.operator == Add {
            match (&left, &right) {
                (Object::String(lhs), Object::String(rhs)) => {
                    return Ok(Object::String(format!("{lhs}{rhs}")));
                }
                (Object::Number(lhs), Object::Number(rhs)) => return Ok(Object::Number(lhs + rhs)),
                _ => {
                    return Err(RuntimeError::PlusOperandError {
                        span: expr.span.into(),
                    }
                    .into())
                }
            }
        }

        if !matches!((&left, &right), (Object::Number(_), Object::Number(_))) {
            return Err(RuntimeError::InfixNonNumberOperandsError {
                span: expr.span.into(),
            }
            .into());
        }
        let left = left.unwrap_number();
        let right = right.unwrap_number();
        match expr.operator {
            Subtract => Ok(Object::Number(left - right)),
            Multiply => Ok(Object::Number(left * right)),
            Divide => Ok(Object::Number(left / right)),
            GreaterThan => Ok(Object::Boolean(left > right)),
            GreaterEqualThan => Ok(Object::Boolean(left >= right)),
            LessThan => Ok(Object::Boolean(left < right)),
            LessEqualThan => Ok(Object::Boolean(left <= right)),
            Add | Equal | NotEqual => {
                unreachable!("handled above")
            }
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
        use UnaryOperator::*;

        let right = expr.right.accept(self)?;
        match expr.operator {
            LogicalNot => Ok(Object::Boolean(!right.is_truthy())),
            UnaryMinus => {
                if let Object::Number(value) = right {
                    Ok(Object::Number(-value))
                } else {
                    Err(RuntimeError::UnaryMinusOperandError {
                        span: expr.right.span().into(),
                    }
                    .into())
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Object::*;
    use super::*;
    use crate::error::RuntimeError::*;
    use crate::parser::Parser;
    use crate::span::Span;
    use pretty_assertions::assert_eq;

    fn evaluate_expression(input: &'static str) -> Result<Object> {
        let parser = Parser::new(input);
        let expr = parser.parse_expression().unwrap_or_else(|report| {
            panic!(
                "Encountered error while parsing\n{:?}",
                report.with_source_code(input)
            )
        });
        let mut evaluator = Evaluator;
        evaluator.evaluate_expression(&expr)
    }

    fn assert(input: &'static str, expected: &Object) {
        match evaluate_expression(input) {
            Ok(obj) => assert_eq!(obj, *expected),
            Err(report) => panic!(
                "Encountered error while evaluation\n{:?}",
                report.with_source_code(input)
            ),
        }
    }

    fn assert_error(input: &'static str, expected: &RuntimeError) {
        let report = evaluate_expression(input).expect_err("Evaluator should emit error");
        match report.downcast_ref::<RuntimeError>() {
            Some(err) => assert_eq!(err, expected),
            None => panic!("Evaluator should emit RuntimeError"),
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
    fn test_unary_minus() {
        assert("-73", &Number(-73.0));
        assert("--73", &Number(73.0));

        assert_error(
            "-true",
            &UnaryMinusOperandError {
                span: Span::new(1, 5).into(),
            },
        );
        assert_error(
            r#"-("foo" + "bar")"#,
            &UnaryMinusOperandError {
                span: Span::new(1, 16).into(),
            },
        );
    }

    #[test]
    fn test_logical_not() {
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

        assert_error(
            r#"17 + "bar""#,
            &PlusOperandError {
                span: Span::new(0, 10).into(),
            },
        );
        assert_error(
            "42 - true",
            &InfixNonNumberOperandsError {
                span: Span::new(0, 9).into(),
            },
        );
        assert_error(
            r#""foo" * 42"#,
            &InfixNonNumberOperandsError {
                span: Span::new(0, 10).into(),
            },
        );
        assert_error(
            "2 / true",
            &InfixNonNumberOperandsError {
                span: Span::new(0, 8).into(),
            },
        );
    }

    #[test]
    fn test_string_concatenation() {
        assert(r#""hello" + " world!""#, &String("hello world!".into()));
    }

    #[test]
    fn test_comparison_and_equality() {
        assert("1 < 2", &Boolean(true));
        assert("1 <= 2", &Boolean(true));
        assert("1 > 2", &Boolean(false));
        assert("1 >= 2", &Boolean(false));
        assert("(-20 + 70) >= (25 * 2)", &Boolean(true));

        assert("1 == 1", &Boolean(true));
        assert(r#"1 == "1""#, &Boolean(false));
        assert(r#""foo" != "bar""#, &Boolean(true));
        assert("(2 + 4) != (3 * 2)", &Boolean(false));
        assert("5 > 4 == (2 < 3 * 6)", &Boolean(true));

        assert_error(
            r#"17 > "bar""#,
            &InfixNonNumberOperandsError {
                span: Span::new(0, 10).into(),
            },
        );
        assert_error(
            "true < 2",
            &InfixNonNumberOperandsError {
                span: Span::new(0, 8).into(),
            },
        );
        assert_error(
            r#"42 >= ("foo" + "bar")"#,
            &InfixNonNumberOperandsError {
                span: Span::new(0, 21).into(),
            },
        );
        assert_error(
            "false > true",
            &InfixNonNumberOperandsError {
                span: Span::new(0, 12).into(),
            },
        );
    }
}
