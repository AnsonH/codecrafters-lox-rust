use crate::{
    ast::{
        expression::*,
        operator::{infix_precedence, postfix_precedence, prefix_precedence},
        BinaryOperator, Expr, Literal, UnaryOperator,
    },
    error::SyntaxError,
    token::TokenKind,
};
use miette::{Report, Result};

use super::Parser;

impl Parser<'_> {
    // TODO: Add docs
    pub(super) fn parse_expr(&mut self, min_precedence: u8) -> Result<Expr> {
        let mut lhs_expr = match self.cur_kind() {
            kind if kind.is_literal() => self.parse_literal_expression()?,
            kind if kind.is_prefix_operator() => self.parse_prefix_expression()?,
            TokenKind::LeftParen => self.parse_grouping_expression()?,
            TokenKind::Identifier => self.parse_identifier()?,
            _ => {
                return Err(SyntaxError::MissingExpression {
                    span: self.cur_span(),
                }
                .into())
            }
        };

        loop {
            let peek_result = self
                .lexer
                .peek()
                .expect("peek token should not be None")
                .clone();

            if let Err(err) = peek_result {
                let report = Report::new(err).wrap_err("Invalid operator");
                return Err(report);
            }

            let peek_kind = peek_result.expect("handled Err above").kind;

            if let Some((left_prec, _)) = postfix_precedence(peek_kind) {
                if left_prec < min_precedence {
                    break;
                }
                self.advance()?;
                lhs_expr = self.parse_postfix_expression(lhs_expr)?;
                continue;
            }

            let Some((lhs_prec, _)) = infix_precedence(peek_kind) else {
                break; // peek token is not infix operator
            };

            if lhs_prec < min_precedence {
                break;
            }

            self.advance()?; // Move to infix operator
            lhs_expr = self.parse_infix_expression(lhs_expr)?;
        }

        Ok(lhs_expr)
    }

    pub(super) fn parse_call_expression(&mut self, callee: Expr) -> Result<Expr> {
        let arguments = self.parse_expression_list(TokenKind::RightParen)?;
        if arguments.len() >= Self::MAX_CALL_ARGUMENTS {
            return Err(SyntaxError::TooManyCallArguments {
                max_count: Self::MAX_CALL_ARGUMENTS,
                span: callee.span(),
            }
            .into());
        }
        let span = self.cur_span().merge(&callee.span());
        Ok(Expr::Call(
            Call {
                callee,
                arguments,
                span,
            }
            .into(),
        ))
    }

    /// Parses a comma-separated list of expressions. The `end` token marks the
    /// end of the list.
    ///
    /// The cursor position (`self.token`) should be one token before the first
    /// element or the `end` token when it is called. For most cases this would
    /// be the starting bracket.
    pub(super) fn parse_expression_list(&mut self, end: TokenKind) -> Result<Vec<Expr>> {
        let mut list: Vec<Expr> = vec![];
        if !self.is_peek_kind(end) {
            self.advance()?;
            loop {
                list.push(self.parse_expr(0)?);
                if !self.try_consume_peek(TokenKind::Comma)? {
                    break;
                }
            }
        }
        self.expect_peek(end)?;
        Ok(list)
    }

    pub(super) fn parse_grouping_expression(&mut self) -> Result<Expr> {
        let start_span = self.cur_span();
        self.consume(TokenKind::LeftParen)?;

        let expression = self.parse_expr(0)?;
        self.expect_peek(TokenKind::RightParen)?;

        let span = self.cur_span().merge(&start_span);
        Ok(Expr::Grouping(Grouping { expression, span }.into()))
    }

    pub(super) fn parse_identifier(&mut self) -> Result<Expr> {
        let name = self.source[self.cur_span()].to_owned();
        let span = self.cur_span();
        Ok(Expr::Identifier(Identifier { name, span }.into()))
    }

    pub(super) fn parse_infix_expression(&mut self, left: Expr) -> Result<Expr> {
        let op_kind = self.cur_kind(); // Current token is the infix operator
        self.advance()?;

        let (_, rhs_prec) =
            infix_precedence(op_kind).expect("current token should be an infix operator");
        let right = self.parse_expr(rhs_prec)?;

        let span = self.cur_span().merge(&left.span());
        match op_kind {
            TokenKind::Equal => Ok(Expr::Assignment(Assignment { left, right, span }.into())),
            _ => Ok(Expr::Binary(
                Binary {
                    left,
                    operator: BinaryOperator::from(op_kind),
                    right,
                    span,
                }
                .into(),
            )),
        }
    }

    pub(super) fn parse_literal_expression(&mut self) -> Result<Expr> {
        let span = self.cur_span();
        let value = match self.cur_kind() {
            TokenKind::Nil => Literal::Nil,
            TokenKind::True => Literal::Boolean(true),
            TokenKind::False => Literal::Boolean(false),
            TokenKind::Number => {
                let raw_number = self.cur_src();
                let value: f64 = raw_number.parse().unwrap();
                Literal::Number(value)
            }
            TokenKind::String => {
                // The token span includes the `"` quotes, so exclude them
                let span_without_quotes = self.cur_span().shrink(1);
                let value = self.source[span_without_quotes].to_owned();
                Literal::String(value)
            }
            _ => unreachable!("unexpected literal kind: {}", self.cur_kind()),
        };
        Ok(Expr::Literal(LiteralExpr { value, span }.into()))
    }

    pub(super) fn parse_postfix_expression(&mut self, left: Expr) -> Result<Expr> {
        match self.cur_kind() {
            TokenKind::LeftParen => self.parse_call_expression(left),
            _ => unreachable!("unexpected postfix operator: {}", self.cur_kind()),
        }
    }

    pub(super) fn parse_prefix_expression(&mut self) -> Result<Expr> {
        let start_span = self.cur_span();
        let operator = match self.cur_kind() {
            TokenKind::Minus => UnaryOperator::UnaryMinus,
            TokenKind::Bang => UnaryOperator::LogicalNot,
            _ => unreachable!("unexpected unary operator: {}", self.cur_kind()),
        };

        self.advance()?;
        let (_, rhs_prec) = prefix_precedence(operator);
        let right = self.parse_expr(rhs_prec)?;

        let span = self.cur_span().merge(&start_span);
        Ok(Expr::Unary(
            Unary {
                operator,
                right,
                span,
            }
            .into(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::printer::AstPrefixPrinter, span::Span};

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert(input: &'static str, expected: &str) {
        let parser = Parser::new(input);
        let mut printer = AstPrefixPrinter;

        match parser.parse_expression() {
            Ok(expr) => assert_eq!(printer.print_expression(&expr), expected),
            Err(report) => panic!(
                "Encountered error while parsing\n{:?}",
                report.with_source_code(input)
            ),
        }
    }

    fn assert_error(input: &str, expected: SyntaxError) {
        let parser = Parser::new(input);
        let report = parser
            .parse_expression()
            .expect_err("Parser should emit error");

        match report.downcast_ref::<SyntaxError>() {
            Some(err) => assert_eq!(*err, expected),
            None => panic!("Parser should emit a SyntaxError"),
        }
    }

    #[test]
    fn test_empty() {
        assert("", "nil");
    }

    #[test]
    fn test_literal() {
        assert("true", "true");
        assert("false", "false");
        assert("nil", "nil");
        assert("1", "1.0");
        assert("42.47", "42.47");
        assert(r#""hello""#, "hello");
    }

    #[test]
    fn test_grouping() {
        assert(r#"("foo")"#, "(group foo)");
        assert(r#"(("foo"))"#, "(group (group foo))");

        assert_error(
            "(true",
            SyntaxError::UnexpectedToken {
                expected: ")".into(),
                actual: "EOF".into(),
                span: Span::new(5, 5),
            },
        );
    }

    #[test]
    fn test_unary_expression() {
        assert("!true", "(! true)");
        assert("-5", "(- 5.0)");
        assert("!-5", "(! (- 5.0))");
        assert("(!!(false))", "(group (! (! (group false))))");

        assert_error(
            "!",
            SyntaxError::MissingExpression {
                span: Span::new(1, 1),
            },
        );
    }

    #[test]
    fn test_arithmetic() {
        assert("1 + 2", "(+ 1.0 2.0)");
        assert("1 - 2", "(- 1.0 2.0)");
        assert("1 * 2", "(* 1.0 2.0)");
        assert("1 / 2", "(/ 1.0 2.0)");

        // Operator precedence
        assert("1 + 2 + 3", "(+ (+ 1.0 2.0) 3.0)");
        assert("1 + 2 - 3", "(- (+ 1.0 2.0) 3.0)");
        assert("1 * 2 * 3", "(* (* 1.0 2.0) 3.0)");
        assert("1 * 2 / 3", "(/ (* 1.0 2.0) 3.0)");
        assert("1 + 2 * 3", "(+ 1.0 (* 2.0 3.0))");
        assert(
            "1 + 2 * 3 - 4 / 5 + 6",
            "(+ (- (+ 1.0 (* 2.0 3.0)) (/ 4.0 5.0)) 6.0)",
        );

        // Unary operator has higher precedence than binary operator
        assert("-1 * 2", "(* (- 1.0) 2.0)");
        assert("1 + -2", "(+ 1.0 (- 2.0))");

        // Grouping
        assert("(1 + 2) * 3", "(* (group (+ 1.0 2.0)) 3.0)");
        assert("1 / (2 + 3) * 4", "(* (/ 1.0 (group (+ 2.0 3.0))) 4.0)");

        // Errors
        assert_error(
            "1 +",
            SyntaxError::MissingExpression {
                span: Span::new(3, 3),
            },
        );
    }

    #[test]
    fn test_comparison_and_equality() {
        assert("1 < 2", "(< 1.0 2.0)");
        assert("1 <= 2", "(<= 1.0 2.0)");
        assert("1 > 2", "(> 1.0 2.0)");
        assert("1 >= 2", "(>= 1.0 2.0)");
        assert("1 == 2", "(== 1.0 2.0)");
        assert("1 != 2", "(!= 1.0 2.0)");

        assert("1 < 2 == 3 < 4", "(== (< 1.0 2.0) (< 3.0 4.0))");
        assert("1 > 2 != 3 < 4", "(!= (> 1.0 2.0) (< 3.0 4.0))");
        assert(
            "1 < 2 + 3 != 4 * 5 < 6 + 7",
            "(!= (< 1.0 (+ 2.0 3.0)) (< (* 4.0 5.0) (+ 6.0 7.0)))",
        );
    }

    #[test]
    fn test_assignment() {
        assert("a = 1", "(assign a 1.0)");
        assert("a = b = c", "(assign a (assign b c))");
        assert("a = b + c = d", "(assign a (assign (+ b c) d))");
        assert("(a = b) = c", "(assign (group (assign a b)) c)");

        assert_error(
            "a =",
            SyntaxError::MissingExpression {
                span: Span::new(3, 3),
            },
        );
    }

    #[test]
    fn test_logical_operators() {
        assert("true or false", "(or true false)");
        assert("true and false", "(and true false)");
        assert("false and true or true", "(or (and false true) true)");
        assert("false or x = true", "(or false (assign x true))");
        assert("true and x = false", "(and true (assign x false))");
        assert(
            "1 + 2 and 3 or 4 - 5",
            "(or (and (+ 1.0 2.0) 3.0) (- 4.0 5.0))",
        );
    }

    #[test]
    fn test_call() {
        assert("foo()", "(foo)");
        assert("foo(1)", "(foo 1.0)");
        assert("foo(1 + 2, 3, 4 * 5)", "(foo (+ 1.0 2.0) 3.0 (* 4.0 5.0))");
        assert("foo(a)(b, bar(c))", "((foo a) b (bar c))");

        assert_error(
            "foo(1, 2",
            SyntaxError::UnexpectedToken {
                expected: ")".into(),
                actual: "EOF".into(),
                span: Span::new(8, 8),
            },
        );
    }

    #[test]
    fn test_span() {
        let input = r#"  1 + (23)"#;
        let parser = Parser::new(input);
        let binary_expr = parser.parse_expression().expect("No SyntaxError");

        assert_eq!(binary_expr.span(), (2, 10).into());
        match binary_expr {
            Expr::Binary(binary) => {
                assert_eq!(binary.left.span(), (2, 3).into());
                assert_eq!(binary.right.span(), (6, 10).into());
            }
            _ => panic!("should be Binary expression"),
        }
    }
}
