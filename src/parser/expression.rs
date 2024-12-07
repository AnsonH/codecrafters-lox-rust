use crate::{
    ast::{BinaryOperator, Expr, Literal, UnaryOperator},
    error::SyntaxError,
    token::TokenKind,
};

use super::Parser;

impl<'src> Parser<'src> {
    // TODO: Add docs
    pub(crate) fn parse_expr(&mut self, min_precedence: u8) -> Result<Expr<'src>, SyntaxError> {
        let mut lhs_expr = match self.cur_kind() {
            kind if kind.is_literal() => self.parse_literal_expression()?,
            kind if kind.is_prefix_operator() => self.parse_prefix_expression()?,
            TokenKind::LeftParen => self.parse_grouping_expression()?,
            TokenKind::Eof => return Ok(Expr::Literal(Literal::Nil)),
            _ => todo!(),
        };

        loop {
            let peek_result = self
                .lexer
                .peek()
                // FIXME: Panics if `1 +`
                .expect("peek token should not be None")
                .as_ref();

            if let Err(err) = peek_result {
                return Err(err.clone());
            }

            let peek_kind = peek_result.expect("handled Err above").kind;
            let Some((lhs_prec, _)) = infix_precedence(peek_kind) else {
                break;
            };

            if lhs_prec < min_precedence {
                break;
            }

            self.advance()?; // Move to infix operator
            lhs_expr = self.parse_infix_expression(lhs_expr)?;
        }

        Ok(lhs_expr)
    }

    pub(crate) fn parse_grouping_expression(&mut self) -> Result<Expr<'src>, SyntaxError> {
        self.advance()?; // Consume `(`
        let expr = self.parse_expr(0)?;
        // TODO: Replace with `self.expect(TokenKind::RightParen)?`
        if self.expect(TokenKind::RightParen).is_err() {
            todo!()
        }
        Ok(Expr::Grouping(Box::new(expr)))
    }

    pub(crate) fn parse_infix_expression(
        &mut self,
        lhs: Expr<'src>,
    ) -> Result<Expr<'src>, SyntaxError> {
        let op_kind = self.cur_kind();
        let (_, rhs_prec) =
            infix_precedence(op_kind).expect("current token should be an infix operator");

        self.advance()?; // Consume operator
        let rhs = self.parse_expr(rhs_prec)?;
        Ok(Expr::Binary(
            Box::new(lhs),
            BinaryOperator::from(op_kind),
            Box::new(rhs),
        ))
    }

    pub(crate) fn parse_literal_expression(&mut self) -> Result<Expr<'src>, SyntaxError> {
        match self.cur_kind() {
            TokenKind::Nil => Ok(Expr::Literal(Literal::Nil)),
            TokenKind::True => Ok(Expr::Literal(Literal::Boolean(true))),
            TokenKind::False => Ok(Expr::Literal(Literal::Boolean(false))),
            TokenKind::Number => {
                let raw_number = self.cur_src();
                let value: f64 = raw_number.parse().unwrap();
                Ok(Expr::Literal(Literal::Number(value)))
            }
            TokenKind::String => {
                // The token span includes the `"` quotes, so exclude them
                let span_without_quotes = self.cur_token().span.shrink(1);
                let value = &self.source[span_without_quotes];
                Ok(Expr::Literal(Literal::String(value)))
            }
            _ => unreachable!("unexpected literal kind: {}", self.cur_kind()),
        }
    }

    pub(crate) fn parse_prefix_expression(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let operator = match self.cur_kind() {
            TokenKind::Minus => UnaryOperator::UnaryMinus,
            TokenKind::Bang => UnaryOperator::LogicalNot,
            _ => unreachable!("unexpected unary operator: {}", self.cur_kind()),
        };
        self.advance()?;

        let (_, rhs_prec) = prefix_precedence(operator);
        let right = self.parse_expr(rhs_prec)?;
        Ok(Expr::Unary(operator, Box::new(right)))
    }
}

// TODO: Replace with enum
// TODO: Move precedence to a new file (preferably same folder as AST)
pub(crate) fn prefix_precedence(op: UnaryOperator) -> ((), u8) {
    match op {
        UnaryOperator::LogicalNot | UnaryOperator::UnaryMinus => ((), 9),
    }
}

/// Gets the left & right precedence values of an infix (binary) operator.
///
/// "LHS < RHS" means the operator is left associative, while the opposite means
/// right associative.
///
/// Returning `None` means the token kind is not an infix operator.
pub(crate) fn infix_precedence(kind: TokenKind) -> Option<(u8, u8)> {
    match kind {
        TokenKind::EqualEqual | TokenKind::BangEqual => Some((1, 2)),
        TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => {
            Some((3, 4))
        }
        TokenKind::Plus | TokenKind::Minus => Some((5, 6)),
        TokenKind::Star | TokenKind::Slash => Some((7, 8)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ErrorFormat;

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert(input: &str, expected: &str) {
        let parser = Parser::new(input);
        match parser.parse_expression() {
            Ok(expr) => assert_eq!(expr.to_string(), expected),
            Err(err) => {
                err.print_error(input, &ErrorFormat::Pretty);
                panic!("Encountered a SyntaxError");
            }
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
    }

    #[test]
    fn test_unary_expression() {
        assert("!true", "(! true)");
        assert("-5", "(- 5.0)");
        assert("!-5", "(! (- 5.0))");
        assert("(!!(false))", "(group (! (! (group false))))");
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
}
