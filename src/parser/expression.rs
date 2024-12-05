use crate::{
    ast::{Expr, Literal, UnaryOperator},
    error::SyntaxError,
    token::TokenKind,
};

use super::Parser;

impl<'src> Parser<'src> {
    // TODO: Add docs
    pub(crate) fn parse_expr(&mut self, min_precedence: u8) -> Result<Expr<'src>, SyntaxError> {
        let lhs_expr = match self.cur_kind() {
            kind if kind.is_literal() => self.parse_literal_expression()?,
            kind if kind.is_unary_operator() => self.parse_unary_expression()?,
            TokenKind::LeftParen => self.parse_grouping_expression()?,
            TokenKind::Eof => return Ok(Expr::Literal(Literal::Nil)),
            _ => todo!(),
        };

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

    pub(crate) fn parse_unary_expression(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let operator = match self.cur_kind() {
            TokenKind::Minus => UnaryOperator::UnaryMinus,
            TokenKind::Bang => UnaryOperator::LogicalNot,
            _ => unreachable!("unexpected unary operator: {}", self.cur_kind()),
        };
        self.advance()?;

        let (_, rhs_prec) = unary_precedence(operator);
        let right = self.parse_expr(rhs_prec)?;
        Ok(Expr::Unary(operator, Box::new(right)))
    }
}

// TODO: Replace with enum
// TODO: Move precedence to a new file (preferably same folder as AST)
pub(crate) fn unary_precedence(op: UnaryOperator) -> ((), u8) {
    match op {
        UnaryOperator::LogicalNot | UnaryOperator::UnaryMinus => ((), 5),
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ErrorFormat;

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert_parsed_expr(input: &str, expected: &str) {
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
        assert_parsed_expr("", "nil");
    }

    #[test]
    fn test_literal() {
        assert_parsed_expr("true", "true");
        assert_parsed_expr("false", "false");
        assert_parsed_expr("nil", "nil");
        assert_parsed_expr("1", "1.0");
        assert_parsed_expr("42.47", "42.47");
        assert_parsed_expr(r#""hello""#, "hello");
    }

    #[test]
    fn test_grouping() {
        assert_parsed_expr(r#"("foo")"#, "(group foo)");
        assert_parsed_expr(r#"(("foo"))"#, "(group (group foo))");
    }

    #[test]
    fn test_unary_expression() {
        assert_parsed_expr("!true", "(! true)");
        assert_parsed_expr("-5", "(- 5.0)");
        assert_parsed_expr("!-5", "(! (- 5.0))");

        assert_parsed_expr("(!!(false))", "(group (! (! (group false))))");
    }

    #[test]
    fn test_arithmetic_operators() {
        // TODO
    }
}
