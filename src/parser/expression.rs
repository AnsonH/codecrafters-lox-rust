use crate::{
    ast::{Expr, Literal},
    error::SyntaxError,
    token::TokenKind,
};

use super::Parser;

impl<'src> Parser<'src> {
    // TODO: Add docs
    pub(crate) fn parse_expr(&mut self, min_precedence: u8) -> Result<Expr<'src>, SyntaxError> {
        let lhs_expr = match self.cur_kind() {
            TokenKind::Eof => return Ok(Expr::Literal(Literal::Nil)),
            kind if kind.is_literal() => self.parse_literal_expression()?,
            TokenKind::LeftParen => {
                // self.expect(Kind::LParen)?;
                // let expr = self.parse_expr(0)?;
                // self.expect(Kind::RParen)?;
                self.advance()?;
                let expr = self.parse_expr(0)?;
                // TODO: Replace with `self.expect(TokenKind::RightParen)?`
                if self.expect(TokenKind::RightParen).is_err() {
                    todo!()
                }
                Expr::Grouping(Box::new(expr))
            }
            _ => todo!(),
        };

        Ok(lhs_expr)
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
            _ => unreachable!(),
        }
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
    }

    #[test]
    fn test_arithmetic_operators() {
        // TODO
    }
}
