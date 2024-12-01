use std::iter::Peekable;

use crate::{
    ast::{Expr, Literal, Program},
    error::SyntaxError,
    lexer::Lexer,
    token::TokenKind,
};

pub struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
        }
    }

    /// Main entry point.
    ///
    /// # Returns
    ///
    /// The Abstract Syntax Tree (AST) of the program if there are no syntax errors.
    /// For simplicity, the parser immediately stops and returns `Err(SyntaxError)`
    /// if it encounters any syntax errors.
    pub fn parse(mut self) -> Result<Program, SyntaxError> {
        todo!()
    }

    // TODO: Extract expression-related parsing into new file

    // TODO: Add docs
    pub fn parse_expression(&mut self, min_precedence: u8) -> Result<Expr<'src>, SyntaxError> {
        let lhs_token = match self.lexer.next() {
            Some(Ok(TokenKind::Eof)) | None => return Ok(Expr::Literal(Literal::Nil)),
            Some(Err(err)) => return Err(err),
            Some(Ok(token)) => token,
        };

        let lhs = match lhs_token {
            TokenKind::True => Expr::Literal(Literal::Boolean(true)),
            TokenKind::False => Expr::Literal(Literal::Boolean(false)),
            TokenKind::Nil => Expr::Literal(Literal::Nil),
            // FIXME
            // TokenKind::Number { value, .. } => Expr::Literal(Literal::Number(value)),
            // TokenKind::String(s) => Expr::Literal(Literal::String(s)),
            // TokenKind::LeftParen => {
            //     let expr = self.parse_expression(0)?;
            //     if self.expect(TokenKind::RightParen).is_err() {
            //         todo!()
            //     }
            //     Expr::Grouping(Box::new(expr))
            // }
            _ => todo!(),
        };

        Ok(lhs)
    }

    /// Consumes the next token if it equals to `expected`.
    fn expect(&mut self, expected: TokenKind) -> Result<TokenKind, ()> {
        match self.lexer.peek() {
            Some(Ok(token)) if *token == expected => {
                let token = self.lexer.next().unwrap().unwrap();
                Ok(token)
            }
            // TODO: Propagate `lexer.peek()`'s SyntaxError
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ErrorFormat;

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert_parsed_expr(input: &str, expected: &str) {
        let mut parser = Parser::new(input);
        match parser.parse_expression(0) {
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
