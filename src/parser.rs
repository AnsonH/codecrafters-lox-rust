use std::iter::Peekable;

use crate::{
    ast::{Expr, Literal, Program},
    error::SyntaxError,
    lexer::Lexer,
    token::{Token, TokenKind, TokenValue},
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
            Some(Ok(Token {
                kind: TokenKind::Eof,
                ..
            }))
            | None => return Ok(Expr::Literal(Literal::Nil)),
            Some(Err(err)) => return Err(err),
            Some(Ok(token)) => token,
        };

        let lhs_expr = match lhs_token {
            Token {
                kind: TokenKind::True,
                ..
            } => Expr::Literal(Literal::Boolean(true)),
            Token {
                kind: TokenKind::False,
                ..
            } => Expr::Literal(Literal::Boolean(false)),
            Token {
                kind: TokenKind::Nil,
                ..
            } => Expr::Literal(Literal::Nil),
            Token {
                kind: TokenKind::Number,
                value: TokenValue::Number(n),
                ..
            } => Expr::Literal(Literal::Number(n)),
            Token {
                kind: TokenKind::String,
                value: TokenValue::String(s),
                ..
            } => Expr::Literal(Literal::String(s)),
            Token {
                kind: TokenKind::LeftParen,
                ..
            } => {
                let expr = self.parse_expression(0)?;
                if self.expect(TokenKind::RightParen).is_err() {
                    todo!()
                }
                Expr::Grouping(Box::new(expr))
            }
            _ => todo!(),
        };

        Ok(lhs_expr)
    }

    /// Consumes the next token if it equals to `expected`.
    fn expect(&mut self, expected: TokenKind) -> Result<Token<'src>, ()> {
        match self.lexer.peek() {
            Some(Ok(token)) if token.kind == expected => {
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
