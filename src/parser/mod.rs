//! Parser for the Lox language.

mod expression;
mod statement;

use std::iter::Peekable;

use crate::{
    ast::{expression::LiteralExpr, Expr, Literal, Program, Stmt},
    error::SyntaxError,
    lexer::Lexer,
    span::Span,
    token::{Token, TokenKind},
};

pub struct Parser<'src> {
    /// Source code.
    source: &'src str,

    lexer: Peekable<Lexer<'src>>,

    /// The current token that the lexer is at.
    token: Token,
}

impl<'src> Parser<'src> {
    /// Having a maximum argument count will simplify bytecode interpreter.
    ///
    /// See https://craftinginterpreters.com/functions.html#maximum-argument-counts
    const MAX_CALL_ARGUMENTS: usize = 255;

    const MAX_FUNCTION_PARAMETERS: usize = 255;

    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source).peekable(),
            token: Token::default(),
        }
    }

    /// Main entry point for parsing a program.
    ///
    /// # Returns
    ///
    /// An `Ok` with [Program] if there are no syntax errors, or `Err` if there
    /// are any errors.
    pub fn parse_program(mut self) -> miette::Result<Program> {
        let mut body: Vec<Stmt> = vec![];

        self.advance()?;
        while !self.is_cur_kind(TokenKind::Eof) {
            let statement = self.parse_declaration_statement()?;
            body.push(statement);
            self.advance()?;
        }

        let span = Span::new(0, self.source.len() as u32);
        Ok(Program { body, span })
    }

    /// Parses an expression (e.g. `1 + 2 * 3`).
    ///
    /// # Returns
    ///
    /// An `Ok` with [Expr] if there are no syntax errors, or `Err` if there are
    /// any errors.
    pub fn parse_expression(mut self) -> miette::Result<Expr> {
        self.advance()?;
        if self.is_cur_kind(TokenKind::Eof) {
            return Ok(Expr::Literal(
                LiteralExpr {
                    value: Literal::Nil,
                    span: self.cur_span(),
                }
                .into(),
            ));
        }
        self.parse_expr(0)
    }

    /// Get current token's kind.
    #[inline]
    fn cur_kind(&self) -> TokenKind {
        self.token.kind
    }

    /// Get current token.
    #[inline]
    fn cur_token(&self) -> Token {
        self.token
    }

    /// Get current token's span.
    #[inline]
    fn cur_span(&self) -> Span {
        self.token.span
    }

    /// Get current token's source text.
    #[inline]
    fn cur_src(&self) -> &'src str {
        // Safety: the lexer ensures that the span is not out of range
        &self.source[self.cur_token().span]
    }

    /// Whether the current token is a certain [TokenKind].
    #[inline]
    fn is_cur_kind(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    /// Whether the peek token is a certain [TokenKind].
    fn is_peek_kind(&mut self, kind: TokenKind) -> bool {
        matches!(self.lexer.peek(), Some(Ok(Token { kind: k, .. })) if *k == kind)
    }

    // TODO: Change return type to `miette::Result<()>`
    /// Advance to the next token.
    fn advance(&mut self) -> Result<(), SyntaxError> {
        if let Some(result) = self.lexer.next() {
            self.token = result?;
        }
        Ok(())
    }

    /// If current token kind is `expected`, advances to next token. Otherwise
    /// return unexpected token error.
    fn consume(&mut self, expected: TokenKind) -> miette::Result<()> {
        if self.is_cur_kind(expected) {
            self.advance()?;
            Ok(())
        } else {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_str().into(),
                actual: self.cur_kind().to_str().into(),
                span: self.cur_span(),
            }
            .into())
        }
    }

    /// If peek token kind is `expected`, advance to that token. Otherwise return
    /// unexpected token error.
    fn expect_peek(&mut self, expected: TokenKind) -> miette::Result<()> {
        match self.lexer.peek() {
            Some(Ok(token)) => {
                if token.kind == expected {
                    self.advance()?;
                    Ok(())
                } else {
                    Err(SyntaxError::UnexpectedToken {
                        expected: expected.to_str().into(),
                        actual: token.kind.to_str().into(),
                        span: token.span,
                    }
                    .into())
                }
            }
            Some(Err(err)) => Err(err.clone().into()),
            None => {
                // i.e. reached EOF
                let span = Span::sized(self.cur_token().span.start, 0);
                Err(SyntaxError::UnexpectedToken {
                    expected: expected.to_str().into(),
                    actual: TokenKind::Eof.to_str().into(),
                    span,
                }
                .into())
            }
        }
    }

    /// If peek token kind is `expected`, consumes that token so that current
    /// token position is one token after the `expected` token. Otherwise, it's
    /// a no-op.
    fn try_consume_peek(&mut self, expected: TokenKind) -> miette::Result<bool> {
        if self.is_peek_kind(expected) {
            self.advance()?; // Move to peek token
            self.consume(expected)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_try_consume_peek() -> miette::Result<()> {
        let input = "+-*";
        let mut parser = Parser::new(input);
        parser.advance()?; // Move to `+`

        let is_consumed = parser.try_consume_peek(TokenKind::Bang)?;
        assert_eq!(is_consumed, false);
        assert_eq!(parser.token.kind, TokenKind::Plus);

        let is_consumed = parser.try_consume_peek(TokenKind::Minus)?;
        assert_eq!(is_consumed, true);
        assert_eq!(parser.token.kind, TokenKind::Star);

        Ok(())
    }
}
