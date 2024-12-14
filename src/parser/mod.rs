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
    pub fn parse_program(mut self) -> miette::Result<Program<'src>> {
        let mut body: Vec<Stmt<'src>> = vec![];

        self.advance()?;
        while !self.is_cur_kind(TokenKind::Eof) {
            let statement = self.parse_statement()?;
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
    pub fn parse_expression(mut self) -> miette::Result<Expr<'src>> {
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

    /// Advance to the next token.
    pub(crate) fn advance(&mut self) -> Result<(), SyntaxError> {
        if let Some(result) = self.lexer.next() {
            self.token = result?;
        }
        Ok(())
    }

    /// Get current token's kind.
    #[inline]
    pub(crate) fn cur_kind(&self) -> TokenKind {
        self.token.kind
    }

    /// Get current token.
    #[inline]
    pub(crate) fn cur_token(&self) -> Token {
        self.token
    }

    /// Get current token's span.
    #[inline]
    pub(crate) fn cur_span(&self) -> Span {
        self.token.span
    }

    /// Get current token's source text.
    #[inline]
    pub(crate) fn cur_src(&self) -> &'src str {
        // Safety: the lexer ensures that the span is not out of range
        &self.source[self.cur_token().span]
    }

    /// Consumes the next token if it equals to `expected`.
    pub(crate) fn expect(&mut self, expected: TokenKind) -> miette::Result<()> {
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

    /// Whether the current token is a certain [TokenKind].
    #[inline]
    pub(crate) fn is_cur_kind(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }
}
