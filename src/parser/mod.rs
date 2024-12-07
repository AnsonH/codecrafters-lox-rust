//! Parser for the Lox language.

mod expression;

use std::iter::Peekable;

use crate::{
    ast::Expr,
    error::SyntaxError,
    lexer::Lexer,
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

    /// Parses an expression (e.g. `1 + 2 * 3`).
    pub fn parse_expression(mut self) -> Result<Expr<'src>, SyntaxError> {
        self.advance()?;
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

    /// Get current token's source text.
    #[inline]
    pub(crate) fn cur_src(&self) -> &'src str {
        // Safety: the lexer ensures that the span is not out of range
        &self.source[self.cur_token().span]
    }

    /// Consumes the next token if it equals to `expected`.
    pub(crate) fn expect(&mut self, expected: TokenKind) -> Result<Token, ()> {
        match self.lexer.peek() {
            Some(Ok(token)) if token.kind == expected => {
                let token = self.lexer.next().unwrap().unwrap();
                Ok(token)
            }
            // TODO: Add "expected token" error
            // TODO: Propagate `lexer.peek()`'s SyntaxError
            _ => Err(()),
        }
    }
}