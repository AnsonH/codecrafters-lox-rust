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
    lexer: Peekable<Lexer<'src>>,
    /// The current token that the lexer is at.
    token: Token<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
            token: Token::default(),
        }
    }

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

    /// Get current token.
    #[inline]
    pub(crate) fn cur_token(&self) -> Token<'src> {
        self.token
    }

    /// Consumes the next token if it equals to `expected`.
    pub(crate) fn expect(&mut self, expected: TokenKind) -> Result<Token<'src>, ()> {
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
