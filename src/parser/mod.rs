mod expression;

use std::iter::Peekable;

use crate::{
    ast::Program,
    error::SyntaxError,
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
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
