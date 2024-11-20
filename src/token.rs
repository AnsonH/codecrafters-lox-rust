use std::fmt;

#[derive(Debug)]
pub struct Token<'de> {
    pub kind: TokenKind,
    /// Source code slice for this token.
    pub lexeme: &'de str,
    // TODO: Add `offset` to track location of token
}

impl<'de> Token<'de> {
    pub fn new(kind: TokenKind, lexeme: &'de str) -> Self {
        Self { kind, lexeme }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lexeme = self.lexeme;
        match self.kind {
            TokenKind::Eof => write!(f, "EOF  null"),
            TokenKind::LeftParen => write!(f, "LEFT_PAREN {lexeme} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {lexeme} null"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    /// End of file
    Eof,

    /// `(`
    LeftParen,
    /// `)`
    RightParen,
}
