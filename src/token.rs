use std::fmt;

#[derive(Debug)]
pub struct Token<'de> {
    pub kind: TokenKind,
    /// Source code slice for this token.
    pub lexeme: &'de str,
    // TODO: Add `span` to track the token location
}

impl<'de> Token<'de> {
    pub fn new(kind: TokenKind, lexeme: &'de str) -> Self {
        Self { kind, lexeme }
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
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,

    /// `,`
    Comma,
    /// `;`
    Semicolon,
    /// `.`
    Dot,

    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,

    /// `=`
    Equal,
    /// `==`
    EqualEqual,
    /// `!`
    Bang,
    /// `!=`
    BangEqual,

    /// `<`
    Less,
    /// `<=`
    LessEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,

    /// String literals (e.g. `"hi"`). The lexeme contains the surrounding double quotes.
    String,
}

impl fmt::Display for Token<'_> {
    // NOTE: Blanket implementation will provide `.to_string()` to `Token`
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lexeme = self.lexeme;
        match self.kind {
            TokenKind::Eof => write!(f, "EOF  null"),
            TokenKind::LeftParen => write!(f, "LEFT_PAREN {lexeme} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {lexeme} null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {lexeme} null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE {lexeme} null"),
            TokenKind::Comma => write!(f, "COMMA {lexeme} null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON {lexeme} null"),
            TokenKind::Dot => write!(f, "DOT {lexeme} null"),
            TokenKind::Plus => write!(f, "PLUS {lexeme} null"),
            TokenKind::Minus => write!(f, "MINUS {lexeme} null"),
            TokenKind::Star => write!(f, "STAR {lexeme} null"),
            TokenKind::Slash => write!(f, "SLASH {lexeme} null"),
            TokenKind::Equal => write!(f, "EQUAL {lexeme} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {lexeme} null"),
            TokenKind::Bang => write!(f, "BANG {lexeme} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {lexeme} null"),
            TokenKind::Less => write!(f, "LESS {lexeme} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {lexeme} null"),
            TokenKind::Greater => write!(f, "GREATER {lexeme} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {lexeme} null"),
            TokenKind::String => write!(f, "STRING {lexeme} {}", lexeme.trim_matches('"')),
        }
    }
}
