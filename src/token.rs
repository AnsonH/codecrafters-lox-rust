use std::{fmt::Display, ops::Range};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    /// Token kind.
    pub kind: TokenKind,
    /// Byte offset range of the token in the source code.
    pub span: Span,
    /// Token's value.
    pub value: TokenValue<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
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

    /// String literals (e.g. `"hi"`)
    String,
    /// Number literals (e.g. `1234`, `12.34`)
    ///
    /// Note: Lox doesn't allow leading/trailing decimal points, such as `.1234`
    /// or `1234.`
    Number,
    /// Identifiers (e.g. `foo`, `bar_2`)
    Identifier,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

/// The token's value.
///
/// # Optimization
///
/// We do not store token's value inside [TokenKind] because it prevents that
/// enum's size from being larger than 1 byte. There will be heavy usages of
/// [TokenKind] in the parser, so it is more memory efficient
/// ([reference](https://oxc.rs/docs/learn/parser_in_rust/lexer.html#smaller-tokens)).
#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue<'src> {
    None,
    Boolean(bool),
    Number(f64),
    /// Since Lox does not support escape sequences like `\n`, we don't need to
    /// allocate a new string to store the computed escaped string. Thus, we can
    /// directly reference the source text.
    String(&'src str),
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenKind, span: Span, value: TokenValue<'src>) -> Self {
        Self { kind, span, value }
    }

    /// Converts the token into a special format for passing [Lox's test
    /// suite](https://github.com/munificent/craftinginterpreters/tree/master/test/scanning).
    ///
    /// # Format
    ///
    /// `<token_type> <lexeme> <value>`
    /// - `<token_type>`: The type of the token
    /// - `<lexeme>`: The actual sequence of characters that formed the token.
    /// - `<literal>`: The literal value of the token
    ///
    /// Examples: `LEFT_PAREN ( null`, `STRING "foo" foo`
    pub fn to_string(&self, source: &'src str) -> String {
        let lexeme = source.get(Range::<usize>::from(self.span)).unwrap_or("");
        format!("{} {} {}", self.kind, lexeme, self.value)
    }
}

impl TokenKind {
    pub fn match_keyword(input: &str) -> Self {
        if input.len() > 6 {
            return TokenKind::Identifier;
        }
        match input {
            "if" => TokenKind::If,
            "or" => TokenKind::Or,

            "and" => TokenKind::And,
            "for" => TokenKind::For,
            "fun" => TokenKind::Fun,
            "nil" => TokenKind::Nil,
            "var" => TokenKind::Var,

            "else" => TokenKind::Else,
            "this" => TokenKind::This,
            "true" => TokenKind::True,

            "class" => TokenKind::Class,
            "false" => TokenKind::False,
            "print" => TokenKind::Print,
            "super" => TokenKind::Super,
            "while" => TokenKind::While,

            "return" => TokenKind::Return,
            _ => TokenKind::Identifier,
        }
    }
}

impl<'src> Display for TokenValue<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenValue::Number(n) => {
                if n.fract() == 0_f64 {
                    // Lox official test suite requires integers to be print as N.0
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            TokenValue::String(s) => write!(f, "{s}"),
            _ => write!(f, "null"),
        }
    }
}
