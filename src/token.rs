use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    /// Token kind.
    kind: TokenKind,
    /// Byte offset range of the token in the source code.
    span: Span,
    /// Token's value.
    value: TokenValue<'src>,
}

#[derive(Debug, Clone, PartialEq, strum::Display)]
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
