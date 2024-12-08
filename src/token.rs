//! Tokens for the Lox language.

use crate::span::Span;

/// A token of the Lox language.
///
/// Design note: There is no need to store the token's value here. Value parsing
/// is done in the parser, and the value is stored in the Abstract Syntax Tree.
/// It is memory inefficient to add a new "value" field here.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Token {
    /// Token kind.
    pub kind: TokenKind,
    /// Byte offset range of the token in the source code.
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, strum::Display)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum TokenKind {
    /// End of file
    #[default]
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

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
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
    pub fn to_string(&self, source: &str) -> String {
        let lexeme = &source[self.span];
        format!("{} {} {}", self.kind, lexeme, self.literal_value(source))
    }

    /// Gets the literal value of the token.
    ///
    /// Only string and number has a literal value. Other tokens have the value
    /// of `null`.
    fn literal_value(&self, source: &str) -> String {
        match self.kind {
            TokenKind::Number => {
                let raw_number = &source[self.span];
                let value: f64 = raw_number.parse().unwrap();
                if value.fract() == 0_f64 {
                    // Lox official test suite requires integers to be print as N.0
                    format!("{value}.0")
                } else {
                    format!("{value}")
                }
            }
            TokenKind::String => {
                let value_span = self.span.shrink(1);
                let value = &source[value_span];
                value.to_string()
            }
            _ => "null".to_string(),
        }
    }
}

impl TokenKind {
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Nil
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Number
                | TokenKind::String
        )
    }

    pub fn is_infix_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::EqualEqual
                | TokenKind::BangEqual
        )
    }

    pub fn is_prefix_operator(&self) -> bool {
        matches!(self, TokenKind::Minus | TokenKind::Bang)
    }

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

    pub fn to_str(self) -> &'static str {
        match self {
            TokenKind::Eof => "EOF",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Dot => ".",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Equal => "=",
            TokenKind::EqualEqual => "==",
            TokenKind::Bang => "!",
            TokenKind::BangEqual => "!=",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::String => "string",
            TokenKind::Number => "number",
            TokenKind::Identifier => "identifier",
            TokenKind::And => "and",
            TokenKind::Class => "class",
            TokenKind::Else => "else",
            TokenKind::False => "false",
            TokenKind::For => "for",
            TokenKind::Fun => "fun",
            TokenKind::If => "if",
            TokenKind::Nil => "nil",
            TokenKind::Or => "or",
            TokenKind::Print => "print",
            TokenKind::Return => "return",
            TokenKind::Super => "super",
            TokenKind::This => "this",
            TokenKind::True => "true",
            TokenKind::Var => "var",
            TokenKind::While => "while",
        }
    }
}
