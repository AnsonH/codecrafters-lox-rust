use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
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
    ///
    /// The enum value is the string content, while the [Token::lexeme] contains
    /// the surrounding double quotes.
    String(&'src str),
    /// Number literals (e.g. `1234`, `12.34`)
    ///
    /// Note: Lox doesn't allow leading/trailing decimal points, such as `.1234`
    /// or `1234.`
    Number {
        value: f64,
        /// The raw code of the number, purely for passing test cases only
        raw: &'src str,
    },
    /// Identifiers (e.g. `foo`, `bar_2`)
    Identifier(&'src str),
}

impl fmt::Display for Token<'_> {
    // NOTE: Blanket implementation will provide `.to_string()` to `Token`
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Eof => write!(f, "EOF  null"),
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"), // To escape `{`, add another `{`
            Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Comma => write!(f, "COMMA , null"),
            Token::Semicolon => write!(f, "SEMICOLON ; null"),
            Token::Dot => write!(f, "DOT . null"),
            Token::Plus => write!(f, "PLUS + null"),
            Token::Minus => write!(f, "MINUS - null"),
            Token::Star => write!(f, "STAR * null"),
            Token::Slash => write!(f, "SLASH / null"),
            Token::Equal => write!(f, "EQUAL = null"),
            Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            Token::Bang => write!(f, "BANG ! null"),
            Token::BangEqual => write!(f, "BANG_EQUAL != null"),
            Token::Less => write!(f, "LESS < null"),
            Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
            Token::Greater => write!(f, "GREATER > null"),
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            Token::String(string) => write!(f, "STRING \"{string}\" {string}"),
            Token::Number { value, raw } => {
                if value.fract() == 0_f64 {
                    // Tests requires integers to be print as N.0
                    write!(f, "NUMBER {raw} {value}.0")
                } else {
                    write!(f, "NUMBER {raw} {value}")
                }
            }
            Token::Identifier(ident) => write!(f, "IDENTIFIER {ident} null"),
        }
    }
}
