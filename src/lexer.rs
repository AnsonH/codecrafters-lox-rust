use crate::error::SyntaxError;
use crate::span::Span;
use crate::token::{Token, TokenKind, TokenValue};
use std::iter::Peekable;
use std::ops::Range;
use std::str::Chars;

/// Lexer tokenizes an input string into a sequence of tokens.
///
/// # Lifetimes
///
/// The `'src` lifetime ensures both the `input` and `rest_chars` are tied to the
/// same lifetime, i.e. they both reference the same string source.
pub struct Lexer<'src> {
    /// The input program.
    input: &'src str,
    /// Remaining characters of the input that the lexer hasn't scanned.
    ///
    /// [Peekable] is useful for peeking into future characters without consuming them.
    rest_chars: Peekable<Chars<'src>>,
    /// Current byte position in the input.
    position: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Lexer {
            input,
            rest_chars: input.chars().peekable(),
            position: 0,
        }
    }

    /// Consumes a character and moves the lexer's position forward.
    ///
    /// Returns an option tuple of the consumed character and its UTF-8 length.
    fn read_char(&mut self) -> Option<(char, usize)> {
        let ch: char = self.rest_chars.next()?;
        let ch_len = ch.len_utf8();
        self.position += ch_len;
        Some((ch, ch_len))
    }

    /// Consumes characters while `predicate` returns true.
    ///
    /// The `predicate` receives the peeked char as the argument. If it returns true,
    /// the character is consumed. If it returns false or the peeked char is end of file,
    /// the loop stops.
    ///
    /// # Returns
    ///
    /// A tuple of `(matched_str, total_len)`:
    /// - `matched_str`: The string slice that satisfies the predicate
    /// - `total_len`: Total UTF-8 length of the matched string
    fn read_chars_while<F>(&mut self, predicate: F) -> (&'src str, usize)
    where
        F: Fn(char) -> bool,
    {
        let start_pos = self.position;

        while let Some(&peek_ch) = self.rest_chars.peek() {
            if predicate(peek_ch) {
                self.read_char();
            } else {
                break;
            }
        }

        let matched_str = &self.input[start_pos..self.position];
        let total_len = self.position - start_pos;
        (matched_str, total_len)
    }

    /// Returns true if peeked char equals `expected`.
    #[inline]
    fn is_peek_char(&mut self, expected: char) -> bool {
        self.rest_chars.peek() == Some(&expected)
    }

    /// Peeks twice to see if that character satisfies `predicate`.
    fn is_next_peek_char<F>(&mut self, predicate: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        let mut rest_chars = self.rest_chars.clone();
        rest_chars.next();
        match rest_chars.peek() {
            Some(&peek_ch) => predicate(peek_ch),
            None => false,
        }
    }
}

/// Scenarios that requires scanning multiple characters to determine the token.
enum Started {
    /// It has a shape of `(to_match, matched, unmatched)`, where token's type
    /// is `matched` if next char equals to `to_match`, else its type is `unmatched`.
    MatchNextChar(char, TokenKind, TokenKind),
    Slash,
    String,
    Number,
    IdentOrKeyword,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token<'src>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position > self.input.len() {
            // After EOF token is emitted, we "stop" the iterator by returning `None`.
            // Rust will stop the `for token in lexer {}` loop if `.next()` returns `None`.
            return None;
        }

        loop {
            if self.rest_chars.peek().is_none() {
                let eof_token = Some(Ok(Token::new(
                    TokenKind::Eof,
                    (self.position, self.position).into(),
                    TokenValue::None,
                )));
                self.position += 1;
                return eof_token;
            }

            let (ch, ch_len) = self.read_char()?;
            let curr_position = self.position;

            let single_token = |kind: TokenKind| {
                Some(Ok(Token::new(
                    kind,
                    // If we use `curr_position` instead of `self.position`,
                    // the closure borrows `self` immutably. Then, we cannot call
                    // `&mut self` methods due to borrow-checker rules.
                    (curr_position - ch_len, curr_position).into(),
                    TokenValue::None,
                )))
            };

            let started = match ch {
                '(' => return single_token(TokenKind::LeftParen),
                ')' => return single_token(TokenKind::RightParen),
                '{' => return single_token(TokenKind::LeftBrace),
                '}' => return single_token(TokenKind::RightBrace),
                ',' => return single_token(TokenKind::Comma),
                ';' => return single_token(TokenKind::Semicolon),
                '.' => return single_token(TokenKind::Dot),
                '+' => return single_token(TokenKind::Plus),
                '-' => return single_token(TokenKind::Minus),
                '*' => return single_token(TokenKind::Star),
                '/' => Started::Slash,
                '=' => Started::MatchNextChar('=', TokenKind::EqualEqual, TokenKind::Equal),
                '!' => Started::MatchNextChar('=', TokenKind::BangEqual, TokenKind::Bang),
                '<' => Started::MatchNextChar('=', TokenKind::LessEqual, TokenKind::Less),
                '>' => Started::MatchNextChar('=', TokenKind::GreaterEqual, TokenKind::Greater),
                '"' => Started::String,
                c if c.is_whitespace() => continue,
                c if c.is_ascii_digit() => Started::Number,
                c if c.is_ascii_alphabetic() || c == '_' => Started::IdentOrKeyword,
                c => {
                    return Some(Err(SyntaxError::SingleTokenError {
                        token: c,
                        span: (self.position - ch_len, ch_len).into(), // (offset, length)
                    }));
                }
            };

            // `break match` = Return the match statement's value back to the loop
            break match started {
                Started::MatchNextChar(to_match, matched, unmatched) => {
                    if self.is_peek_char(to_match) {
                        let (_, matched_ch_len) = self.read_char()?;
                        Some(Ok(Token::new(
                            matched,
                            (self.position - matched_ch_len - ch_len, self.position).into(),
                            TokenValue::None,
                        )))
                    } else {
                        single_token(unmatched)
                    }
                }
                Started::Slash => {
                    if self.is_peek_char('/') {
                        // Ignore single line comments, we don't have a token for that
                        self.read_chars_while(|c| c != '\n');
                        continue;
                    } else {
                        single_token(TokenKind::Slash)
                    }
                }
                Started::String => {
                    let (str_content, str_content_len) = { self.read_chars_while(|c| c != '"') };

                    if self.is_peek_char('"') {
                        self.read_char(); // Consume ending `"``
                        Some(Ok(Token::new(
                            TokenKind::String,
                            (self.position - str_content_len - 2, self.position).into(),
                            TokenValue::String(str_content),
                        )))
                    } else {
                        Some(Err(SyntaxError::UnterminatedStringError {
                            span: (self.position - str_content_len - 1, str_content_len + 1).into(),
                        }))
                    }
                }
                Started::Number => {
                    let start_pos = self.position - ch_len; // since first digit is consumed

                    self.read_chars_while(|c| c.is_ascii_digit());

                    // Trailing decimal point (e.g. `123.`) is considered invalid,
                    // and we treat it as two tokens: "123" and "."
                    if self.is_peek_char('.') && self.is_next_peek_char(|c| c.is_ascii_digit()) {
                        self.read_char(); // Consume `.`
                        self.read_chars_while(|c| c.is_ascii_digit());
                    }

                    let span: Span = (start_pos, self.position).into();
                    let raw = self.input.get(Range::<usize>::from(span))?;
                    let value: f64 = raw.parse().unwrap();

                    Some(Ok(Token::new(
                        TokenKind::Number,
                        span,
                        TokenValue::Number(value),
                    )))
                }
                Started::IdentOrKeyword => {
                    let start_pos = self.position - ch_len; // since first char is consumed

                    self.read_chars_while(|c| c.is_ascii_alphanumeric() || c == '_');

                    let span: Span = (start_pos, self.position).into();
                    let word = self.input.get(Range::<usize>::from(span))?;
                    let kind = TokenKind::match_keyword(word);

                    Some(Ok(Token::new(kind, span, TokenValue::None)))
                }
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::{error::ErrorFormat, token::TokenKind};
    use pretty_assertions::assert_eq;

    fn assert_tokens(input: &str, expected: &Vec<&str>) {
        let mut lexer = Lexer::new(input);
        for expected_token_str in expected {
            match lexer.next().unwrap() {
                Ok(token) => assert_eq!(token.to_string(input), *expected_token_str),
                Err(err) => {
                    err.print_error(input, &ErrorFormat::Pretty);
                    panic!("Encountered a SyntaxError");
                }
            }
        }
        assert!(lexer.next().is_none()); // After EOF, lexer should return None
    }

    /// Asserts the output of `lexer.next()` equals the `expected` [SyntaxError]
    fn assert_syntax_error(input: Option<Result<Token, SyntaxError>>, expected: &SyntaxError) {
        let error = input.unwrap().expect_err("should be a SyntaxError");
        assert_eq!(error, *expected);
    }

    #[test]
    fn test_read_chars_while() {
        let input = "abcd123";
        let mut lexer = Lexer::new(input);

        // Consume 'abcd'
        let result = lexer.read_chars_while(|c| c.is_alphabetic());
        assert_eq!(result, ("abcd", 4));
        assert_eq!(lexer.rest_chars.peek(), Some(&'1'));
        assert_eq!(lexer.position, 4);

        // Predicate returns false immediately -> No-op
        let result = lexer.read_chars_while(|c| c.is_alphabetic());
        assert_eq!(result, ("", 0));
        assert_eq!(lexer.rest_chars.peek(), Some(&'1'));
        assert_eq!(lexer.position, 4);

        // Consume '123'
        let result = lexer.read_chars_while(|c| c.is_numeric());
        assert_eq!(result, ("123", 3));
        assert_eq!(lexer.rest_chars.peek(), None);
        assert_eq!(lexer.position, 7);

        // Next token is EOF
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::new(
                TokenKind::Eof,
                (7, 7).into(),
                TokenValue::None
            )))
        );
        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_is_next_peek_char() {
        let input = "ab1";
        let mut lexer = Lexer::new(input);

        assert!(lexer.is_next_peek_char(|c| c == 'b'));
        assert_eq!(lexer.rest_chars.peek(), Some(&'a')); // Peek doesn't advance lexer

        lexer.read_char(); // Read 'a'
        assert!(lexer.is_next_peek_char(|c| c == '1'));

        lexer.read_char(); // Read 'b'
        assert!(!lexer.is_next_peek_char(|_| true));

        let empty_input = "";
        let mut lexer = Lexer::new(empty_input);
        assert!(!lexer.is_next_peek_char(|_| true));
        assert_eq!(lexer.rest_chars.peek(), None);
    }

    #[test]
    fn test_empty() {
        let input = "";
        let expected = vec!["EOF  null"];
        assert_tokens(input, &expected);
    }

    #[test]
    fn test_single_char_tokens() {
        let input = r#"({*.,;+*})"#;
        let expected = vec![
            "LEFT_PAREN ( null",
            "LEFT_BRACE { null",
            "STAR * null",
            "DOT . null",
            "COMMA , null",
            "SEMICOLON ; null",
            "PLUS + null",
            "STAR * null",
            "RIGHT_BRACE } null",
            "RIGHT_PAREN ) null",
            "EOF  null",
        ];
        assert_tokens(input, &expected);
    }

    #[test]
    fn test_assignment_and_equality() {
        let input = r#"={===}="#;
        let expected = vec![
            "EQUAL = null",
            "LEFT_BRACE { null",
            "EQUAL_EQUAL == null",
            "EQUAL = null",
            "RIGHT_BRACE } null",
            "EQUAL = null",
            "EOF  null",
        ];
        assert_tokens(input, &expected);
    }

    #[test]
    fn test_negation_and_inequality() {
        let input = r#"!!===!"#;
        let expected = vec![
            "BANG ! null",
            "BANG_EQUAL != null",
            "EQUAL_EQUAL == null",
            "BANG ! null",
            "EOF  null",
        ];
        assert_tokens(input, &expected);
    }

    #[test]
    fn test_relational() {
        let input = r#"<<==>>=="#;
        let expected = vec![
            "LESS < null",
            "LESS_EQUAL <= null",
            "EQUAL = null",
            "GREATER > null",
            "GREATER_EQUAL >= null",
            "EQUAL = null",
            "EOF  null",
        ];
        assert_tokens(input, &expected);
    }

    #[test]
    fn test_single_line_comments() {
        let empty_comment = "//";
        let expected = vec!["EOF  null"];
        assert_tokens(empty_comment, &expected);

        let comment_till_eof = "// Hi";
        let expected = vec!["EOF  null"];
        assert_tokens(comment_till_eof, &expected);

        let multi_line_with_comments = "(// Hi\n.// Bye\n+";
        let expected = vec![
            "LEFT_PAREN ( null",
            "DOT . null",
            "PLUS + null",
            "EOF  null",
        ];
        assert_tokens(multi_line_with_comments, &expected);

        let not_comment = "/(";
        let expected = vec!["SLASH / null", "LEFT_PAREN ( null", "EOF  null"];
        assert_tokens(not_comment, &expected);
    }

    #[test]
    fn test_whitespaces() {
        let input = "    (\t\r
        )";
        let expected = vec!["LEFT_PAREN ( null", "RIGHT_PAREN ) null", "EOF  null"];
        assert_tokens(input, &expected);
    }

    #[test]
    fn test_strings() {
        let string_only = r#""foo baz!""#;
        let expected = vec!["STRING \"foo baz!\" foo baz!", "EOF  null"];
        assert_tokens(string_only, &expected);

        let string_in_middle = r#"("foo baz")"#;
        let expected = vec![
            "LEFT_PAREN ( null",
            "STRING \"foo baz\" foo baz",
            "RIGHT_PAREN ) null",
            "EOF  null",
        ];
        assert_tokens(string_in_middle, &expected);

        let non_ascii_string = r#"("hi 你好")"#; // '你' and '好' has UTF-8 length of 3
        let expected = vec![
            "LEFT_PAREN ( null",
            "STRING \"hi 你好\" hi 你好",
            "RIGHT_PAREN ) null",
            "EOF  null",
        ];
        assert_tokens(non_ascii_string, &expected);

        let multi_line_string = "\"foo\nbar\"";
        let expected = vec!["STRING \"foo\nbar\" foo\nbar", "EOF  null"];
        assert_tokens(multi_line_string, &expected);
    }

    #[test]
    fn test_number() {
        let valid_numbers = "0 9 123 123.456 65.000 0.00001 0123";
        let expected = vec![
            "NUMBER 0 0.0",
            "NUMBER 9 9.0",
            "NUMBER 123 123.0",
            "NUMBER 123.456 123.456",
            "NUMBER 65.000 65.0",
            "NUMBER 0.00001 0.00001",
            "NUMBER 0123 123.0", // leading zeros are ignored
            "EOF  null",
        ];
        assert_tokens(valid_numbers, &expected);

        let invalid_numbers = ".456 123. 12,0 12.34.56";
        let expected = vec![
            "DOT . null",
            "NUMBER 456 456.0",
            "NUMBER 123 123.0",
            "DOT . null",
            "NUMBER 12 12.0",
            "COMMA , null",
            "NUMBER 0 0.0",
            "NUMBER 12.34 12.34",
            "DOT . null",
            "NUMBER 56 56.0",
            "EOF  null",
        ];
        assert_tokens(invalid_numbers, &expected);

        let non_ascii_number = "¹";
        let mut lexer = Lexer::new(non_ascii_number);
        assert_syntax_error(
            lexer.next(),
            &SyntaxError::SingleTokenError {
                token: '¹',
                span: (0, '¹'.len_utf8()).into(),
            },
        );
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let ident_only = "andy formless fo _ _123 _abc ab123
        abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";
        let expected = vec![
            "IDENTIFIER andy null",
            "IDENTIFIER formless null",
            "IDENTIFIER fo null",
            "IDENTIFIER _ null",
            "IDENTIFIER _123 null",
            "IDENTIFIER _abc null",
            "IDENTIFIER ab123 null",
            "IDENTIFIER abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_ null",
            "EOF  null",
        ];
        assert_tokens(ident_only, &expected);

        let ident_with_other_tokens = "foo.bar;";
        let expected = vec![
            "IDENTIFIER foo null",
            "DOT . null",
            "IDENTIFIER bar null",
            "SEMICOLON ; null",
            "EOF  null",
        ];
        assert_tokens(ident_with_other_tokens, &expected);

        let invalid_ident = "1foo"; // identifiers shouldn't start with a number
        let expected = vec!["NUMBER 1 1.0", "IDENTIFIER foo null", "EOF  null"];
        assert_tokens(invalid_ident, &expected);

        let keywords =
            "and class else false for fun if nil or print return super this true var while";
        let expected = vec![
            "AND and null",
            "CLASS class null",
            "ELSE else null",
            "FALSE false null",
            "FOR for null",
            "FUN fun null",
            "IF if null",
            "NIL nil null",
            "OR or null",
            "PRINT print null",
            "RETURN return null",
            "SUPER super null",
            "THIS this null",
            "TRUE true null",
            "VAR var null",
            "WHILE while null",
            "EOF  null",
        ];
        assert_tokens(keywords, &expected);

        let ident_and_keywords = "class CLASS fun fn true truee";
        let expected = vec![
            "CLASS class null",
            "IDENTIFIER CLASS null",
            "FUN fun null",
            "IDENTIFIER fn null",
            "TRUE true null",
            "IDENTIFIER truee null",
            "EOF  null",
        ];
        assert_tokens(ident_and_keywords, &expected);
    }

    #[test]
    fn test_single_token_error() {
        let input = r#",.$("#;
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Comma);
        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Dot);
        assert_syntax_error(
            lexer.next(),
            &SyntaxError::SingleTokenError {
                token: '$',
                span: (2, 1).into(), // (offset, length)
            },
        );
        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::LeftParen);
    }

    #[test]
    fn test_unterminated_string_error() {
        let input = r#"."bar"#;
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Dot);
        assert_syntax_error(
            lexer.next(),
            &SyntaxError::UnterminatedStringError {
                span: (1, 4).into(),
            },
        )
    }
}
