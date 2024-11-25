use crate::error::SyntaxError;
use crate::token::{Token, TokenKind};
use std::iter::Peekable;
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
}

/// Scenarios that requires scanning multiple characters to determine the token.
enum Started<'src> {
    /// Token kind is `matched` if next char is `to_match`, else is `unmatched`.
    MatchNextChar {
        to_match: char,
        matched: TokenKind<'src>,
        unmatched: TokenKind<'src>,
    },
    Slash,
    String,
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
                self.position += 1;
                return Some(Ok(Token::new(TokenKind::Eof, "")));
            }

            let (ch, ch_len) = self.read_char()?;
            let ch_str: &'src str = self.input.get((self.position - ch_len)..self.position)?;

            let just = |kind: TokenKind<'src>| Some(Ok(Token::new(kind, ch_str)));

            let started = match ch {
                '(' => return just(TokenKind::LeftParen),
                ')' => return just(TokenKind::RightParen),
                '{' => return just(TokenKind::LeftBrace),
                '}' => return just(TokenKind::RightBrace),
                ',' => return just(TokenKind::Comma),
                ';' => return just(TokenKind::Semicolon),
                '.' => return just(TokenKind::Dot),
                '+' => return just(TokenKind::Plus),
                '-' => return just(TokenKind::Minus),
                '*' => return just(TokenKind::Star),
                '/' => Started::Slash,
                '=' => Started::MatchNextChar {
                    to_match: '=',
                    matched: TokenKind::EqualEqual,
                    unmatched: TokenKind::Equal,
                },
                '!' => Started::MatchNextChar {
                    to_match: '=',
                    matched: TokenKind::BangEqual,
                    unmatched: TokenKind::Bang,
                },
                '<' => Started::MatchNextChar {
                    to_match: '=',
                    matched: TokenKind::LessEqual,
                    unmatched: TokenKind::Less,
                },
                '>' => Started::MatchNextChar {
                    to_match: '=',
                    matched: TokenKind::GreaterEqual,
                    unmatched: TokenKind::Greater,
                },
                '"' => Started::String,
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(SyntaxError::SingleTokenError {
                        token: c,
                        err_span: (self.position - ch_len, ch_len).into(), // (offset, length)
                    }));
                }
            };

            // `break match` = Return the match statement's value back to the loop
            break match started {
                Started::MatchNextChar {
                    to_match,
                    matched,
                    unmatched,
                } => {
                    if self.is_peek_char(to_match) {
                        let (_, next_ch_len) = self.read_char()?;
                        let lexeme = self
                            .input
                            .get((self.position - next_ch_len - ch_len)..self.position)?;

                        Some(Ok(Token::new(matched, lexeme)))
                    } else {
                        just(unmatched)
                    }
                }
                Started::Slash => {
                    if self.is_peek_char('/') {
                        // Ignore single line comments, we don't have a token for that
                        self.read_chars_while(|c| c != '\n');
                        continue;
                    } else {
                        just(TokenKind::Slash)
                    }
                }
                Started::String => {
                    let (str_content, str_content_len) = { self.read_chars_while(|c| c != '"') };

                    if self.is_peek_char('"') {
                        self.read_char(); // Consume ending `"``

                        // Lexeme includes the enclosing `"`
                        let lexeme = self
                            .input
                            .get((self.position - str_content_len - 2)..self.position)?;

                        Some(Ok(Token::new(TokenKind::String(str_content), lexeme)))
                    } else {
                        Some(Err(SyntaxError::UnterminatedStringError {
                            // The `1` below is the length of starting `"`
                            err_span: (self.position - str_content_len - 1, str_content_len + 1)
                                .into(),
                        }))
                    }
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
                Ok(token) => assert_eq!(token.to_string(), *expected_token_str),
                Err(err) => {
                    err.print_error(input, &ErrorFormat::Pretty);
                    panic!("Encountered a SyntaxError");
                }
            }
        }
        assert!(lexer.next().is_none()); // After EOF, lexer should return None
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
        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Eof);
        assert!(lexer.next().is_none());
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
    fn test_single_token_error() {
        let input = r#",.$("#;
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Comma);
        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Dot);

        let error = lexer.next().unwrap().expect_err("should be a SyntaxError");
        assert_eq!(
            error,
            SyntaxError::SingleTokenError {
                token: '$',
                err_span: (2, 1).into()
            }
        );

        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::LeftParen);
    }

    #[test]
    fn test_unterminated_string_error() {
        let input = r#"."bar"#;
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Dot);

        let error = lexer.next().unwrap().expect_err("should be a SyntaxError");
        assert_eq!(
            error,
            SyntaxError::UnterminatedStringError {
                err_span: (1, 4).into()
            }
        )
    }
}
