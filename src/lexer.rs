use crate::error::SyntaxError;
use crate::token::{Token, TokenKind};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'de> {
    /// The input program.
    input: &'de str,
    /// Remaining characters of the input that the lexer hasn't scanned.
    ///
    /// [Peekable] is useful for peeking into future characters without consuming them.
    rest_chars: Peekable<Chars<'de>>,
    /// Current byte position in the input.
    position: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
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
    /// Returns a vector of chars that satisfies the predicate.
    fn read_chars_while<F>(&mut self, predicate: F) -> Vec<char>
    where
        F: Fn(char) -> bool,
    {
        let mut chars: Vec<char> = Vec::new();
        while let Some(&peek_ch) = self.rest_chars.peek() {
            if predicate(peek_ch) {
                let (ch, _) = self.read_char().unwrap();
                chars.push(ch);
            } else {
                break;
            }
        }
        chars
    }
}

/// Scenarios that requires scanning multiple characters to determine the token.
enum Started {
    /// Token kind is `matched` if next char is `to_match`, else is `unmatched`.
    MatchNextChar {
        to_match: char,
        matched: TokenKind,
        unmatched: TokenKind,
    },
    Slash,
    String,
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, SyntaxError>;

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
            let ch_str = self.input.get((self.position - ch_len)..self.position)?;

            let just = |kind: TokenKind| Some(Ok(Token::new(kind, ch_str)));

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
                    if self.rest_chars.peek() == Some(&to_match) {
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
                    if self.rest_chars.peek() == Some(&'/') {
                        self.read_chars_while(|ch| ch != '\n');
                        continue;
                    } else {
                        just(TokenKind::Slash)
                    }
                }
                Started::String => {
                    let str_content: String =
                        self.read_chars_while(|ch| ch != '"').into_iter().collect();
                    let str_content_len = str_content.len();

                    if let Some(&'"') = self.rest_chars.peek() {
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
    use crate::token::TokenKind;
    use pretty_assertions::assert_eq;

    fn assert_tokens(input: &str, expected: &Vec<&str>) {
        let mut lexer = Lexer::new(input);
        for expected_token_str in expected {
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.to_string(), *expected_token_str);
        }
        assert!(lexer.next().is_none()); // After EOF, lexer should return None
    }

    #[test]
    fn test_read_chars_while() {
        let mut lexer = Lexer::new("abc123");

        let chars = lexer.read_chars_while(|ch| ch.is_alphabetic());
        assert_eq!(chars, vec!['a', 'b', 'c']);
        assert_eq!(lexer.rest_chars.peek(), Some(&'1'));

        let chars = lexer.read_chars_while(|ch| ch.is_ascii_digit());
        assert_eq!(chars, vec!['1', '2', '3']);
        assert_eq!(lexer.next().unwrap().unwrap().kind, TokenKind::Eof);
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
