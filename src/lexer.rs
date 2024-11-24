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
}

enum Started {
    /// Token kind is `matched` if next char is `to_match`, else is `unmatched`.
    MatchNextChar {
        to_match: char,
        matched: TokenKind,
        unmatched: TokenKind,
    },
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position > self.input.len() {
            // After EOF token is emitted, we "stop" the iterator by returning `None`.
            // Rust will stop the `for token in lexer {}` loop if `.next()` returns `None`.
            return None;
        }
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
            '/' => return just(TokenKind::Slash),
            '=' => Started::MatchNextChar {
                to_match: '=',
                matched: TokenKind::EqualEqual,
                unmatched: TokenKind::Equal,
            },
            c => {
                return Some(Err(SyntaxError::SingleTokenError {
                    token: c,
                    err_span: (self.position - ch_len, ch_len).into(), // (offset, length)
                }));
            }
        };

        match started {
            Started::MatchNextChar {
                to_match,
                matched,
                unmatched,
            } => {
                if matches!(self.rest_chars.peek(), Some(&c) if c == to_match) {
                    let (_, next_ch_len) = self.read_char()?;
                    let lexeme = self
                        .input
                        .get((self.position - next_ch_len - ch_len)..self.position)?;

                    Some(Ok(Token::new(matched, lexeme)))
                } else {
                    just(unmatched)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::token::TokenKind;

    fn assert_tokens(input: &str, expected: &Vec<&str>) {
        let mut lexer = Lexer::new(input);
        for expected_token_str in expected {
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.to_string(), *expected_token_str);
        }
        assert!(lexer.next().is_none()); // After EOF, lexer should return None
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
}
