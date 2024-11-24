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

        let ch: char = self.rest_chars.next()?;
        let ch_len = ch.len_utf8();
        let ch_str = self.input.get(self.position..(self.position + ch_len))?;
        self.position += ch_len;

        let just = |kind: TokenKind| Some(Ok(Token::new(kind, ch_str)));

        match ch {
            '(' => just(TokenKind::LeftParen),
            ')' => just(TokenKind::RightParen),
            '{' => just(TokenKind::LeftBrace),
            '}' => just(TokenKind::RightBrace),
            ',' => just(TokenKind::Comma),
            ';' => just(TokenKind::Semicolon),
            '.' => just(TokenKind::Dot),
            '+' => just(TokenKind::Plus),
            '-' => just(TokenKind::Minus),
            '*' => just(TokenKind::Star),
            '/' => just(TokenKind::Slash),
            c => Some(Err(SyntaxError::SingleTokenError {
                token: c,
                err_span: (self.position - ch_len, ch_len).into(), // (offset, length)
            })),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::token::TokenKind;

    fn assert_token_kinds(input: &str, expected: Vec<TokenKind>) {
        let mut lexer = Lexer::new(input);
        for expected_kind in expected {
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.kind, expected_kind);
        }
        assert!(lexer.next().is_none()); // After EOF, lexer should return None
    }

    #[test]
    fn test_empty() {
        assert_token_kinds("", vec![TokenKind::Eof]);
    }

    #[test]
    fn test_single_char_tokens() {
        let input = r#"({*.,+*})"#;
        let expected = vec![
            TokenKind::LeftParen,
            TokenKind::LeftBrace,
            TokenKind::Star,
            TokenKind::Dot,
            TokenKind::Comma,
            TokenKind::Plus,
            TokenKind::Star,
            TokenKind::RightBrace,
            TokenKind::RightParen,
            TokenKind::Eof,
        ];
        assert_token_kinds(input, expected);
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
