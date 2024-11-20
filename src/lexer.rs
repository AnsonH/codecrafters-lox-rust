use crate::token::{Token, TokenKind};

pub struct Lexer<'de> {
    /// The input program.
    input: &'de str,
    /// Rest of the input that we haven't scanned.
    rest: &'de str,
    /// Current byte position in the input.
    position: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Lexer {
            input,
            rest: input,
            position: 0,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    // TODO: Change this to Result
    type Item = Token<'de>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position > self.input.len() {
            return None;
        }
        if self.rest.is_empty() {
            self.position += 1;
            return Some(Token::new(TokenKind::Eof, ""));
        }

        // TODO: Maybe extract this logic into `advance` / `read_char`?
        let mut chars = self.rest.chars();
        let ch: char = chars.next()?;
        let ch_len = ch.len_utf8();
        let ch_str = &self.rest[..ch_len]; // "converts" `char` to `&str`

        self.rest = chars.as_str();
        self.position += ch_len;

        match ch {
            '(' => return Some(Token::new(TokenKind::LeftParen, ch_str)),
            ')' => return Some(Token::new(TokenKind::RightParen, ch_str)),
            _ => todo!("Handle unknown token"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    fn assert_token_kinds(input: &str, expected: Vec<TokenKind>) {
        let mut lexer = Lexer::new(input);
        for expected_kind in expected {
            let token = lexer.next().unwrap();
            assert_eq!(token.kind, expected_kind);
        }
        assert!(lexer.next().is_none()); // After EOF, lexer should return None
    }

    #[test]
    fn test_empty() {
        assert_token_kinds("", vec![TokenKind::Eof]);
    }

    #[test]
    fn test_parentheses() {
        let input = "(()";
        let expected = vec![
            TokenKind::LeftParen,
            TokenKind::LeftParen,
            TokenKind::RightParen,
            TokenKind::Eof,
        ];
        assert_token_kinds(input, expected);
    }
}
