use crate::{
    ast::{Expr, Literal, Program},
    error::SyntaxError,
    lexer::Lexer,
    token::{Keyword, Token},
};

pub struct Parser<'src> {
    /// The input program.
    input: &'src str,
    lexer: Lexer<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            input,
            lexer: Lexer::new(input),
        }
    }

    /// Main entry point.
    ///
    /// # Returns
    ///
    /// The Abstract Syntax Tree (AST) of the program if there are no syntax errors.
    /// For simplicity, the parser immediately stops and returns `Err(SyntaxError)`
    /// if it encounters any syntax errors.
    pub fn parse(mut self) -> Result<Program, SyntaxError> {
        todo!()
    }

    // TODO: Extract expression-related parsing into new file

    // TODO: Add docs
    pub fn parse_expression(&mut self, min_precedence: u8) -> Result<Expr<'src>, SyntaxError> {
        let lhs_token = match self.lexer.next() {
            Some(Ok(Token::Eof)) | None => return Ok(Expr::Literal(Literal::Nil)),
            Some(Err(err)) => return Err(err),
            Some(Ok(token)) => token,
        };

        let lhs = match lhs_token {
            Token::Keyword(Keyword::True) => Expr::Literal(Literal::Boolean(true)),
            Token::Keyword(Keyword::False) => Expr::Literal(Literal::Boolean(false)),
            Token::Keyword(Keyword::Nil) => Expr::Literal(Literal::Nil),
            _ => todo!(),
        };

        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ErrorFormat;

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert_parsed_expression(input: &str, expected: &str) {
        let mut parser = Parser::new(input);
        match parser.parse_expression(0) {
            Ok(expr) => assert_eq!(expr.to_string(), expected),
            Err(err) => {
                err.print_error(input, &ErrorFormat::Pretty);
                panic!("Encountered a SyntaxError");
            }
        }
    }

    #[test]
    fn test_empty() {
        assert_parsed_expression("", "nil");
    }

    #[test]
    fn test_literal() {
        assert_parsed_expression("true", "true");
        assert_parsed_expression("false", "false");
        assert_parsed_expression("nil", "nil");
    }

    #[test]
    fn test_arithmetic_operators() {
        // TODO
    }
}
