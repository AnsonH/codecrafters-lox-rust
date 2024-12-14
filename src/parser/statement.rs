use miette::Result;

use crate::{
    ast::{statement::PrintStmt, Stmt},
    token::TokenKind,
};

use super::Parser;

impl<'src> Parser<'src> {
    /// Entry point for parsing a statement.
    pub(crate) fn parse_statement(&mut self) -> Result<Stmt<'src>> {
        dbg!(self.cur_kind());
        match self.cur_kind() {
            TokenKind::Print => self.parse_print_statement(),
            _ => todo!("parse expr statement or throw error?"),
        }
    }

    pub(crate) fn parse_print_statement(&mut self) -> Result<Stmt<'src>> {
        let keyword_span = self.cur_span();

        self.advance()?;
        let expression = self.parse_expr(0)?;
        self.expect(TokenKind::Semicolon)?;

        let span = self.cur_span().merge(&keyword_span);
        Ok(Stmt::PrintStmt(PrintStmt { expression, span }.into()))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{expression::LiteralExpr, Expr, Literal},
        span::Span,
    };

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert(input: &str, expected_body: &Vec<Stmt>) {
        let parser = Parser::new(input);
        let program = parser.parse_program().expect("no SyntaxError");
        assert_eq!(program.body, *expected_body);
    }

    #[test]
    fn test_print_statement() {
        let input = "print 42;";
        let body = vec![Stmt::PrintStmt(
            PrintStmt {
                expression: Expr::Literal(
                    LiteralExpr {
                        value: Literal::Number(42.0),
                        span: Span::new(6, 8),
                    }
                    .into(),
                ),
                span: Span::new(0, 9),
            }
            .into(),
        )];
        assert(input, &body);
    }
}
