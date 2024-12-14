use miette::Result;

use crate::{
    ast::{statement::*, Stmt},
    token::TokenKind,
};

use super::Parser;

impl<'src> Parser<'src> {
    /// Entry point for parsing a statement.
    pub(crate) fn parse_statement(&mut self) -> Result<Stmt<'src>> {
        match self.cur_kind() {
            TokenKind::Print => self.parse_print_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub(crate) fn parse_expression_statement(&mut self) -> Result<Stmt<'src>> {
        let expression = self.parse_expr(0)?;
        self.expect(TokenKind::Semicolon)?;

        let span = expression.span().expand_right(1);
        Ok(Stmt::ExpressionStmt(
            ExpressionStmt { expression, span }.into(),
        ))
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
    use crate::{ast::printer::AstPrefixPrinter, error::SyntaxError, span::Span};

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert(input: &'static str, expected: &[&str]) {
        let parser = Parser::new(input);
        let mut printer = AstPrefixPrinter;

        match parser.parse_program() {
            Ok(expr) => assert_eq!(printer.print_program(&expr), expected),
            Err(report) => panic!(
                "Encountered error while parsing\n{:?}",
                report.with_source_code(input)
            ),
        }
    }

    fn assert_error(input: &str, expected: SyntaxError) {
        let parser = Parser::new(input);
        let report = parser
            .parse_program()
            .expect_err("Parser should emit error");

        match report.downcast_ref::<SyntaxError>() {
            Some(err) => assert_eq!(*err, expected),
            None => panic!("Parser should emit a SyntaxError"),
        }
    }

    #[test]
    fn test_print_statement() {
        assert("print 42;", &["(print 42.0)"]);
        assert("print 1 + 2;", &["(print (+ 1.0 2.0))"]);

        assert_error(
            "print",
            SyntaxError::MissingExpression {
                span: Span::new(5, 5),
            },
        );
        assert_error(
            "print 1",
            SyntaxError::UnexpectedToken {
                expected: ";".into(),
                actual: "EOF".into(),
                span: Span::new(7, 7),
            },
        );
    }
}
