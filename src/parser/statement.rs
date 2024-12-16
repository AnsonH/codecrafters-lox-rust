use miette::Result;

use crate::{
    ast::{statement::*, Expr, Stmt},
    error::SyntaxError,
    token::TokenKind,
};

use super::Parser;

impl<'src> Parser<'src> {
    /// Entry point for parsing a statement.
    pub(crate) fn parse_statement(&mut self) -> Result<Stmt<'src>> {
        match self.cur_kind() {
            TokenKind::LeftBrace => self.parse_block_statement(),
            TokenKind::Print => self.parse_print_statement(),
            TokenKind::Var => self.parse_var_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub(crate) fn parse_block_statement(&mut self) -> Result<Stmt<'src>> {
        let mut statements: Vec<Stmt<'src>> = vec![];
        let left_brace_span = self.cur_span();

        self.advance()?;
        while !matches!(self.cur_kind(), TokenKind::RightBrace | TokenKind::Eof) {
            statements.push(self.parse_statement()?);
            self.advance()?;
        }
        if !self.is_cur_kind(TokenKind::RightBrace) {
            return Err(SyntaxError::UnclosedBlock {
                span: left_brace_span,
            }
            .into());
        }

        let span = self.cur_span().merge(&left_brace_span);
        Ok(Stmt::BlockStatement(
            BlockStatement { statements, span }.into(),
        ))
    }

    pub(crate) fn parse_expression_statement(&mut self) -> Result<Stmt<'src>> {
        let expression = self.parse_expr(0)?;
        self.expect(TokenKind::Semicolon)?;

        let span = expression.span().expand_right(1);
        Ok(Stmt::ExpressionStatement(
            ExpressionStatement { expression, span }.into(),
        ))
    }

    pub(crate) fn parse_print_statement(&mut self) -> Result<Stmt<'src>> {
        let print_keyword_span = self.cur_span();
        self.advance()?;

        let expression = self.parse_expr(0)?;
        self.expect(TokenKind::Semicolon)?;

        let span = self.cur_span().merge(&print_keyword_span);
        Ok(Stmt::PrintStatement(
            PrintStatement { expression, span }.into(),
        ))
    }

    pub(crate) fn parse_var_statement(&mut self) -> Result<Stmt<'src>> {
        let var_keyword_span = self.cur_span();
        self.advance()?;

        if !self.is_cur_kind(TokenKind::Identifier) {
            return Err(SyntaxError::MissingVariableName {
                span: self.cur_span(),
            }
            .into());
        }
        let Expr::Identifier(name) = self.parse_identifier()? else {
            unreachable!()
        };

        let initializer = if self.is_peek_kind(TokenKind::Equal) {
            self.advance()?; // Move to `=`
            self.advance()?; // Move to start of expression
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        self.expect(TokenKind::Semicolon)?;

        Ok(Stmt::VarStatement(
            VarStatement {
                ident: *name,
                initializer,
                span: self.cur_span().merge(&var_keyword_span),
            }
            .into(),
        ))
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

    #[test]
    fn test_var_statement() {
        assert("var foo = 1;", &["(var foo 1.0)"]);
        assert("var a = 1 + 2 * 3;", &["(var a (+ 1.0 (* 2.0 3.0)))"]);
        assert("var bar;", &["(var bar)"]);

        assert_error(
            "var",
            SyntaxError::MissingVariableName {
                span: Span::new(3, 3),
            },
        );
        assert_error(
            "var foo = ;",
            SyntaxError::MissingExpression {
                span: Span::new(10, 11),
            },
        );
        assert_error(
            "var x",
            SyntaxError::UnexpectedToken {
                expected: ";".into(),
                actual: "EOF".into(),
                span: Span::new(5, 5),
            },
        );
        assert_error(
            "var foo = 1",
            SyntaxError::UnexpectedToken {
                expected: ";".into(),
                actual: "EOF".into(),
                span: Span::new(11, 11),
            },
        );
    }

    #[test]
    fn test_block_statement() {
        assert(
            r"
            var a = 1;
            { var a = 1; var b = 2; }
            var c = 3;",
            &[
                "(var a 1.0)",
                "(begin (var a 1.0) (var b 2.0))",
                "(var c 3.0)",
            ],
        );
        assert(
            r"{{ var a = 1; }} var b = 2;",
            &["(begin (begin (var a 1.0)))", "(var b 2.0)"],
        );

        assert_error(
            r"{ var a = 1;",
            SyntaxError::UnclosedBlock {
                span: Span::new(0, 1),
            },
        );
    }
}
