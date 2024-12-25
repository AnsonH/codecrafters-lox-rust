use miette::Result;

use crate::{ast::*, error::SyntaxError, token::TokenKind, unwrap_ast_node};

use super::Parser;

impl Parser<'_> {
    pub(super) fn parse_declaration_statement(&mut self) -> Result<Stmt> {
        match self.cur_kind() {
            TokenKind::Class => todo!(),
            TokenKind::Fun => self.parse_function_declaration(),
            TokenKind::Var => self.parse_var_statement(),
            _ => self.parse_statement(),
        }
    }

    pub(super) fn parse_statement(&mut self) -> Result<Stmt> {
        match self.cur_kind() {
            TokenKind::For => self.parse_for_statement(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::LeftBrace => self.parse_block_statement(),
            TokenKind::Print => self.parse_print_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::While => self.parse_while_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub(super) fn parse_block_statement(&mut self) -> Result<Stmt> {
        let mut statements: Vec<Stmt> = vec![];

        let start_span = self.cur_span();
        self.consume(TokenKind::LeftBrace)?;

        while !matches!(self.cur_kind(), TokenKind::RightBrace | TokenKind::Eof) {
            statements.push(self.parse_declaration_statement()?);
            self.advance()?;
        }
        if !self.is_cur_kind(TokenKind::RightBrace) {
            return Err(SyntaxError::UnclosedBlock { span: start_span }.into());
        }

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::BlockStatement(
            BlockStatement { statements, span }.into(),
        ))
    }

    pub(super) fn parse_expression_statement(&mut self) -> Result<Stmt> {
        let expression = self.parse_expr(0)?;
        self.expect_peek(TokenKind::Semicolon)?;

        let span = expression.span().expand_right(1);
        Ok(Stmt::ExpressionStatement(
            ExpressionStatement { expression, span }.into(),
        ))
    }

    pub(super) fn parse_for_statement(&mut self) -> Result<Stmt> {
        let start_span = self.cur_span();
        self.consume(TokenKind::For)?;
        self.consume(TokenKind::LeftParen)?;

        let init = match self.cur_kind() {
            TokenKind::Semicolon => {
                self.consume(TokenKind::Semicolon)?;
                None
            }
            TokenKind::Var => {
                let var_stmt = unwrap_ast_node!(self.parse_var_statement()?, Stmt::VarStatement);
                // No need to consume `;` since it's done by `parse_var_statement`
                self.advance()?; // Move to first token after `;`
                Some(ForStatementInit::VarStatement(var_stmt))
            }
            _ => {
                let expr_stmt = unwrap_ast_node!(
                    self.parse_expression_statement()?,
                    Stmt::ExpressionStatement
                );
                self.advance()?;
                Some(ForStatementInit::ExpressionStatement(expr_stmt))
            }
        };

        let condition = match self.cur_kind() {
            TokenKind::Semicolon => None,
            _ => {
                let expr = self.parse_expr(0)?;
                self.advance()?; // Move to `;`
                Some(expr)
            }
        };
        self.consume(TokenKind::Semicolon)?;

        let update = match self.cur_kind() {
            TokenKind::RightParen => None,
            _ => {
                let expr = self.parse_expr(0)?;
                self.advance()?; // Move to `)`
                Some(expr)
            }
        };
        self.consume(TokenKind::RightParen)?;

        let body = self.parse_statement()?;

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::ForStatement(
            ForStatement {
                init,
                condition,
                update,
                body,
                span,
            }
            .into(),
        ))
    }

    pub(super) fn parse_function_declaration(&mut self) -> Result<Stmt> {
        let start_span = self.cur_span();
        self.consume(TokenKind::Fun)?;

        if !self.is_cur_kind(TokenKind::Identifier) {
            return Err(SyntaxError::UnexpectedFunctionName {
                span: self.cur_span(),
            }
            .into());
        }
        let name_token = self.cur_token();
        let name = *unwrap_ast_node!(self.parse_identifier()?, Expr::Identifier);

        self.expect_peek(TokenKind::LeftParen)?;

        let mut parameters: Vec<Identifier> = vec![];
        if !self.is_peek_kind(TokenKind::RightParen) {
            self.advance()?;
            loop {
                if parameters.len() >= Self::MAX_FUNCTION_PARAMETERS {
                    return Err(SyntaxError::TooManyParameters {
                        max_count: Self::MAX_FUNCTION_PARAMETERS,
                        span: name_token.span,
                    }
                    .into());
                }
                if !self.is_cur_kind(TokenKind::Identifier) {
                    return Err(SyntaxError::UnexpectedParameterName {
                        span: self.cur_span(),
                    }
                    .into());
                }

                let param = unwrap_ast_node!(self.parse_identifier()?, Expr::Identifier);
                parameters.push(*param);
                if !self.try_consume_peek(TokenKind::Comma)? {
                    break;
                }
            }
        }
        self.expect_peek(TokenKind::RightParen)?;

        self.expect_peek(TokenKind::LeftBrace)?;
        let body = *unwrap_ast_node!(self.parse_block_statement()?, Stmt::BlockStatement);

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::FunctionDeclaration(
            FunctionDeclaration {
                name,
                parameters,
                body,
                span,
            }
            .into(),
        ))
    }

    pub(super) fn parse_if_statement(&mut self) -> Result<Stmt> {
        let start_span = self.cur_span();
        self.consume(TokenKind::If)?;

        self.consume(TokenKind::LeftParen)?;
        let condition = self.parse_expr(0)?;
        self.advance()?;
        self.consume(TokenKind::RightParen)?;

        let then_branch = self.parse_statement()?;
        let else_branch = if self.try_consume_peek(TokenKind::Else)? {
            Some(self.parse_statement()?)
        } else {
            None
        };

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::IfStatement(
            IfStatement {
                condition,
                then_branch,
                else_branch,
                span,
            }
            .into(),
        ))
    }

    pub(super) fn parse_print_statement(&mut self) -> Result<Stmt> {
        let start_span = self.cur_span();
        self.consume(TokenKind::Print)?;

        let expression = self.parse_expr(0)?;
        self.expect_peek(TokenKind::Semicolon)?;

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::PrintStatement(
            PrintStatement { expression, span }.into(),
        ))
    }

    pub(super) fn parse_return_statement(&mut self) -> Result<Stmt> {
        let start_span = self.cur_span();
        self.consume(TokenKind::Return)?;

        let expression = if self.is_cur_kind(TokenKind::Semicolon) {
            None
        } else {
            let expr = self.parse_expr(0)?;
            self.expect_peek(TokenKind::Semicolon)?;
            Some(expr)
        };

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::ReturnStatement(
            ReturnStatement { expression, span }.into(),
        ))
    }

    pub(super) fn parse_var_statement(&mut self) -> Result<Stmt> {
        let start_span = self.cur_span();
        self.consume(TokenKind::Var)?;

        if !self.is_cur_kind(TokenKind::Identifier) {
            return Err(SyntaxError::MissingVariableName {
                span: self.cur_span(),
            }
            .into());
        }
        let name = unwrap_ast_node!(self.parse_identifier()?, Expr::Identifier);

        let initializer = if self.try_consume_peek(TokenKind::Equal)? {
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        self.expect_peek(TokenKind::Semicolon)?;

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::VarStatement(
            VarStatement {
                ident: *name,
                initializer,
                span,
            }
            .into(),
        ))
    }

    pub(super) fn parse_while_statement(&mut self) -> Result<Stmt> {
        let start_span = self.cur_span();
        self.consume(TokenKind::While)?;

        self.consume(TokenKind::LeftParen)?;
        let condition = self.parse_expr(0)?;
        self.advance()?;
        self.consume(TokenKind::RightParen)?;

        let body = self.parse_statement()?;

        let span = self.cur_span().merge(&start_span);
        Ok(Stmt::WhileStatement(
            WhileStatement {
                condition,
                body,
                span,
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

    #[test]
    fn test_if_statement() {
        assert("if (x > 1) print x;", &["(if (> x 1.0) (print x))"]);
        assert(
            r"if (x > 1) { x = 2; print x; }",
            &["(if (> x 1.0) (begin (assign x 2.0) (print x)))"],
        );
        assert(
            "if (x > 1) print x; else print 0;",
            &["(if (> x 1.0) (print x) (print 0.0))"],
        );
        assert(
            r#"
            var stage = "unknown";
            var age = 2;
            if (age >= 18) { stage = "adult"; } 
            else { stage = "child"; }
            print stage;"#,
            &[
                "(var stage unknown)",
                "(var age 2.0)",
                "(if (>= age 18.0) (begin (assign stage adult)) (begin (assign stage child)))",
                "(print stage)",
            ],
        );
    }

    #[test]
    fn test_while_statement() {
        assert(
            "while (foo < 3) print foo;",
            &["(while (< foo 3.0) (print foo))"],
        );
        assert(
            r"
            var product = 1;
            var i = 1;
            while (i <= 5) {
                product = product * i;
                i = i + 1;
            }
            print product;",
            &[
                "(var product 1.0)",
                "(var i 1.0)",
                "(while (<= i 5.0) (begin (assign product (* product i)) (assign i (+ i 1.0))))",
                "(print product)",
            ],
        );
    }

    #[test]
    fn test_for_statement() {
        assert(
            "for (var x = 0; x < 5; x = x + 1) print x;",
            &["(for (var x 0.0) (< x 5.0) (assign x (+ x 1.0)) (print x))"],
        );
        assert(
            "for (; x < 5; x = x + 1) print x;",
            &["(for (< x 5.0) (assign x (+ x 1.0)) (print x))"],
        );
        assert(
            "for (;; x = x + 1) print x;",
            &["(for (assign x (+ x 1.0)) (print x))"],
        );
        assert("for (;;) print x;", &["(for (print x))"]);
    }

    #[test]
    fn test_function_declaration() {
        assert(r"fun foo() {}", &["(fun foo () (begin))"]);
        assert(r"fun foo(a) {}", &["(fun foo (a) (begin))"]);
        assert(r"fun foo(a, b, c) {}", &["(fun foo (a b c) (begin))"]);
        assert(
            r"fun foo(a, b, c) { print a; print b; }",
            &["(fun foo (a b c) (begin (print a) (print b)))"],
        );

        assert_error(
            r"fun 10() {}",
            SyntaxError::UnexpectedFunctionName {
                span: Span::new(4, 6),
            },
        );
        assert_error(
            r"fun foo(a, 10) {}",
            SyntaxError::UnexpectedParameterName {
                span: Span::new(11, 13),
            },
        );
    }

    #[test]
    fn test_return_statement() {
        assert("return;", &["(return)"]);
        assert("return 1 + 2 * 3;", &["(return (+ 1.0 (* 2.0 3.0)))"]);
    }
}
