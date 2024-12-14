use super::{expression::*, statement::*, Expr, Literal, Program};

/// Prints AST in a [prefix notation](https://en.wikipedia.org/wiki/Polish_notation)
/// format that matches the [official Lox test specs](https://github.com/munificent/craftinginterpreters/blob/master/test/expressions/parse.lox).
///
/// The official specs only specified the format for expressions, so the printing
/// of statements uses a custom format.
///
/// # Example
/// ```
/// use rust_lox::ast::printer::AstPrefixPrinter;
/// use rust_lox::parser::Parser;
///
/// let input = "print 1 + 2;";
///
/// let parser = Parser::new(input);
/// let program = parser.parse_program().unwrap();
///
/// let mut printer = AstPrefixPrinter;
/// assert_eq!(
///     printer.print_program(&program),
///     vec!["(print (+ 1.0 2.0))".to_string()],
/// );
/// ```
pub struct AstPrefixPrinter;

impl AstPrefixPrinter {
    /// Prints each statement of the program's body as a string.
    pub fn print_program(&mut self, program: &Program) -> Vec<String> {
        program.body.iter().map(|stmt| stmt.accept(self)).collect()
    }

    /// Prints an expression in a
    /// [prefix notation](https://en.wikipedia.org/wiki/Polish_notation).
    ///
    /// For an example, see [AstPrefixPrinter]'s "Example" section.
    pub fn print_expression(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }

    fn parenthesize(&mut self, name: &str, exprs: Vec<&Expr>) -> String {
        let mut output = String::from("(");
        output.push_str(name);
        for expr in exprs {
            output.push(' ');
            output.push_str(&expr.accept(self));
        }
        output.push(')');
        output
    }
}

impl StmtVisitor for AstPrefixPrinter {
    type Value = String;

    fn visit_expression_stmt(&mut self, expr: &ExpressionStatement) -> Self::Value {
        expr.expression.accept(self)
    }

    fn visit_print_stmt(&mut self, expr: &PrintStatement) -> Self::Value {
        self.parenthesize("print", vec![&expr.expression])
    }

    fn visit_var_stmt(&mut self, expr: &VarStatement) -> Self::Value {
        let mut output = format!("(var {}", expr.name.name);
        if let Some(initializer) = &expr.initializer {
            output.push(' ');
            output.push_str(&initializer.accept(self));
        }
        output.push(')');
        output
    }
}

impl ExprVisitor for AstPrefixPrinter {
    type Value = String;

    fn visit_binary_expr(&mut self, expr: &Binary) -> Self::Value {
        self.parenthesize(&expr.operator.to_string(), vec![&expr.left, &expr.right])
    }

    fn visit_grouping_expr(&mut self, expr: &Grouping) -> Self::Value {
        self.parenthesize("group", vec![&expr.expression])
    }

    fn visit_identifier_expr(&mut self, expr: &Identifier) -> Self::Value {
        expr.name.to_string()
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Value {
        match expr.value {
            Literal::Boolean(b) => b.to_string(),
            Literal::Nil => "nil".to_string(),
            Literal::Number(n) => {
                if n.fract() == 0_f64 {
                    // Tests requires integers to be print as N.0
                    format!("{n}.0")
                } else {
                    n.to_string()
                }
            }
            Literal::String(s) => s.to_string(),
        }
    }

    fn visit_unary_expr(&mut self, expr: &Unary) -> Self::Value {
        self.parenthesize(&expr.operator.to_string(), vec![&expr.right])
    }
}
