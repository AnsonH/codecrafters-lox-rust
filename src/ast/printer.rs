use super::{
    expression::{Binary, ExprVisitor, Grouping, Identifier, LiteralExpr, Unary},
    Expr, Literal,
};

/// Prints AST in a [prefix notation](https://en.wikipedia.org/wiki/Polish_notation)
/// format that matches the [official Lox test specs](https://github.com/munificent/craftinginterpreters/blob/master/test/expressions/parse.lox).
///
/// # Example
/// ```
/// use rust_lox::ast::printer::AstPrefixPrinter;
/// use rust_lox::parser::Parser;
///
/// let input = "1 + 2 * 3";
///
/// let parser = Parser::new(input);
/// let expr = parser.parse_expression().unwrap();
///
/// let mut printer = AstPrefixPrinter;
/// assert_eq!(printer.print(&expr), "(+ 1.0 (* 2.0 3.0))".to_string());
/// ```
pub struct AstPrefixPrinter;

impl AstPrefixPrinter {
    /// Main entry point, which prints the expression in a
    /// [prefix notation](https://en.wikipedia.org/wiki/Polish_notation).
    ///
    /// See [AstPrefixPrinter] for example.
    pub fn print(&mut self, expr: &Expr) -> String {
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
