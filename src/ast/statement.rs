use crate::span::Span;

use super::Expr;

/// Statement produces side effects, and doesn't evaluate to a value.
///
/// # Optimization
///
/// This enum's size is optimized by `Box`-ing all enum variants.
/// See the [oxc AST guide](https://oxc.rs/docs/learn/parser_in_rust/ast.html#enum-size).
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'src> {
    ExpressionStatement(Box<ExpressionStatement<'src>>),
    PrintStatement(Box<PrintStatement<'src>>),
}

impl<'src> Stmt<'src> {
    /// Accepts a visitor implementing [StmtVisitor] to visit the expression
    /// and perform operations based on the expression's type.
    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::Value {
        match self {
            Stmt::ExpressionStatement(expr) => visitor.visit_expression_stmt(expr),
            Stmt::PrintStatement(expr) => visitor.visit_print_stmt(expr),
        }
    }

    /// Gets the [Span] of the current node.
    pub fn span(&self) -> Span {
        match self {
            Self::ExpressionStatement(e) => e.span,
            Self::PrintStatement(p) => p.span,
        }
    }
}

/// Visitor for [Stmt] AST nodes.
///
/// The visitor pattern allows adding new operations to AST nodes without having
/// to modify them. An example implementor would be an AST printer, where it
/// specifies how to print each expression node.
///
/// See:
/// - [Rust Design Patterns - Visitor](https://rust-unofficial.github.io/patterns/patterns/behavioural/visitor.html)
/// - [Visitor in Rust](https://refactoring.guru/design-patterns/visitor/rust/example)
pub trait StmtVisitor {
    type Value;

    fn visit_expression_stmt(&mut self, expr: &ExpressionStatement) -> Self::Value;
    fn visit_print_stmt(&mut self, expr: &PrintStatement) -> Self::Value;
}

/// Syntax: `<expression>;`
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement<'src> {
    pub expression: Expr<'src>,
    pub span: Span,
}

/// Syntax: `print <expression>;`
#[derive(Debug, Clone, PartialEq)]
pub struct PrintStatement<'src> {
    pub expression: Expr<'src>,
    pub span: Span,
}
