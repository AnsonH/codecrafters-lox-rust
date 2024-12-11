use crate::span::Span;

use super::{BinaryOperator, UnaryOperator};

/// Expression produces a value.
///
/// # Optimization
///
/// This enum's size is optimized by `Box`-ing all enum variants.
/// See the [oxc AST guide](https://oxc.rs/docs/learn/parser_in_rust/ast.html#enum-size).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Binary(Box<Binary<'src>>),
    Grouping(Box<Grouping<'src>>),
    Identifier(Box<Identifier<'src>>),
    Literal(Box<LiteralExpr<'src>>),
    Unary(Box<Unary<'src>>),
}

impl<'src> Expr<'src> {
    /// Accepts a visitor implementing [ExprVisitor] to visit the expression
    /// and perform operations based on the expression's type.
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Value {
        match self {
            Expr::Binary(expr) => visitor.visit_binary_expr(expr),
            Expr::Grouping(expr) => visitor.visit_grouping_expr(expr),
            Expr::Identifier(expr) => visitor.visit_identifier_expr(expr),
            Expr::Literal(expr) => visitor.visit_literal_expr(expr),
            Expr::Unary(expr) => visitor.visit_unary_expr(expr),
        }
    }

    /// Gets the [Span] of the current node.
    pub fn span(&self) -> Span {
        // Yes this looks tedious since we cannot inherit structs in Rust
        match self {
            Expr::Binary(b) => b.span,
            Expr::Grouping(g) => g.span,
            Expr::Identifier(i) => i.span,
            Expr::Literal(l) => l.span,
            Expr::Unary(u) => u.span,
        }
    }
}

/// Visitor for [Expr] AST nodes.
///
/// The visitor pattern allows adding new operations to AST nodes without having
/// to modify them. An example implementor would be an AST printer, where it
/// specifies how to print each expression node.
///
/// See:
/// - [Rust Design Patterns - Visitor](https://rust-unofficial.github.io/patterns/patterns/behavioural/visitor.html)
/// - [Visitor in Rust](https://refactoring.guru/design-patterns/visitor/rust/example)
pub trait ExprVisitor {
    type Value;

    fn visit_binary_expr(&mut self, expr: &Binary) -> Self::Value;
    fn visit_grouping_expr(&mut self, expr: &Grouping) -> Self::Value;
    fn visit_identifier_expr(&mut self, expr: &Identifier) -> Self::Value;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Value;
    fn visit_unary_expr(&mut self, expr: &Unary) -> Self::Value;
}

/// Binary expression (e.g. `1 + 2`)
#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'src> {
    pub left: Expr<'src>,
    pub operator: BinaryOperator,
    pub right: Expr<'src>,
    pub span: Span,
}

/// Grouped expression using parenthesis (e.g. `("foo")`)
#[derive(Debug, Clone, PartialEq)]
pub struct Grouping<'src> {
    pub expression: Expr<'src>,
    pub span: Span,
}

/// Identifier (e.g. variable, function name)
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier<'src> {
    pub name: &'src str,
    pub span: Span,
}

/// Literal expression
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr<'src> {
    pub value: Literal<'src>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Boolean(bool),
    Nil,
    Number(f64),
    String(&'src str),
}

/// Unary expression (e.g. `-5`)
#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'src> {
    pub operator: UnaryOperator,
    pub right: Expr<'src>,
    pub span: Span,
}
