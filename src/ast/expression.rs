use crate::span::Span;

use super::{BinaryOperator, UnaryOperator};

/// Expression produces a value.
///
/// # Optimization
///
/// This enum's size is optimized by `Box`-ing all enum variants.
/// See the [oxc AST guide](https://oxc.rs/docs/learn/parser_in_rust/ast.html#enum-size).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assignment(Box<Assignment>),
    Binary(Box<Binary>),
    Call(Box<Call>),
    Grouping(Box<Grouping>),
    Identifier(Box<Identifier>),
    Literal(Box<LiteralExpr>),
    Unary(Box<Unary>),
}

impl Expr {
    /// Accepts a visitor implementing [ExprVisitor] to visit the expression
    /// and perform operations based on the expression's type.
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Value {
        match self {
            Expr::Assignment(expr) => visitor.visit_assignment_expr(expr),
            Expr::Binary(expr) => visitor.visit_binary_expr(expr),
            Expr::Call(expr) => visitor.visit_call_expr(expr),
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
            Expr::Assignment(e) => e.span,
            Expr::Binary(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::Grouping(e) => e.span,
            Expr::Identifier(e) => e.span,
            Expr::Literal(e) => e.span,
            Expr::Unary(e) => e.span,
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

    fn visit_assignment_expr(&mut self, expr: &Assignment) -> Self::Value;
    fn visit_binary_expr(&mut self, expr: &Binary) -> Self::Value;
    fn visit_call_expr(&mut self, expr: &Call) -> Self::Value;
    fn visit_grouping_expr(&mut self, expr: &Grouping) -> Self::Value;
    fn visit_identifier_expr(&mut self, expr: &Identifier) -> Self::Value;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Value;
    fn visit_unary_expr(&mut self, expr: &Unary) -> Self::Value;
}

/// Assignment expression (e.g. `foo = 10`)
#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub left: Expr,
    pub right: Expr,
    pub span: Span,
}

/// Binary expression (e.g. `1 + 2`)
#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
    pub span: Span,
}

/// Call expression (e.g. `clock()`)
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
    pub span: Span,
}

/// Grouped expression using parenthesis (e.g. `("foo")`)
#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
    pub expression: Expr,
    pub span: Span,
}

/// Identifier (e.g. variable, function name)
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

/// Literal expression
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Nil,
    Number(f64),
    // Note: We purposely use `String` over `&'src str` to avoid introducing
    // lifetimes everywhere.
    String(String),
}

/// Unary expression (e.g. `-5`)
#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: UnaryOperator,
    pub right: Expr,
    pub span: Span,
}
