use crate::span::Span;

use super::expression::*;

/// Statement produces side effects, and doesn't evaluate to a value.
///
/// # Optimization
///
/// This enum's size is optimized by `Box`-ing all enum variants.
/// See the [oxc AST guide](https://oxc.rs/docs/learn/parser_in_rust/ast.html#enum-size).
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    BlockStatement(Box<BlockStatement>),
    ExpressionStatement(Box<ExpressionStatement>),
    ForStatement(Box<ForStatement>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    IfStatement(Box<IfStatement>),
    PrintStatement(Box<PrintStatement>),
    VarStatement(Box<VarStatement>),
    WhileStatement(Box<WhileStatement>),
}

impl Stmt {
    /// Accepts a visitor implementing [StmtVisitor] to visit the expression
    /// and perform operations based on the expression's type.
    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::Value {
        match self {
            Stmt::BlockStatement(stmt) => visitor.visit_block_stmt(stmt),
            Stmt::ExpressionStatement(stmt) => visitor.visit_expression_stmt(stmt),
            Stmt::ForStatement(stmt) => visitor.visit_for_stmt(stmt),
            Stmt::FunctionDeclaration(stmt) => visitor.visit_function_declaration(stmt),
            Stmt::IfStatement(stmt) => visitor.visit_if_stmt(stmt),
            Stmt::PrintStatement(stmt) => visitor.visit_print_stmt(stmt),
            Stmt::VarStatement(stmt) => visitor.visit_var_stmt(stmt),
            Stmt::WhileStatement(stmt) => visitor.visit_while_stmt(stmt),
        }
    }

    /// Gets the [Span] of the current node.
    pub fn span(&self) -> Span {
        match self {
            Self::BlockStatement(s) => s.span,
            Self::ExpressionStatement(s) => s.span,
            Self::ForStatement(s) => s.span,
            Self::FunctionDeclaration(s) => s.span,
            Self::IfStatement(s) => s.span,
            Self::PrintStatement(s) => s.span,
            Self::VarStatement(s) => s.span,
            Self::WhileStatement(s) => s.span,
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

    fn visit_block_stmt(&mut self, stmt: &BlockStatement) -> Self::Value;
    fn visit_expression_stmt(&mut self, stmt: &ExpressionStatement) -> Self::Value;
    fn visit_for_stmt(&mut self, stmt: &ForStatement) -> Self::Value;
    fn visit_function_declaration(&mut self, stmt: &FunctionDeclaration) -> Self::Value;
    fn visit_if_stmt(&mut self, stmt: &IfStatement) -> Self::Value;
    fn visit_print_stmt(&mut self, stmt: &PrintStatement) -> Self::Value;
    fn visit_var_stmt(&mut self, stmt: &VarStatement) -> Self::Value;
    fn visit_while_stmt(&mut self, stmt: &WhileStatement) -> Self::Value;
}

/// Syntax: `{ <statement(s)> }`
#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

/// Syntax: `<expression>;`
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expr,
    pub span: Span,
}

/// Syntax: `for ( <initializer?> ; <condition?> ; <update?> ) <statement>`
///
/// # Syntactic Sugar
///
/// The [original Lox implementation](https://craftinginterpreters.com/control-flow.html#for-loops)
/// immediately de-sugars for-loop into a while-loop during parsing. However, our
/// implementation has a dedicated for-statement AST node, and the de-sugaring
/// will happen during evaluation.
#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub init: Option<ForStatementInit>,
    pub condition: Option<Expr>,
    pub update: Option<Expr>,
    pub body: Stmt,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForStatementInit {
    ExpressionStatement(Box<ExpressionStatement>),
    VarStatement(Box<VarStatement>),
}

/// Syntax: `fun <identifier>( <parameter(s)> ) { <statement(s)> }`
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub span: Span,
}

/// Syntax:
/// - `if ( <expression> ) <statement>`
/// - `if ( <expression> ) <statement> else <statement>`
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
    pub span: Span,
}

/// Syntax: `print <expression>;`
#[derive(Debug, Clone, PartialEq)]
pub struct PrintStatement {
    pub expression: Expr,
    pub span: Span,
}

/// Syntax:
/// - `var <name>;`
/// - `var <name> = <expression>;`
#[derive(Debug, Clone, PartialEq)]
pub struct VarStatement {
    pub ident: Identifier,
    pub initializer: Option<Expr>,
    pub span: Span,
}

/// Syntax: `while ( <expression> ) <statement>`
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Stmt,
    pub span: Span,
}
