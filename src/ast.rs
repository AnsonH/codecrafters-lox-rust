use std::fmt::Display;

/// Root AST node that represents the whole program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Stmt>,
}

/// Statement does not produce a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {}

/// Expression produces a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Literal(Literal<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Boolean(bool),
    Nil,
    Number(f64),
    String(&'src str),
}

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(literal) => write!(f, "{literal}"),
        }
    }
}

impl<'src> Display for Literal<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Nil => write!(f, "nil"),
            Literal::Number(n) => write!(f, "{n}"),
            Literal::String(s) => write!(f, "{s}"),
        }
    }
}
