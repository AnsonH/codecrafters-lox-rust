/// Unwraps the inner type of [Expr](crate::ast::Expr) or [Stmt](crate::ast::Stmt)
/// given a known enum variant.
///
/// It panics if the inner type does not match the given variant.
///
/// # Example
/// ```
/// use rust_lox::{ast::{Expr, Identifier}, unwrap_ast_node};
///
/// let expr = Expr::Identifier(Box::new(Identifier {
///     name: "foo".to_string(),
///     span: (0, 3).into(),
/// }));
/// let identifier = unwrap_ast_node!(expr, Expr::Identifier);
/// assert_eq!(identifier.name, "foo");
/// ```
#[macro_export]
macro_rules! unwrap_ast_node {
    ($node: expr, $variant: path) => {
        if let $variant(inner) = $node {
            inner
        } else {
            panic!("cannot unwrap {} from {:?}", stringify!($variant), $node)
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expr, Identifier, PrintStatement, Stmt},
        span::Span,
    };

    use pretty_assertions::assert_eq;

    #[test]
    fn test_unwrap_ast_node() {
        let ident = Box::new(Identifier {
            name: "foo".to_string(),
            span: Span::new(0, 3),
        });
        let print = Box::new(PrintStatement {
            expression: Expr::Identifier(ident.clone()),
            span: Span::new(0, 10),
        });
        let stmt = Stmt::PrintStatement(print.clone());

        let print_unwrapped = unwrap_ast_node!(stmt, Stmt::PrintStatement);
        assert_eq!(print_unwrapped, print.clone());
        assert_eq!(
            unwrap_ast_node!(print_unwrapped.expression, Expr::Identifier),
            ident.clone()
        );
    }
}
