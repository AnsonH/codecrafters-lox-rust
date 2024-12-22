use crate::token::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, strum::Display)]
pub enum UnaryOperator {
    #[strum(to_string = "!")]
    LogicalNot,
    #[strum(to_string = "-")]
    UnaryMinus,
}

#[derive(Debug, Clone, Copy, PartialEq, strum::Display)]
pub enum BinaryOperator {
    #[strum(to_string = "+")]
    Add,
    #[strum(to_string = "-")]
    Subtract,
    #[strum(to_string = "*")]
    Multiply,
    #[strum(to_string = "/")]
    Divide,
    #[strum(to_string = ">")]
    GreaterThan,
    #[strum(to_string = ">=")]
    GreaterEqualThan,
    #[strum(to_string = "<")]
    LessThan,
    #[strum(to_string = "<=")]
    LessEqualThan,
    #[strum(to_string = "==")]
    Equal,
    #[strum(to_string = "!=")]
    NotEqual,
    #[strum(to_string = "or")]
    LogicalOr,
    #[strum(to_string = "and")]
    LogicalAnd,
}

impl From<TokenKind> for BinaryOperator {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Plus => BinaryOperator::Add,
            TokenKind::Minus => BinaryOperator::Subtract,
            TokenKind::Star => BinaryOperator::Multiply,
            TokenKind::Slash => BinaryOperator::Divide,
            TokenKind::Greater => BinaryOperator::GreaterThan,
            TokenKind::GreaterEqual => BinaryOperator::GreaterEqualThan,
            TokenKind::Less => BinaryOperator::LessThan,
            TokenKind::LessEqual => BinaryOperator::LessEqualThan,
            TokenKind::EqualEqual => BinaryOperator::Equal,
            TokenKind::BangEqual => BinaryOperator::NotEqual,
            TokenKind::Or => BinaryOperator::LogicalOr,
            TokenKind::And => BinaryOperator::LogicalAnd,
            _ => unreachable!("Expected binary operator, got {kind}"),
        }
    }
}

// TODO: Replace with enum
/// Gets the right precedence value of a prefix operator.
pub(crate) fn prefix_precedence(op: UnaryOperator) -> ((), u8) {
    match op {
        UnaryOperator::LogicalNot | UnaryOperator::UnaryMinus => ((), 15),
    }
}

/// Gets the left & right precedence values of an infix (binary) operator.
///
/// "LHS < RHS" means the operator is left associative, while the opposite means
/// right associative.
///
/// Returning `None` means the token kind is not an infix operator.
pub(crate) fn infix_precedence(kind: TokenKind) -> Option<(u8, u8)> {
    use TokenKind::*;
    match kind {
        Or => Some((1, 2)),
        And => Some((3, 4)),
        Equal => Some((6, 5)),
        EqualEqual | BangEqual => Some((7, 8)),
        Greater | GreaterEqual | Less | LessEqual => Some((8, 9)),
        Plus | Minus => Some((10, 11)),
        Star | Slash => Some((12, 13)),
        _ => None,
    }
}

/// Gets the left precedence value of a postfix operator.
///
/// Returning `None` means the token kind is not an postfix operator.
pub(crate) fn postfix_precedence(kind: TokenKind) -> Option<(u8, ())> {
    use TokenKind::*;
    match kind {
        LeftParen => Some((17, ())), // Call expression
        _ => None,
    }
}
