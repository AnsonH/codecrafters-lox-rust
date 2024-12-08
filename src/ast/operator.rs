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
            _ => unreachable!("Expected binary operator, got {kind}"),
        }
    }
}

// TODO: Replace with enum
pub(crate) fn prefix_precedence(op: UnaryOperator) -> ((), u8) {
    match op {
        UnaryOperator::LogicalNot | UnaryOperator::UnaryMinus => ((), 9),
    }
}

/// Gets the left & right precedence values of an infix (binary) operator.
///
/// "LHS < RHS" means the operator is left associative, while the opposite means
/// right associative.
///
/// Returning `None` means the token kind is not an infix operator.
pub(crate) fn infix_precedence(kind: TokenKind) -> Option<(u8, u8)> {
    match kind {
        TokenKind::EqualEqual | TokenKind::BangEqual => Some((1, 2)),
        TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => {
            Some((3, 4))
        }
        TokenKind::Plus | TokenKind::Minus => Some((5, 6)),
        TokenKind::Star | TokenKind::Slash => Some((7, 8)),
        _ => None,
    }
}
