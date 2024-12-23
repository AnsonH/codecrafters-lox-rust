use clap::ValueEnum;
use miette::{Diagnostic, Report};
use thiserror::Error;

use crate::span::Span;

/// CLI option to control error reporting format.
#[derive(ValueEnum, Copy, Clone, Debug, PartialEq, Eq)]
pub enum ErrorFormat {
    /// Pretty errors using [miette](https://docs.rs/miette/latest/miette).
    Pretty,
    /// Simple plain text errors, used for passing
    /// [CodeCrafters](https://app.codecrafters.io/courses/interpreter) test cases.
    Simple,
}

/// Syntax errors when lexing/parsing the source code.
#[derive(Error, Diagnostic, Clone, Debug, PartialEq)]
pub enum SyntaxError {
    #[error("Expected an expression")]
    MissingExpression {
        #[label("here")]
        span: Span,
    },

    #[error("Expect variable name.")]
    MissingVariableName {
        #[label("here")]
        span: Span,
    },

    #[error("Unexpected character: {token}")]
    SingleTokenError {
        token: char,
        #[label("this character")]
        span: Span,
    },

    #[error("Can't have more than {max_count} arguments.")]
    TooManyCallArguments {
        max_count: usize,
        #[label("This callee")]
        span: Span,
    },

    #[error("Can't have more than {max_count} parameters.")]
    TooManyParameters {
        max_count: usize,
        #[label("here")]
        span: Span,
    },

    #[error("Expect '}}' after block.")]
    UnclosedBlock {
        #[label("this block is unclosed")]
        span: Span,
    },

    #[error("Expect function name.")]
    UnexpectedFunctionName {
        #[label("invalid name")]
        span: Span,
    },

    #[error("Expect parameter name.")]
    UnexpectedParameterName {
        #[label("invalid name")]
        span: Span,
    },

    #[error("Expected `{expected}` but found `{actual}`")]
    UnexpectedToken {
        expected: String,
        actual: String,
        #[label("this token")]
        span: Span,
    },

    #[error("Unterminated string.")]
    UnterminatedStringError {
        #[label("this string literal")]
        span: Span,
    },
}

/// Runtime errors during evaluation.
#[derive(Error, Diagnostic, Clone, Debug, PartialEq)]
pub enum RuntimeError {
    #[error("Expected {expected} arguments but got {actual}.")]
    CallArityMismatch {
        expected: usize,
        actual: usize,
        #[label("this callee")]
        span: Span,
    },

    #[error("Invalid assignment target.")]
    InvalidAssignment {
        #[label("cannot assign value to this target")]
        span: Span,
    },

    #[error("Operands must be numbers.")]
    InfixNonNumberOperandsError {
        #[label("this expression")]
        span: Span,
    },

    #[error("Can only call functions and classes.")]
    NotCallable {
        #[label("this cannot be called")]
        span: Span,
    },

    #[error("Operands must be two numbers or two strings.")]
    PlusOperandError {
        #[label("this expression")]
        span: Span,
    },

    #[error("Operand must be a number.")]
    UnaryMinusOperandError {
        #[label("this operand")]
        span: Span,
    },

    #[error("Undefined variable '{name}'.")]
    UndefinedVariable {
        name: String,
        #[label("this variable")]
        span: Span,
    },
}

impl SyntaxError {
    /// Consumes and prints the error to stderr
    // TODO: Deprecate this with a custom `miette::ReportHandler` implementation
    pub fn print_error(self, source_code: &str, format: &ErrorFormat) {
        match format {
            ErrorFormat::Pretty => {
                // `self: miette::Diagnostic` is simply a `std::error::Error`. We must convert it to
                // `miette::Report` so that the error is reported nicely.
                let report = Report::new(self).with_source_code(source_code.to_string());
                eprintln!("{:?}", report);
            }
            ErrorFormat::Simple => {
                eprintln!("[line {}] Error: {}", self.line_start(source_code), self);
            }
        }
    }

    /// Starting line number of the error
    fn line_start(&self, source_code: &str) -> usize {
        let span_until_err = Span::new(0, self.span().start + 1);
        let src_until_err = &source_code[span_until_err];
        src_until_err.lines().count()
    }

    fn span(&self) -> Span {
        match self {
            Self::MissingExpression { span, .. } => *span,
            Self::MissingVariableName { span, .. } => *span,
            Self::SingleTokenError { span, .. } => *span,
            Self::TooManyCallArguments { span, .. } => *span,
            Self::TooManyParameters { span, .. } => *span,
            Self::UnclosedBlock { span, .. } => *span,
            Self::UnexpectedFunctionName { span, .. } => *span,
            Self::UnexpectedParameterName { span, .. } => *span,
            Self::UnexpectedToken { span, .. } => *span,
            Self::UnterminatedStringError { span, .. } => *span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_line_start() {
        let source_code = "\"hi\nbye";
        let error = SyntaxError::UnterminatedStringError {
            span: Span::new(0, 8),
        };
        assert_eq!(error.line_start(source_code), 1);

        let source_code = "1\n2\n@";
        let error = SyntaxError::SingleTokenError {
            token: '@',
            span: Span::new(4, 5),
        };
        assert_eq!(error.line_start(source_code), 3);
    }
}
