use clap::ValueEnum;
use miette::{Diagnostic, Report, SourceSpan};

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
#[derive(thiserror::Error, Diagnostic, Clone, Debug, PartialEq)]
pub enum SyntaxError {
    #[error("Expected an expression")]
    MissingExpression {
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Unexpected character: {token}")]
    SingleTokenError {
        token: char,
        #[label("This character")]
        span: SourceSpan,
    },

    #[error("Expected `{expected}` but found `{actual}`")]
    UnexpectedToken {
        expected: String,
        actual: String,
        #[label("This token")]
        span: SourceSpan,
    },

    #[error("Unterminated string.")]
    UnterminatedStringError {
        #[label("This string literal")]
        span: SourceSpan,
    },
}

/// Runtime errors during evaluation.
#[derive(thiserror::Error, Diagnostic, Clone, Debug, PartialEq)]
pub enum RuntimeError {
    // TODO: Show error span after AST supports span
    #[error("Operands must be numbers.")]
    InfixNonNumberOperandsError,

    #[error("Operands must be two numbers or two strings.")]
    PlusOperandError,

    #[error("Operand must be a number.")]
    UnaryMinusOperandError,
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
        let offset = match self {
            Self::MissingExpression { span, .. } => span.offset(),
            Self::SingleTokenError { span, .. } => span.offset(),
            Self::UnexpectedToken { span, .. } => span.offset(),
            Self::UnterminatedStringError { span, .. } => span.offset(),
        };

        let src_until_err = &source_code[..=offset];
        src_until_err.lines().count()
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
            span: (0, 7).into(), // (offset, length)
        };
        assert_eq!(error.line_start(source_code), 1);

        let source_code = "1\n2\n@";
        let error = SyntaxError::SingleTokenError {
            token: '@',
            span: (4, 1).into(),
        };
        assert_eq!(error.line_start(source_code), 3);
    }
}
