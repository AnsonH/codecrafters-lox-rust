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

#[derive(thiserror::Error, Diagnostic, Debug, PartialEq)]
pub enum SyntaxError {
    #[error("Unexpected character: {token}")]
    SingleTokenError {
        token: char,

        #[label("This character")]
        err_span: SourceSpan,
    },

    #[error("Unterminated string")]
    StringTerminationError {
        #[label("This string literal")]
        err_span: SourceSpan,
    },
}

impl SyntaxError {
    /// Consumes and prints the error to stderr
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
            Self::SingleTokenError { err_span, .. } => err_span.offset(),
            Self::StringTerminationError { err_span, .. } => err_span.offset(),
        };

        let src_until_err = &source_code[..=offset];
        src_until_err.lines().count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_start() {
        let source_code = "'hi\nbye";
        let error = SyntaxError::StringTerminationError {
            err_span: (0, 7).into(), // (offset, length)
        };
        assert_eq!(error.line_start(source_code), 1);

        let source_code = "1\n2\n@";
        let error = SyntaxError::SingleTokenError {
            token: '@',
            err_span: (4, 1).into(),
        };
        assert_eq!(error.line_start(source_code), 3);
    }
}
