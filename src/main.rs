use std::fs;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};
use rust_lox::{
    error::{ErrorFormat, SyntaxError},
    lexer::Lexer,
    token::Token,
};

enum ExitCode {
    LexicalError = 65,
}

// From ExitCode to i32
impl From<ExitCode> for i32 {
    fn from(val: ExitCode) -> Self {
        val as i32
    }
}

#[derive(Parser)]
#[command(version, about = "A Lox language interpreter")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Lexes the input and prints out a list of tokens.
    Tokenize {
        /// Path to a `.lox` file.
        filename: PathBuf,

        /// Error formatting.
        #[arg(
            long = "error-format",
            value_name = "FORMAT",
            default_value_t = ErrorFormat::Pretty,
            value_enum
        )]
        error_format: ErrorFormat,
    },
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize {
            filename,
            error_format,
        } => {
            let mut has_errors = false;

            let file_contents = fs::read_to_string(filename)
                .into_diagnostic() // Converts Error into a miette::Diagnostic for pretty errors
                .wrap_err(format!("Fail to read '{}'", filename.display()))?;

            let mut tokens: Vec<Token> = vec![];
            let mut errors: Vec<SyntaxError> = vec![];

            let lexer = Lexer::new(&file_contents);
            for token in lexer {
                match token {
                    Ok(t) => tokens.push(t),
                    Err(e) => {
                        errors.push(e);
                        has_errors = true;
                    }
                }
            }

            for error in errors {
                error.print_error(&file_contents, error_format);
            }
            for token in tokens {
                println!("{token}");
            }
            if has_errors {
                std::process::exit(ExitCode::LexicalError.into());
            }
        }
    }

    Ok(())
}
