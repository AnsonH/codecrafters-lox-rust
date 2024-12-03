use std::fs;
use std::path::PathBuf;

use clap::{Parser as ClapParser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};
use rust_lox::{
    error::{ErrorFormat, SyntaxError},
    lexer::Lexer,
    parser::Parser,
    token::Token,
};

enum ExitCode {
    LexicalError = 65,
}

#[derive(ClapParser)]
#[command(version, about = "A Lox language interpreter")]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Error formatting.
    #[arg(
            long = "error-format",
            value_name = "FORMAT",
            default_value_t = ErrorFormat::Pretty,
            value_enum
        )]
    error_format: ErrorFormat,
}

#[derive(Subcommand)]
enum Commands {
    /// Lexes the input and prints out a list of tokens.
    Tokenize {
        /// Path to a `.lox` file.
        filename: PathBuf,
    },
    /// Parses an expression and prints out a braces-format AST.
    Parse {
        /// Path to a `.lox` file.
        filename: PathBuf,
    },
}

fn read_file(filename: &PathBuf) -> miette::Result<String> {
    fs::read_to_string(filename)
        .into_diagnostic() // Converts Error into a miette::Report for pretty errors
        .wrap_err(format!("Fail to read '{}'", filename.display()))
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize { filename } => {
            let source = read_file(filename)?;

            let mut has_errors = false;
            let mut tokens: Vec<Token> = vec![];
            let mut errors: Vec<SyntaxError> = vec![];

            let lexer = Lexer::new(&source);
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
                error.print_error(&source, &cli.error_format);
            }
            for token in tokens {
                println!("{}", token.to_string(&source));
            }
            if has_errors {
                std::process::exit(ExitCode::LexicalError as i32);
            }
        }
        Commands::Parse { filename } => {
            let source = read_file(filename)?;

            let parser = Parser::new(&source);
            match parser.parse_expression() {
                Ok(expr) => println!("{expr}"),
                Err(err) => {
                    err.print_error(&source, &cli.error_format);
                    std::process::exit(ExitCode::LexicalError as i32);
                }
            }
        }
    }

    Ok(())
}
