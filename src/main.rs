use std::fs;
use std::path::PathBuf;

use clap::{Parser as ClapParser, Subcommand};
use miette::{IntoDiagnostic, NamedSource, Result, WrapErr};
use rust_lox::{
    ast::printer::AstPrefixPrinter,
    error::{ErrorFormat, SyntaxError},
    evaluator::Evaluator,
    lexer::Lexer,
    parser::Parser,
    token::Token,
};

#[derive(Clone, Copy)]
enum ExitCode {
    /// Due to syntax error during lexing or parsing.
    LexicalError = 65,
    /// Due to runtime error during evaluation.
    RuntimeError = 70,
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
    /// Evaluates an expression.
    Evaluate {
        /// Path to a `.lox` file.
        filename: PathBuf,
    },
    /// Executes a Lox program.
    Run {
        /// Path to a `.lox` file.
        filename: PathBuf,
    },
}

fn read_file(filename: &PathBuf) -> Result<NamedSource<String>> {
    fs::read_to_string(filename)
        .map(|content| NamedSource::new(filename.display().to_string(), content))
        .into_diagnostic() // Converts Error into a miette::Report for pretty errors
        .wrap_err(format!("Fail to read '{}'", filename.display()))
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize { filename } => {
            let source = read_file(filename)?;
            let source = source.inner();

            let mut has_errors = false;
            let mut tokens: Vec<Token> = vec![];
            let mut errors: Vec<SyntaxError> = vec![];

            let lexer = Lexer::new(source);
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
                error.print_error(source, &cli.error_format);
            }
            for token in tokens {
                println!("{}", token.to_string(source));
            }
            if has_errors {
                std::process::exit(ExitCode::LexicalError as i32);
            }
        }
        Commands::Parse { filename } => {
            let source = read_file(filename)?;

            let parser = Parser::new(source.inner());
            let mut printer = AstPrefixPrinter;

            match parser.parse_expression() {
                Ok(expr) => println!("{}", printer.print_expression(&expr)),
                Err(report) => {
                    // TODO: Implement a new `miette::ReportHandler` that can emit
                    // Lox-styled simple error reports
                    eprintln!("{:?}", report.with_source_code(source));
                    std::process::exit(ExitCode::LexicalError as i32);
                }
            }
        }
        Commands::Evaluate { filename } => {
            let source = read_file(filename)?;

            let parser = Parser::new(source.inner());
            let expr = match parser.parse_expression() {
                Ok(expr) => expr,
                Err(report) => {
                    eprintln!("{:?}", report.with_source_code(source));
                    std::process::exit(ExitCode::LexicalError as i32);
                }
            };

            let mut evaluator = Evaluator::default();
            match evaluator.evaluate_expression(&expr) {
                Ok(obj) => println!("{obj}"),
                Err(report) => {
                    eprintln!("{:?}", report.with_source_code(source));
                    std::process::exit(ExitCode::RuntimeError as i32);
                }
            }
        }
        Commands::Run { filename } => {
            let source = read_file(filename)?;

            let parser = Parser::new(source.inner());
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(report) => {
                    eprintln!("{:?}", report.with_source_code(source));
                    std::process::exit(ExitCode::LexicalError as i32);
                }
            };

            let mut evaluator = Evaluator::default();
            if let Err(report) = evaluator.evaluate_program(&program) {
                eprintln!("{:?}", report.with_source_code(source));
                std::process::exit(ExitCode::RuntimeError as i32);
            }
        }
    }

    Ok(())
}
