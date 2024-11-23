use std::env;
use std::fs;

use rust_lox::lexer::Lexer;

enum ExitCode {
    LexicalError = 65,
}

// From ExitCode to i32
impl From<ExitCode> for i32 {
    fn from(val: ExitCode) -> Self {
        val as i32
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    // TODO: Use clop for CLI
    match command.as_str() {
        "tokenize" => {
            let mut has_errors = false;

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let lexer = Lexer::new(&file_contents);
            for token in lexer {
                match token {
                    Ok(t) => println!("{t}"),
                    Err(err) => {
                        err.print_error(&file_contents);
                        has_errors = true;
                    }
                }
            }

            if has_errors {
                std::process::exit(ExitCode::LexicalError.into());
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command)
        }
    }
}
