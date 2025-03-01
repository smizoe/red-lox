use std::{
    io::{stderr, stdout},
    process::ExitCode,
};

use clap::{Parser, Subcommand};

use red_lox_command::compiler::run_compiler;
use red_lox_command::interpreter::run_interpreter;

#[derive(Debug, Parser)]
#[command(arg_required_else_help(true))]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Interpreter { file_name: Option<String> },
    Compiler { file_name: Option<String> },
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match &cli.command {
        Command::Interpreter { file_name } => {
            run_interpreter(file_name.as_ref(), &mut stdout(), &mut stderr())
        }
        Command::Compiler { file_name } => {
            run_compiler(file_name.as_ref(), &mut stdout(), &mut stderr())
        }
    }
}
