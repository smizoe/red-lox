use std::{
    io::{stderr, stdout},
    path::Path,
    process::ExitCode,
};

use clap::{Parser, Subcommand};

use red_lox_compiler::command::{compile_and_run, run_vm_as_interpreter};
use red_lox_interpreter::{
    command::{run_file, run_prompt},
    Interpreter,
};

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
        Command::Interpreter { file_name } => run_interpreter(file_name.as_ref()),
        Command::Compiler { file_name } => run_compiler(file_name.as_ref()),
    }
}

fn run_interpreter<S>(file_name: Option<S>) -> ExitCode
where
    S: AsRef<str>,
{
    match file_name {
        None => match run_prompt(&mut Interpreter::new(&mut stdout(), &mut stderr())) {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        },
        Some(file_name) => {
            match run_file(
                Path::new(file_name.as_ref()),
                &mut Interpreter::new(&mut stdout(), &mut stderr()),
            ) {
                Ok(_) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("One or more errors occurred: {:}", e);
                    ExitCode::FAILURE
                }
            }
        }
    }
}

fn run_compiler<S>(file_name: Option<S>) -> ExitCode
where
    S: AsRef<str>,
{
    match file_name {
        None => match run_vm_as_interpreter() {
            Ok(()) => ExitCode::SUCCESS,
            _ => ExitCode::FAILURE,
        },
        Some(name) => match compile_and_run(name.as_ref()) {
            Ok(()) => ExitCode::SUCCESS,
            _ => ExitCode::FAILURE,
        },
    }
    //let mut chunk = Chunk::new();
    //chunk.write(&Instruction::Constant(1.2), 123)?;
    //chunk.write(&Instruction::Constant(3.4), 123)?;
    //chunk.write(&Instruction::Add, 123)?;
    //chunk.write(&Instruction::Constant(5.6), 123)?;
    //chunk.write(&Instruction::Divide, 123)?;
    //chunk.write(&Instruction::Negate, 123)?;
    //chunk.write(&Instruction::Return, 123)?;
    //let mut vm = VirtualMachine::new(&chunk);
    //vm.interpret()?;
    //Ok(())
}
