use std::{
    env,
    io::{stderr, stdout},
    path::Path,
    process::ExitCode,
};

use red_lox_interpreter::{
    command::{run_file, run_prompt},
    Interpreter,
};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() > 1 {
        println!("Usage: red-lox [script]");
        ExitCode::from(64)
    } else if args.len() == 1 {
        match run_file(
            Path::new(&args[0]),
            &mut Interpreter::new(&mut stdout(), &mut stderr()),
        ) {
            Ok(_) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("One or more errors occurred: {:}", e);
                ExitCode::FAILURE
            }
        }
    } else {
        match run_prompt(&mut Interpreter::new(&mut stdout(), &mut stderr())) {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        }
    }
}
