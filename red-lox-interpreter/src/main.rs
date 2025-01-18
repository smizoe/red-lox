use std::{
    env,
    path::Path,
    process::ExitCode,
};

use red_lox_interpreter::command::{run_file, run_prompt};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() > 1 {
        println!("Usage: red-lox [script]");
        ExitCode::from(64)
    } else if args.len() == 1 {
        match run_file(Path::new(&args[0])) {
            Ok(_) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("One or more errors occurred: {:}", e);
                ExitCode::FAILURE
            }
        }
    } else {
        match run_prompt() {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        }
    }
}

