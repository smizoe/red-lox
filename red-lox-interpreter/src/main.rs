use std::{
    env,
    fs::File,
    io::{stdin, stdout, Read, Write},
    path::Path,
    process::ExitCode,
};

use red_lox_interpreter::Interpreter;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() > 1 {
        println!("Usage: red-lox [script]");
        ExitCode::from(64)
    } else if args.len() == 1 {
        match run_file(Path::new(&args[0])) {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        }
    } else {
        match run_prompt() {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        }
    }
}

use red_lox_ast::{parser::Parser, scanner::Scanner};

pub fn run_file(file_path: &Path) -> anyhow::Result<()> {
    let mut file = File::open(file_path)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;
    run(&s)
}

pub fn run_prompt() -> anyhow::Result<()> {
    let mut line = String::new();
    loop {
        print!("> ");
        stdout().flush()?;
        line.clear();
        let read = stdin().read_line(&mut line)?;
        if read == 0 {
            // Ctrl-d
            break;
        }
        let _ = run(&line);
    }
    Ok(())
}

fn run(prog: &str) -> anyhow::Result<()> {
    let mut scanner = Scanner::new(prog);
    let result = scanner.scan_tokens();
    if !result.errors.is_empty() {
        println!("One or more errors occurred during tokenization:");
        for e in result.errors.iter() {
            println!("{}", e);
        }
        return Err(anyhow::anyhow!("Error"));
    }
    let mut parser = Parser::new(result.tokens);
    let result = parser.parse();
    if !result.errors.is_empty() {
        println!("One or more errors occurred during parsing the expression:");
        for e in result.errors.iter() {
            println!("{}", e);
        }
        return Err(anyhow::anyhow!("Error"));
    }
    let interpreter = Interpreter {};
    interpreter.interpret(&result.stmts);
    Ok(())
}
