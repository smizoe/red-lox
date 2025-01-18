use std::{
    fs::File, io::{stdin, stdout, Read, Write}, path::Path
};

use crate::Interpreter;
use red_lox_ast::{parser::Parser, scanner::Scanner};

pub fn run_file(file_path: &Path) -> anyhow::Result<()> {
    let mut file = File::open(file_path)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;
    run(&mut Interpreter::new(), &s)
}

pub fn run_prompt() -> anyhow::Result<()> {
    let mut line = String::new();
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        stdout().flush()?;
        line.clear();
        let read = stdin().read_line(&mut line)?;
        if read == 0 {
            // Ctrl-d
            break;
        }
        let _ = run(&mut interpreter, &line);
    }
    Ok(())
}

fn run(interpreter: &mut Interpreter, prog: &str) -> anyhow::Result<()> {
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
    interpreter.interpret(&result.stmts);
    Ok(())
}
