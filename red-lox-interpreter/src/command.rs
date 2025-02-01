use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
    path::Path,
};

use crate::{resolver::Resolver, Interpreter};
use red_lox_ast::{parser::Parser, scanner::Scanner};

pub fn run_file(file_path: &Path, interpreter: &mut Interpreter) -> anyhow::Result<()> {
    let mut file = File::open(file_path)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;
    run(interpreter, &s)
}

pub fn run_prompt(interpreter: &mut Interpreter) -> anyhow::Result<()> {
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
        let _ = run(interpreter, &line);
    }
    Ok(())
}

fn run(interpreter: &mut Interpreter, prog: &str) -> anyhow::Result<()> {
    let mut scanner = Scanner::new(prog);
    let result = scanner.scan_tokens();
    if !result.errors.is_empty() {
        writeln!(
            interpreter.out,
            "One or more errors occurred during tokenization:"
        )?;
        for e in result.errors.iter() {
            writeln!(interpreter.out, "{}", e)?;
        }
        return Err(anyhow::anyhow!("Tokenization Error"));
    }
    let mut parser = Parser::new(result.tokens);
    let result = parser.parse();
    if !result.errors.is_empty() {
        writeln!(
            interpreter.err,
            "One or more errors occurred during parsing the expression:"
        )?;
        for e in result.errors.iter() {
            writeln!(interpreter.err, "{}", e)?;
        }
        return Err(anyhow::anyhow!("Parse Error"));
    }

    let mut resolver = Resolver::new(interpreter);
    if let Err(es) = resolver.resolve(&result.stmts) {
        writeln!(
            interpreter.err,
            "One or more errors occurred during resolving variables:"
        )?;
        for e in es {
            writeln!(interpreter.err, "{}", e)?;
        }
        return Err(anyhow::anyhow!("Resolution Error"))
    }
    interpreter.interpret(result.stmts);
    Ok(())
}
