use std::{env, path::Path, process::ExitCode};

mod scanner;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() > 1 {
        println!("Usage: red-lox [script]");
        ExitCode::from(64)
    } else if args.len() == 1 {
        match internal::run_file(Path::new(&args[0])) {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        }
    } else {
        match internal::run_prompt() {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        }
    }
}

mod internal {
    use anyhow;
    use std::{
        fs::File,
        io::{stdin, stdout, Read, Write},
        path::Path,
    };

    use crate::scanner::Scanner;

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
            let _ = stdin().read_line(&mut line)?;
            let cmd = line.trim_end();
            if cmd.len() == 1 && cmd.as_bytes()[0] == 4 { // Ctrl-d
                break;
            }
            let _ = run(cmd);
        }
        Ok(())
    }

    fn run(prog: &str) -> anyhow::Result<()> {
        let mut scanner = Scanner::new(prog);
        let result = scanner.scan_tokens();
        for token in result.tokens.iter() {
            println!("{:?}", token);
        }
        for e in result.errors.iter() {
            println!("{:?}", e);
        }
        Ok(())
    }
}
