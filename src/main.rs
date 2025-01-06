use std::{
    env, io::{stdout, Write}, path::Path, process::ExitCode
};

mod scanner;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        stdout().write(b"Usage: red-lox [script]");
        ExitCode::from(64)
    } else if (args.len() == 1) {
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
            stdout().write(b"> ")?;
            line.clear();
            let read = stdin().read_line(&mut line)?;
            if read == 0 {
                break;
            }
            run(&line);
        }
        Ok(())
    }

    fn run(prog: &str) -> anyhow::Result<()> {
        let scanner = Scanner::new(prog);
        Ok(())
    }
}
