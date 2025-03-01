use std::{
    fs::File,
    io::{stdin, stdout, Read, Write}, process::ExitCode,
};

use anyhow::anyhow;
use red_lox_compiler::{compiler::Compiler, debug::disassemble_chunk, vm::VirtualMachine};

fn compile_and_run_file(file_name: &str) -> anyhow::Result<()> {
    let mut file = File::open(file_name)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;

    compile_and_run(&s)?;
    Ok(())
}

fn run_vm_as_interpreter() -> anyhow::Result<()> {
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

        if let Err(e) = compile_and_run(&line) {
            eprintln!("{}", e);
        }
    }
    Ok(())
}

fn compile_and_run(code: &str) -> anyhow::Result<()> {
    let mut compiler = Compiler::new(code);
    if let Err(e) = compiler.compile() {
        return Err(anyhow!("Failed to compile the statement: {}", e));
    }
    let chunk = compiler.finish();
    if cfg!(debug_assertions) {
        disassemble_chunk(&chunk, "__interpreter__");
    }
    let mut vm = VirtualMachine::new(&chunk);
    if let Err(e) = vm.interpret() {
        return Err(anyhow!("Failed to interpret the statement: {}", e));
    }
    Ok(())
}


pub fn run_compiler<S>(file_name: Option<S>) -> ExitCode
where
    S: AsRef<str>,
{
    match file_name {
        None => match run_vm_as_interpreter() {
            Ok(()) => ExitCode::SUCCESS,
            _ => ExitCode::FAILURE,
        },
        Some(name) => match compile_and_run_file(name.as_ref()) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("{}", e);
                ExitCode::FAILURE
            }
        },
    }
}