use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
};

use crate::{compiler::Compiler, debug::disassemble_chunk, vm::VirtualMachine};
use anyhow::anyhow;

pub fn compile_and_run_file(file_name: &str) -> anyhow::Result<()> {
    let mut file = File::open(file_name)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;

    compile_and_run(&s)?;
    Ok(())
}

pub fn run_vm_as_interpreter() -> anyhow::Result<()> {
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

#[derive(Debug, thiserror::Error)]
pub enum Error {}
