use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
};

use crate::{compiler::Compiler, debug::disassemble_chunk, vm::VirtualMachine};

pub fn compile_and_run(file_name: &str) -> anyhow::Result<()> {
    let mut file = File::open(file_name)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;

    let mut compiler = Compiler::new(&s);
    compiler.compile()?;
    let chunk = compiler.finish();
    if cfg!(debug_assertions) {
        disassemble_chunk(&chunk, file_name);
    }
    let mut vm = VirtualMachine::new(&chunk);
    vm.interpret()?;
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

        let mut compiler = Compiler::new(&line);
        if let Err(e) = compiler.compile() {
            eprintln!("Failed to compile the statement: {}", e);
            continue;
        }
        let chunk = compiler.finish();
        if cfg!(debug_assertions) {
            disassemble_chunk(&chunk, "__interpreter__");
        }
        let mut vm = VirtualMachine::new(&chunk);
        if let Err(e) = vm.interpret() {
            eprintln!("Failed to interpret the statement: {}", e);
        }
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum Error {}
