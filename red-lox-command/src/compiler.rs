use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
    path::Path,
    process::ExitCode,
};

use anyhow::anyhow;
use red_lox_compiler::{compiler::Compiler, debug::disassemble_chunk, vm::VirtualMachine};

fn compile_and_run_file<O, E>(file_name: &Path, out: &mut O, err: &mut E) -> anyhow::Result<()>
where
    O: std::io::Write,
    E: std::io::Write,
{
    let mut file = File::open(file_name)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;

    compile_and_run(&s, out, err)?;
    Ok(())
}

fn run_vm_as_interpreter<O, E>(out: &mut O, err: &mut E) -> anyhow::Result<()>
where
    O: std::io::Write,
    E: std::io::Write,
{
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

        if let Err(e) = compile_and_run(&line, out, err) {
            eprintln!("{}", e);
        }
    }
    Ok(())
}

fn compile_and_run<O, E>(code: &str, out: &mut O, err: &mut E) -> anyhow::Result<()>
where
    O: std::io::Write,
    E: std::io::Write,
{
    let mut compiler = Compiler::new(code);
    if let Err(e) = compiler.compile() {
        return Err(anyhow!("Failed to compile the statement: {}", e));
    }
    let chunk = compiler.finish();
    if cfg!(debug_assertions) {
        disassemble_chunk(&chunk, "__interpreter__");
    }
    let mut vm = VirtualMachine::new(&chunk, out);
    if let Err(e) = vm.interpret() {
        return Err(anyhow!("Failed to interpret the statement: {}", e));
    }
    Ok(())
}

pub fn run_compiler<S, O, E>(file_name: Option<S>, out: &mut O, err: &mut E) -> ExitCode
where
    S: AsRef<Path>,
    O: std::io::Write,
    E: std::io::Write,
{
    match file_name {
        None => match run_vm_as_interpreter(out, err) {
            Ok(()) => ExitCode::SUCCESS,
            _ => ExitCode::FAILURE,
        },
        Some(name) => match compile_and_run_file(name.as_ref(), out, err) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("{}", e);
                ExitCode::FAILURE
            }
        },
    }
}
