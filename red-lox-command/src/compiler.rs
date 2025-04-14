use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
    path::Path,
    process::ExitCode,
};

use anyhow::anyhow;
use red_lox_compiler::{
    common::chunk::debug::disassemble_chunk, compiler::Compiler, runtime::VirtualMachine,
};

fn compile_and_run_file<O>(file_name: &Path, out: &mut O) -> anyhow::Result<()>
where
    O: std::io::Write,
{
    let mut file = File::open(file_name)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;

    compile_and_run(&s, out)?;
    Ok(())
}

fn run_vm_as_interpreter<O>(out: &mut O) -> anyhow::Result<()>
where
    O: std::io::Write,
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

        if let Err(e) = compile_and_run(&line, out) {
            eprintln!("{}", e);
        }
    }
    Ok(())
}

fn compile_and_run<O>(code: &str, out: &mut O) -> anyhow::Result<()>
where
    O: std::io::Write,
{
    let mut compiler = Compiler::new(code);
    if let Err(e) = compiler.compile() {
        return Err(anyhow!("Failed to compile the statement:\n{}", e));
    }
    let result = compiler.finish();
    if cfg!(debug_assertions) {
        disassemble_chunk(&result.script.fun().chunk(), "__interpreter__");
    }
    let mut vm = VirtualMachine::new(result.script, result.interned_string_registry, out);
    if let Err(e) = vm.interpret() {
        return Err(anyhow!(
            "Failed to interpret the statement: {}\nStack trace:\n{}",
            e,
            vm.stack_trace()
        ));
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
        None => match run_vm_as_interpreter(out) {
            Ok(()) => ExitCode::SUCCESS,
            _ => ExitCode::FAILURE,
        },
        Some(name) => match compile_and_run_file(name.as_ref(), out) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                writeln!(err, "{}", e).expect("Failed to write to the error output.");
                ExitCode::FAILURE
            }
        },
    }
}
