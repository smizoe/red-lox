use std::io::Write;

use crate::common::{chunk::Chunk, op_code::OpCode, value::Value};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset = 0;
    while offset < chunk.code_len() {
        offset += disassemble_instruction(offset, chunk);
    }
}

pub fn disassemble_instruction(offset: usize, chunk: &Chunk) -> usize {
    disassemble_instruction_internal(offset, chunk, &mut std::io::stdout())
        .expect("Failed to write to stdout.")
}

#[cfg(test)]
pub(crate) fn disassemble_chunk_for_testing(chunk: &Chunk, w: &mut dyn Write) {
    let mut offset = 0;
    while offset < chunk.code_len() {
        offset += disassemble_instruction_internal(offset, chunk, w)
            .expect("Failed to write to the output");
    }
}

fn disassemble_instruction_internal(
    offset: usize,
    chunk: &Chunk,
    w: &mut dyn Write,
) -> Result<usize, std::io::Error> {
    write!(w, "{:04} ", offset)?;
    let line = chunk.line_of(offset);
    if offset > 0 && line == chunk.line_of(offset - 1) {
        write!(w, "   | ")?;
    } else {
        write!(w, "{:4} ", line)?;
    }
    match chunk.get_code(offset).try_into() {
        Ok(op) => match op {
            OpCode::Constant
            | OpCode::DefineGlobal
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::Call => {
                let constant_index = chunk.get_code(offset + 1);
                writeln!(
                    w,
                    "{:<16} {:04} '{}'",
                    op,
                    constant_index,
                    chunk.get_constant(usize::from(constant_index))
                )?;
                Ok(2)
            }
            OpCode::GetLocal | OpCode::SetLocal | OpCode::GetUpValue | OpCode::SetUpValue => {
                let stack_index = chunk.get_code(offset + 1);
                writeln!(w, "{:<16} {:04}", op, stack_index)?;
                Ok(2)
            }
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::Loop => {
                let op_len = 3;
                let jump =
                    u16::from_be_bytes([chunk.get_code(offset + 1), chunk.get_code(offset + 2)]);
                let next_location = match op {
                    OpCode::Loop => offset + op_len - usize::from(jump),
                    _ => offset + op_len + usize::from(jump),
                };
                writeln!(w, "{:<16} {:04} -> {}", op, offset, next_location)?;
                Ok(op_len)
            }
            OpCode::Closure => {
                let const_location = chunk.get_code(offset + 1);
                let mut op_len = 2;
                writeln!(
                    w,
                    "{:<16} {:04} {}",
                    op,
                    const_location,
                    chunk.get_constant(const_location.into())
                )?;
                let closure = match chunk.get_constant(const_location.into()) {
                    Value::Closure(c) => c,
                    _ => unreachable!(),
                };
                for _ in 0..(closure.upvalue_count()) {
                    let is_local = chunk.get_code(offset + op_len);
                    op_len += 1;
                    let index = chunk.get_code(offset + op_len);
                    op_len += 1;
                    writeln!(
                        w,
                        "{:04}      |                     {} {}",
                        offset + op_len - 2,
                        if is_local > 0 { "local" } else { "upvalue" },
                        index
                    )?;
                }

                Ok(op_len)
            }
            OpCode::Negate
            | OpCode::Print
            | OpCode::CloseUpValue
            | OpCode::Return
            | OpCode::Nil
            | OpCode::True
            | OpCode::False
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Pop
            | OpCode::Equal
            | OpCode::Greater
            | OpCode::Less
            | OpCode::Not
            | OpCode::Comma => {
                writeln!(w, "{}", op)?;
                Ok(1)
            }
        },
        Err(e) => {
            writeln!(w, "{:}", e)?;
            Ok(1) // Move to the next code
        }
    }
}
