use std::io::Write;

use crate::{chunk::Chunk, op_code::OpCode};

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
        Ok(op) => {
            match op {
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
                }
                OpCode::GetLocal | OpCode::SetLocal => {
                    let stack_index = chunk.get_code(offset + 1);
                    writeln!(w, "{:<16} {:04}", op, stack_index)?;
                }
                OpCode::Jump | OpCode::JumpIfFalse | OpCode::Loop => {
                    let jump = u16::from_be_bytes([
                        chunk.get_code(offset + 1),
                        chunk.get_code(offset + 2),
                    ]);
                    let next_location = match op {
                        OpCode::Loop => offset + op.len() - usize::from(jump),
                        _ => offset + op.len() + usize::from(jump),
                    };
                    writeln!(w, "{:<16} {:04} -> {}", op, offset, next_location)?;
                }
                OpCode::Negate
                | OpCode::Print
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
                | OpCode::Comma => writeln!(w, "{}", op)?,
            }
            Ok(op.len())
        }
        Err(e) => {
            writeln!(w, "{:}", e)?;
            Ok(1) // Move to the next code
        }
    }
}
