use crate::{chunk::Chunk, op_code::OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset = 0;
    while offset < chunk.code.len() {
        offset += disassemble_instruction(offset, chunk);
    }
}

pub fn disassemble_instruction(offset: usize, chunk: &Chunk) -> usize {
    print!("{:04} ", offset);
    let line = chunk.line_of(offset);
    if offset > 0 && line == chunk.line_of(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", line);
    }
    match chunk.code[offset].try_into() {
        Ok(op) => {
            match op {
                OpCode::Constant => {
                    let constant_index = chunk.code[offset + 1];
                    println!(
                        "{:<16} {:04} '{}'",
                        op,
                        constant_index,
                        chunk.get_constant(usize::from(constant_index))
                    );
                }
                OpCode::Negate
                | OpCode::Return
                | OpCode::Add
                | OpCode::Subtract
                | OpCode::Multiply
                | OpCode::Divide => println!("{}", op),
            }
            op.len()
        }
        Err(e) => {
            println!("{:}", e);
            1 // Move to the next code
        }
    }
}
