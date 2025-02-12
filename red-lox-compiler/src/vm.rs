use crate::chunk::Chunk;

pub struct VirtualMachine<'a> {
    chunk: &'a Chunk,
    ip: usize,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self { chunk, ip: 0 }
    }
}
