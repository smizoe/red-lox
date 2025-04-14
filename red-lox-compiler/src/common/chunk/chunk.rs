use crate::common::value::Value;

#[derive(Clone, PartialEq)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<LineInfo>,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
struct LineInfo {
    offset: usize,
    line: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub(crate) fn add_constant(&mut self, v: Value) {
        self.constants.push(v);
    }

    pub(crate) fn get_constant(&self, index: usize) -> Value {
        self.constants[index].clone()
    }

    pub(crate) fn get_num_constants(&self) -> usize {
        self.constants.len()
    }

    pub(crate) fn line_of(&self, offset: usize) -> usize {
        if self.lines.len() == 0 {
            return 1;
        }
        match self
            .lines
            .binary_search_by_key(&offset, |LineInfo { offset, .. }| *offset)
        {
            Ok(index) => self.lines[index].line,
            Err(index) => self.lines[index - 1].line,
        }
    }

    pub(crate) fn add_code(&mut self, code: u8) {
        self.code.push(code);
    }

    pub(crate) fn get_code(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub(crate) fn set_code(&mut self, offset: usize, value: u8) {
        self.code[offset] = value;
    }

    pub(crate) fn code_len(&self) -> usize {
        self.code.len()
    }

    pub(crate) fn maybe_update_line_info(&mut self, offset: usize, line: usize) {
        match self.lines.last() {
            Some(line_info) if line_info.line == line => (),
            _ => self.lines.push(LineInfo { offset, line }),
        }
    }
}
