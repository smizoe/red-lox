use crate::{common::chunk::Chunk, common::InternedString};

#[derive(Clone)]
pub struct LoxFunction {
    pub(crate) name: InternedString,
    pub(crate) arity: usize,
    chunk: Chunk,
}

impl LoxFunction {
    pub fn new(name: InternedString, arity: usize) -> Self {
        Self {
            name,
            arity,
            chunk: Chunk::new(),
        }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

impl std::fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoxFunction")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .field("chunk", &"__chunk__")
            .finish()
    }
}

impl std::fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.as_ref().is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", self.name)
        }
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity && self.chunk == other.chunk
    }
}
