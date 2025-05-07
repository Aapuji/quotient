/// Represents some span of text in a source file.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    start: usize,
    end: usize, // starting pos of next character
    file_id: FileId // Used to resolve to actual source lines
}

impl Span {
    pub fn new(start: usize, end: usize, file_id: FileId) -> Self {
        Self { start, end, file_id }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.start
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }
}

pub type FileId = usize;
