use std::ops::Range;

/// Represents some span of text in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        self.end
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn resolve_content<'t>(&self, src: &'t str) -> Option<&'t str> {
        src.get(self.range())
    }
}

pub type FileId = usize;
