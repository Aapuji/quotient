/// Represents some span of text in a source file.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file_id: FileId // Used to resolve to actual source lines
}

pub type FileId = usize;
