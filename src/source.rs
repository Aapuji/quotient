/// Represents a source of code.
#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    name: String,
    source: String
}

/// Represents the collection of all the sources used. The src_id for a source is its index in this structure.
#[derive(Debug, Clone)]
pub struct SourceMap {
    files: Vec<Source>
}

/// Represents some span of text in a source file.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file_id: FileId // Used to resolve to actual source lines
}

pub type FileId = usize;
