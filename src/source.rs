use std::{iter, sync::Arc};
use miette::{MietteSpanContents, SourceCode, SourceSpan};

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

impl Into<SourceSpan> for Span {
    fn into(self) -> SourceSpan {
        SourceSpan::new(self.start.into(), (self.end - self.start).into())
    }
}

/// Represents multiple sources, creating unique identifiers for each.
#[derive(Debug, Clone)]
pub struct SourceMap {
    sources: Vec<Arc<SourceText>>
}

pub type FileId = usize;

impl SourceMap {
    pub fn new() -> Self {
        Self { 
            sources: vec![]
        }
    }

    pub fn add(&mut self, name: String, buf: &str) -> FileId {
        self.sources.push(Arc::new(SourceText::new(name, buf)));

        self.sources.len() - 1
    }

    pub fn get(&self, id: FileId) -> Option<Arc<SourceText>> {
        if id >= self.sources.len() {
            None
        } else {
            Some(Arc::clone(&self.sources[id]))
        }
    }
}

/// Represents some source code.
#[derive(Debug, Clone)]
pub struct SourceText {
    name: String,
    buf: Arc<str>,
    line_starts: Vec<usize>
}

impl SourceText {
    pub fn new(name: String, buf: &str) -> Self {
        Self { 
            name, 
            line_starts: iter::once(0)
                .chain((&buf)
                    .match_indices('\n')
                    .map(|(i, _)| i + 1))
                .collect(),
            buf: buf.into()
        }
    }

    /// Gets the 0-indexed line:col number of the byte offset.
    pub fn coords_of(&self, offset: usize) -> (usize, usize) {
        let (mut low, mut high) = (0, self.line_starts.len() - 1);

        let line_offset = self.line_starts[loop {
            let mid = low + high / 2;

            // low is right next to high
            if self.line_starts[mid] <= offset && offset < self.line_starts[mid + 1] {
                break mid;
            } else if offset < self.line_starts[mid] {
                high = mid - 1;
            } else {
                low = mid + 1
            }
        }];

        (line_offset, offset - line_offset)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn source(&self) -> Arc<str> {
        Arc::clone(&self.buf)
    }
}

impl SourceCode for SourceText {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let range = (span.offset() - context_lines_before)..(span.offset() + span.len() + context_lines_after);
        let (line_start, col) = self.coords_of(range.start);
        let (line_end, _) = self.coords_of(range.end);

        Ok(Box::new(MietteSpanContents::new_named(
            self.name.clone(),
            self.buf[range].as_bytes(), 
            *span, 
            line_start,
            col, 
            line_end - line_start + 1)))
    }
}
