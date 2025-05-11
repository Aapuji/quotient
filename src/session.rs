use miette::Diagnostic;

use crate::source::{FileId, SourceMap};

/// Represents the state for one entire run of the compilation process.
#[derive(Debug)]
pub struct Session {
    source_map: SourceMap,
    main_id: FileId,
    diagnostics: Vec<Box<dyn Diagnostic>>
}

impl Session {
    pub fn new(source_map: SourceMap, main_id: FileId, diagnostics: Vec<Box<dyn Diagnostic>>) -> Self {
        Self {
            source_map,
            main_id,
            diagnostics
        }
    }

    pub fn push_diagnostic(&mut self, diagnostic: Box<dyn Diagnostic>) {
        self.diagnostics.push(diagnostic)
    }

    pub fn append_diagnostics(&mut self, diagnostics: &mut Vec<Box<dyn Diagnostic>>) {
        self.diagnostics.append(diagnostics);
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    pub fn main_id(&self) -> &FileId {
        &self.main_id
    }

    pub fn main_src(&self) -> &str {
        self.source_map.get(self.main_id).unwrap().source()
    }

    pub fn diagnostics(&self) -> &[Box<dyn Diagnostic>] {
        &self.diagnostics
    }
}

