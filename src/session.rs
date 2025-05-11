use std::sync::{Arc, RwLock};

use crate::{error::{DiagWrapper, LexerDiagnostic, WithContext}, source::{FileId, SourceMap}};

/// Represents the state for one entire run of the compilation process.
#[derive(Debug)]
pub struct Session {
    source_map: Arc<RwLock<SourceMap>>,
    main_id: FileId,
    pub diagnostics: Vec<DiagWrapper>
}

impl Session {
    pub fn new(source_map: Arc<RwLock<SourceMap>>, main_id: FileId, diagnostics: Vec<DiagWrapper>) -> Self {
        Self {
            source_map,
            main_id,
            diagnostics
        }
    }

    pub fn push_diagnostic(&mut self, diagnostic: DiagWrapper) {
        self.diagnostics.push(diagnostic)
    }

    pub fn append_diagnostics(&mut self, diagnostics: Vec<DiagWrapper>) {
        self.diagnostics.extend_from_slice(&diagnostics);
    }

    pub fn source_map(&self) -> Arc<RwLock<SourceMap>> {
        Arc::clone(&self.source_map)
    }

    pub fn main_id(&self) -> &FileId {
        &self.main_id
    }

    pub fn main_src(&self) -> Arc<str> {
        self.source_map
            .read()
            .unwrap()
            .get(self.main_id)
            .unwrap()
            .source()
    }

    pub fn diagnostics(&self) -> &[DiagWrapper] {
        &self.diagnostics
    }

    pub fn diagnostics_mut(&mut self) -> &mut Vec<DiagWrapper> {
        &mut self.diagnostics
    }
}
