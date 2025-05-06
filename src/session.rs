use std::borrow::Cow;
use codespan_reporting::{files::SimpleFiles, diagnostic::Diagnostic};

use crate::source::FileId;

/// Represents the state for one entire run of the compilation process.
#[derive(Debug)]
pub struct Session<'t> {
    source_map: SimpleFiles<Cow<'t, str>, String>,
    main_id: FileId,
    diagnostics: Vec<Diagnostic<FileId>>
}

impl<'t> Session<'t> {
    pub fn new(source_map: SimpleFiles<Cow<'t, str>, String>, main_id: FileId, diagnostics: Vec<Diagnostic<FileId>>) -> Self {
        Self {
            source_map,
            main_id,
            diagnostics
        }
    }

    pub fn source_map(&self) -> &SimpleFiles<Cow<'t, str>, String> {
        &self.source_map
    }

    pub fn main_id(&self) -> &FileId {
        &self.main_id
    }

    pub fn main_src(&self) -> &str {
        self.source_map.get(self.main_id).unwrap().source()
    }

    pub fn diagnostics(&self) -> &Vec<Diagnostic<FileId>> {
        &self.diagnostics
    }
}

