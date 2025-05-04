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

