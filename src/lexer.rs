use std::iter::Peekable;
use std::str::CharIndices;

use crate::source::FileId;

/// Represents the Lexer.
#[derive(Debug)]
pub struct Lexer<'t> {
    src: &'t str,
    chars: Peekable<CharIndices<'t>>,
    pos: usize, // Only holds flat index, line and cols are calculated when needed
}

impl<'t> Lexer<'t> {
    pub fn new(src: &'t str, file_id: FileId) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
            pos: 0
        }
    }
} 
