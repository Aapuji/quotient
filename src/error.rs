use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

use structs2enum_macro::define_enum_from_structs;


// Numbers
#[derive(Debug, Error, Diagnostic)]
#[error("invalid digit for base {base} literal")]
#[diagnostic(code(lexer::invalid_digit))]
pub struct InvalidDigit {
    base: u8,
    span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("cannot have multiple floating points")]
#[diagnostic(code(lexer::multiple_floats))]
pub struct MultipleFloatingPoints {
    span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("base {base} floating point literals are not supported")]
#[diagnostic(code(lexer::nondecimal_float))]
pub struct NonDecimalFloat {
    base: u8,
    span: SourceSpan,
}

// Strings
#[derive(Debug, Error, Diagnostic)]
#[error("unterminated string literal")]
#[diagnostic(code(lexer::unterminated_string))]
pub struct UnterminatedString {
    span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("invalid escape sequence: `{escape}`")]
#[diagnostic(code(lexer::invalid_escape))]
pub struct InvalidEscape {
    escape: String,
    span: SourceSpan,
}


#[derive(Debug, Error, Diagnostic)]
#[error("invalid unicode escape sequence: {escape}")]
pub struct InvalidUnicodeEscape {
    escape: String,
    span: SourceSpan,
    
    #[note]
    note: String
}







#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum LexerError {
    // Numbers
    InvalidDigit,
    NonDecimalFloatingPoint,
    MultipleFloatingPoints,
    // Strings
    UnterminatedString,
    InvalidEscapeSequence,
    InvalidUnicodeEscapeSequence
}
