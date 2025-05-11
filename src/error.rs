use std::error::Error;
use std::fmt;
use miette::{Diagnostic, LabeledSpan, SourceCode, SourceSpan, Report};
use structs2enum_macro::define_enum_from_structs;
use thiserror::Error;

use crate::source::{FileId, SourceMap};

/// A struct to couple a diagnostic with the source information.
#[derive(Debug)]
pub struct WithContext<'t, E: Diagnostic + ?Sized> {
    error: Box<E>,
    source_map: &'t SourceMap
}

impl<'t, E: Diagnostic + ?Sized> WithContext<'t, E> {
    pub fn new(error: Box<E>, source_map: &'t SourceMap) -> Self {
        Self { error, source_map }
    }
}

impl<'t, E: Error + Diagnostic> fmt::Display for WithContext<'t, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.error, f)
    }
}

impl<'t, E: Error + Diagnostic> Error for WithContext<'t, E> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.error.source()
    }
}

pub fn report_diagnostic(diagnostics: &[WithContext<dyn Diagnostic>], source_map: SourceMap ) {
    for diag in diagnostics {
        Report::new(*diag);
    }
}

macro_rules! impl_with_context {
    ( $err:ty; $code:expr; $label:expr ) => {
        impl<'t> Diagnostic for WithContext<'t, $err> {
            fn code<'a>(&'a self) -> Option<Box<(dyn std::fmt::Display + 'a)>> {
                Some(Box::new($code))
            }

            fn source_code(&self) -> Option<&dyn SourceCode> {
                self.source_map.get(self.error.file_id).map(|s| s as &dyn SourceCode)
            }

            fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
                Some(Box::new(std::iter::once(LabeledSpan::new_with_span(
                    if $label.len() == 0 {
                        None
                    } else {
                        Some($label.to_string())
                    },
                    self.error.span,
                ))))
            }
        }
    };
}

define_enum_from_structs! {
    // A flat error enum for all possible `Lexer` errors.
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
    pub enum LexerError {
        // Numbers
        #[derive(Debug, Error, Diagnostic)]
        #[error("invalid digit for base {base} literal")]
        #[diagnostic(code(lexer::invalid_digit))]
        pub struct InvalidDigit {
            pub base: u8,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        #[derive(Debug, Error, Diagnostic)]
        #[error("cannot have multiple floating points")]
        #[diagnostic(code(lexer::multiple_floats))]
        pub struct MultipleFloatingPoints {
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        #[derive(Debug, Error, Diagnostic)]
        #[error("base {base} floating point literals are not supported")]
        #[diagnostic(code(lexer::nondecimal_float))]
        pub struct NonDecimalFloatingPoint {
            pub base: u8,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        // Strings
        #[derive(Debug, Error, Diagnostic)]
        #[error("unterminated string literal")]
        #[diagnostic(code(lexer::unterminated_string))]
        pub struct UnterminatedString {
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        #[derive(Debug, Error, Diagnostic)]
        #[error("invalid escape sequence: `{escape}`")]
        #[diagnostic(code(lexer::invalid_escape))]
        pub struct InvalidEscapeSequence {
            pub escape: String,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }


        #[derive(Debug, Error, Diagnostic)]
        #[error("invalid unicode escape sequence: {escape}")]
        pub struct InvalidUnicodeEscapeSequence {
            pub escape: String,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }
    }
}

impl_with_context!(InvalidDigit; "lexer::invalid_digit"; "invalid digit here");
impl_with_context!(MultipleFloatingPoints; "lexer::invalid_digit"; "invalid digit here");
impl_with_context!(NonDecimalFloatingPoint; "lexer::invalid_digit"; "invalid digit here");
impl_with_context!(UnterminatedString; "lexer::invalid_digit"; "invalid digit here");
impl_with_context!(InvalidEscapeSequence; "lexer::invalid_digit"; "invalid digit here");
impl_with_context!(InvalidUnicodeEscapeSequence; "lexer::invalid_digit"; "invalid digit here");
