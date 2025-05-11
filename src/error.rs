use std::any::Any;
use std::sync::RwLock;
use std::{error::Error, sync::Arc};
use std::fmt;
use miette::{Diagnostic, LabeledSpan, SourceCode, SourceSpan, Report};
use structs2enum_macro::define_enum_from_structs;
use thiserror::Error;

use crate::source::{FileId, SourceMap, SourceText};

pub fn report_diagnostic<'t>(diagnostic: DiagWrapper) {
    eprintln!("{}", Report::new(diagnostic));
}

#[derive(Debug, Clone, Error)]
#[error(transparent)]
pub struct DiagWrapper(pub Arc<WithContext>);

impl Diagnostic for DiagWrapper {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        self.0.labels()
    }

    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.0.code()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.0.diagnostic_source()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.0.help()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.0.related()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.0.severity()
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        self.0.source_code()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.0.code()
    }
}

/// A struct to couple a diagnostic with the source information.
#[derive(Debug, Clone)]
pub struct WithContext {
    error: LexerDiagnostic,
    source_map: Arc<RwLock<SourceMap>>
}

impl WithContext {
    pub fn new(error: LexerDiagnostic, source_map: Arc<RwLock<SourceMap>>) -> Self {
        Self { error, source_map }
    }

    pub fn get_source_text(&self) -> Arc<SourceText> {
        let file_id = match &self.error {
            LexerDiagnostic::InvalidDigit(x) => x.file_id,
            LexerDiagnostic::MultipleFloatingPoints(x) => x.file_id,
            LexerDiagnostic::NonDecimalFloatingPoint(x) => x.file_id,
            LexerDiagnostic::UnterminatedString(x) => x.file_id,
            LexerDiagnostic::InvalidEscapeSequence(x) => x.file_id,
            LexerDiagnostic::InvalidUnicodeEscapeSequence(x) => x.file_id
        };

        self.source_map
            .read()
            .unwrap()
            .get(file_id)
            .unwrap()
    }
}

impl fmt::Display for WithContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.error, f)
    }
}

impl Error for WithContext {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.error.source()
    }
}

impl Diagnostic for WithContext {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.error.code()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.error.diagnostic_source()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.error.help()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        self.error.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.error.related()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.error.severity()
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        self.error.source_code()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.error.url()
    }
}

// macro_rules! impl_with_context {
//     ( $err:ty; $code:expr; $label:expr ) => {
//         impl Diagnostic for WithContext<$err> {
//             fn code<'a>(&'a self) -> Option<Box<(dyn std::fmt::Display + 'a)>> {
//                 Some(Box::new($code))
//             }

//             fn source_code(&self) -> Option<&dyn SourceCode> {
//                 self.source_map.get(self.error.file_id).map(|s| s as &dyn SourceCode)
//             }

//             fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
//                 Some(Box::new(std::iter::once(LabeledSpan::new_with_span(
//                     if $label.len() == 0 {
//                         None
//                     } else {
//                         Some($label.to_string())
//                     },
//                     self.error.span,
//                 ))))
//             }
//         }
//     };
// }

define_enum_from_structs! {
    // A flat error enum for all possible `Lexer` errors.
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
    pub enum LexerError,
    #[derive(Debug, Clone, /*Error, Diagnostic*/)]
    pub enum LexerDiagnostic {
        // Numbers
        #[derive(Debug, Clone, Error, Diagnostic)]
        #[error("invalid digit for base {base} literal")]
        #[diagnostic(code(lexer::invalid_digit))]
        pub struct InvalidDigit {
            pub base: u8,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        #[derive(Debug, Clone, Error, Diagnostic)]
        #[error("cannot have multiple floating points")]
        #[diagnostic(code(lexer::multiple_floats))]
        pub struct MultipleFloatingPoints {
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        #[derive(Debug, Clone, Error, Diagnostic)]
        #[error("base {base} floating point literals are not supported")]
        #[diagnostic(code(lexer::nondecimal_float))]
        pub struct NonDecimalFloatingPoint {
            pub base: u8,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        // Strings
        #[derive(Debug, Clone, Error, Diagnostic)]
        #[error("unterminated string literal")]
        #[diagnostic(code(lexer::unterminated_string))]
        pub struct UnterminatedString {
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }

        #[derive(Debug, Clone, Error, Diagnostic)]
        #[error("invalid escape sequence: `{escape}`")]
        #[diagnostic(code(lexer::invalid_escape))]
        pub struct InvalidEscapeSequence {
            pub escape: String,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }


        #[derive(Debug, Clone, Error, Diagnostic)]
        #[error("invalid unicode escape sequence: {escape}")]
        pub struct InvalidUnicodeEscapeSequence {
            pub escape: String,
            #[label]
            pub span: SourceSpan,
            pub file_id: FileId
        }
    }
}

macro_rules! match_variants {
    ( $value:expr, $enum:ident, $eval:expr, $( $variant:ident ),* ) => {
        match $value {
            $(
                $enum::$variant(data) => $eval(data)
            ),*
        }
    };
}

impl fmt::Display for LexerDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match_variants! {
            self, 
            Self, 
            |x| { fmt::Display::fmt(x, f) },
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }
}

impl Error for LexerDiagnostic {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match_variants! {
            self,
            Self,
            Error::source,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }
}

impl Diagnostic for LexerDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        match_variants! {
            self,
            Self,
            Diagnostic::code,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        match_variants! {
            self,
            Self,
            Diagnostic::diagnostic_source,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        match_variants! {
            self,
            Self,
            Diagnostic::help,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match_variants! {
            self,
            Self,
            Diagnostic::labels,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        match_variants! {
            self,
            Self,
            Diagnostic::related,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }

    fn severity(&self) -> Option<miette::Severity> {
        match_variants! {
            self,
            Self,
            Diagnostic::severity,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        match_variants! {
            self,
            Self,
            Diagnostic::source_code,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        match_variants! {
            self,
            Self,
            Diagnostic::url,
            InvalidDigit,
            MultipleFloatingPoints,
            NonDecimalFloatingPoint,
            UnterminatedString,
            InvalidEscapeSequence,
            InvalidUnicodeEscapeSequence
        }
    }
}


// impl_with_context!(InvalidDigit; "lexer::invalid_digit"; "invalid digit here");
// impl_with_context!(MultipleFloatingPoints; "lexer::invalid_digit"; "invalid digit here");
// impl_with_context!(NonDecimalFloatingPoint; "lexer::invalid_digit"; "invalid digit here");
// impl_with_context!(UnterminatedString; "lexer::invalid_digit"; "invalid digit here");
// impl_with_context!(InvalidEscapeSequence; "lexer::invalid_digit"; "invalid digit here");
// impl_with_context!(InvalidUnicodeEscapeSequence; "lexer::invalid_digit"; "invalid digit here");
