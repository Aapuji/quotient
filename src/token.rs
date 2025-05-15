use std::backtrace;

use bitflags::bitflags;

use crate::{lexer::LexerError, source::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TokenKind {
    // Keywords
    Let, Mut, And, Type, Class, Module, Impl, Deriving, Import, As,
    If, Then, Else, Match, With, Do, End, Using, Matches,
    Rec, Proc, Fun, Sealed, Extends, Some, 
    Prefix, Postfix, LAssoc, RAssoc, WithPrec, Lazy, Memo,
    
    // Identifiers
    Ident,
    
    // Literals
    Int,
    Real,
    Imaginary,
    StringStart(StringKind), StringSegment, CharacterEsc, UnicodeEsc, StringEnd,
    True, False,
    Unit,

    // Separators
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Comma, Semicolon, Colon,
    Tilde,

    // Operator
    Operator,
    Underscore,
    
    // Comments
    DocComment,
    UpperDocComment,

    Error(LexerError),
    Eof
}


bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
    pub struct StringKind : u8 {
        const Normal    = 0;
        const Format    = 1;
        const BigFormat = 1 << 1; 
        const Byte      = 1 << 2;
        const Raw       = 1 << 3;
        const Regex     = 1 << 4;
    }
}
