use bitflags::bitflags;
use phf::phf_map;

use crate::lexer::LexerError;
use crate::source::Span;

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

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum TokenKind {
    // Keywords
    Let, Mut, And, Type, Class, Module, Impl, Deriving, Import, As,
    If, Then, Else, Match, With, Do, End, Using, Matches,
    Rec, Sealed, Opaque, Extends, Some, Any,
    Prefix, Postfix, LAssoc, RAssoc, WithPrec, Lazy, Memo, Auto, Const,
    TodoDirective, UnreachableDirective, NoDirective, IgnoreDirective,
    
    // Identifiers
    Ident,
    Directive,
    
    // Literals
    Int,
    Real,
    Imaginary, 
    StringStart(StringKind, u8), StringSegment, CharacterEsc, UnicodeEsc, StringEnd,
    True, False, //         ^^ number of #s
    Unit,

    // Separators
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Comma, Semicolon,
    Tilde,

    // Operator
    Operator,
    Underscore, // Can be any positive number of underscores, described by the length of the span
    
    // Comments
    Comment,
    DocComment,
    UpperDocComment,

    Error(LexerError),
    Eof
}

pub static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "let" => TokenKind::Let,
    "mut" => TokenKind::Mut,
    "and" => TokenKind::And,
    "type" => TokenKind::Type,
    "class" => TokenKind::Class,
    "module" => TokenKind::Module,
    "impl" => TokenKind::Impl,
    "deriving" => TokenKind::Deriving,
    "import" => TokenKind::Import,
    "as" => TokenKind::As,
    "if" => TokenKind::If,
    "then" => TokenKind::Then,
    "else" => TokenKind::Else,
    "match" => TokenKind::Match,
    "with" => TokenKind::With,
    "do" => TokenKind::Do,
    "end" => TokenKind::End,
    "using" => TokenKind::Using,
    "matches" => TokenKind::Matches,
    "rec" => TokenKind::Rec,
    "sealed" => TokenKind::Sealed,
    "opaque" => TokenKind::Opaque,
    "extends" => TokenKind::Extends,
    "some" => TokenKind::Some,
    "any" => TokenKind::Any,
    "prefix" => TokenKind::Prefix,
    "postfix" => TokenKind::Postfix,
    "lassoc" => TokenKind::LAssoc,
    "rassoc" => TokenKind::RAssoc,
    "with_prec" => TokenKind::WithPrec,
    "lazy" => TokenKind::Lazy,
    "memo" => TokenKind::Memo,
    "auto" => TokenKind::Auto,
    "const" => TokenKind::Const,
    "true" => TokenKind::True,
    "false" => TokenKind::False,
    "unit" => TokenKind::Unit
};

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
    pub struct StringKind : u8 {
        const Normal    = 0;
        const Byte      = 1;
        const Char      = 1 << 1;
        const Format    = 1 << 2;
        const BigFormat = 1 << 3;
        const Raw       = 1 << 4;
        const Trim      = 1 << 5;
    }
}
