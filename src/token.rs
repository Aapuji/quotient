use crate::num::{Int, Real};
use crate::source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TokenKind {
    // Keywords
    Let, Mut, And, Type, Class, Module, Impl, Deriving, Import, As,
    If, Then, Else, Match, With, Do, End, Using, Matches,
    Rec, Proc, Fun, Sealed, Extends, Some, 
    Prefix, Postfix, LAssoc, RAssoc, WithPrec, Lazy, Memo,
    True, False, Unit, Underscore,
    
    // Identifiers
    Ident(String),
    
    // Literals
    Int(Int),
    Real(Real),
    Imaginary(Real),
    String(String),
    Bool(bool),

    // Separators
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Comma, Semicolon, Colon,
    Tilde,

    // Operator
    Operator(String),
    
    // Comments
    DocComment(String),
    UpperDocComment(String),

    Invalid(char),
    Eof
}
