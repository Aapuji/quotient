use std::iter::Peekable;
use std::str::CharIndices;

use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

use crate::session::Session;
use crate::source::{FileId, Span};
use crate::token::{self, Token, TokenKind};

/// Represents the Lexer.
#[derive(Debug)]
pub struct Lexer<'t> {
    chars: Peekable<CharIndices<'t>>,
    ch: Option<char>,
    pos: usize,
    file_id: FileId,
}

impl<'t> Lexer<'t> {
    pub fn new(session: &'t mut Session) -> Self {
        Self::with_file_id(session, *session.main_id())
    }

    pub fn with_file_id(session: &'t mut Session, file_id: FileId) -> Self {
        let src = session.source_map().get(file_id).unwrap().source();
        let mut chars = src.char_indices().peekable();
        let ch = chars.peek().map(|&(_, ch)| ch);

        Self {
            chars,
            ch,
            pos: 0,
            file_id,
        }
    }

    fn at_end(&mut self) -> bool {
        self.ch.is_none()
    }

    fn next(&mut self) -> Option<(usize, char)> {
        if let Some((i, ch)) = self.chars.next() {
            if let Some(c) = self.ch {
                self.pos += c.len_utf8();
            }
            self.ch = Some(ch);
            Some((i, ch))
        } else {
            self.ch = None;
            None
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, ch)| ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    fn lex_number(&mut self, tokens: &mut Vec<Token>, mut base: u8) -> Vec<Box<dyn Diagnostic + Send + Sync>> {
        let start_pos = self.pos;
        let mut num_kind = TokenKind::Int;
        let mut diagnostics = Vec::new();

        macro_rules! lex_base {
            ($n:expr) => {
                {
                    self.next().unwrap();
                    base = $n;
                }
            };
        }

        if base == 0 {
            if let Some('0') = self.ch {
                self.next();
                match self.ch {
                    Some('b') | Some('B') => lex_base!(2),
                    Some('o') | Some('O') => lex_base!(8),
                    Some('d') | Some('D') => lex_base!(10),
                    Some('x') | Some('X') => lex_base!(16),
                    _ => base = 10,
                }
            } else {
                base = 10;
            }
        }

        while let Some(c) = self.ch {
            if c == '_' {
                self.next();
            } else if c.is_ascii_digit() {
                if c >= '0' && c < ('0' as u8 + base.min(10)) as char {
                    self.next();
                } else {
                    let start = self.pos;
                    let end = self.pos + 1;
                    tokens.push(Token::new(TokenKind::Error(LexerError::InvalidDigit), Span::new(start, end, self.file_id)));
                    diagnostics.push(Box::new(InvalidDigitError {
                        base,
                        span: (start.into(), 1).into(),
                    }));
                    self.next();
                }
            } else if c == 'i' {
                self.next();
                num_kind = TokenKind::Imaginary;
                break;
            } else if c.is_ascii_alphabetic() && base == 16 {
                if c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F' {
                    self.next();
                } else {
                    break;
                }
            } else if c == '.' {
                if let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        if base == 10 {
                            if let TokenKind::Int = num_kind {
                                self.next();
                                num_kind = TokenKind::Real;
                            } else {
                                let start = self.pos;
                                diagnostics.push(Box::new(MultipleFloatingPointsError {
                                    span: (start.into(), 1).into(),
                                }));
                                self.next();
                            }
                        } else {
                            let start = self.pos;
                            diagnostics.push(Box::new(NonDecimalFloatError {
                                base,
                                span: (start.into(), 1).into(),
                            }));
                            self.next();
                        }
                    } else {
                        break;
                    }
                } else {
                    self.next();
                }
            } else {
                break
            }
        }

        if diagnostics.is_empty() {
            tokens.push(Token::new(num_kind, Span::new(start_pos, self.pos, self.file_id)));
        }

        diagnostics
    }

    fn lex_string(&mut self, tokens: &mut Vec<Token>) -> Vec<Box<dyn Diagnostic + Send + Sync>> {
        let start_pos = self.pos;
        let mut diagnostics = Vec::new();

        loop {
            self.next();

            match self.ch {
                Some('"') => {
                    self.next();
                    break;
                }
                Some('\\') => {
                    self.next();
                    match self.ch {
                        Some('\\') | Some('\'') | Some('"') |
                        Some('a') | Some('b') | Some('e') | Some('f') |
                        Some('n') | Some('r') | Some('s') | Some('t') |
                        Some('v') | Some('0') | Some(' ') | Some('\r') |
                        Some('\n') => (),
                        Some('x') => (), // TODO
                        Some('u') | Some('U') => match self.peek() {
                            _ => (), // TODO
                        },
                        Some(c) => {
                            let start = self.pos;
                            diagnostics.push(Box::new(InvalidEscapeError {
                                escape: c,
                                span: (start.into(), 1).into(),
                            }));
                            self.next();
                        }
                        None => break,
                    }
                }
                Some(_) => (),
                None => {
                    diagnostics.push(Box::new(UnterminatedStringError {
                        span: (start_pos.into(), (self.pos - start_pos).into()).into(),
                    }));
                    break;
                }
            }
        }

        if diagnostics.is_empty() {
            tokens.push(Token::new(TokenKind::String, Span::new(start_pos, self.pos, self.file_id)));
        }

        diagnostics
    }

    pub fn lex_token(&mut self, tokens: &mut Vec<Token>) -> Vec<Box<dyn Diagnostic + Send + Sync>> {
        let mut diagnostics = Vec::new();

        self.skip_whitespace();

        let res = match self.ch {
            Some(c) if c.is_ascii_digit() => Some(diagnostics.append(&mut self.lex_number(tokens, 0))),
            Some('"') => Some(diagnostics.append(&mut self.lex_string(tokens))),
            Some(_) => todo!(),
            None => {
                tokens.push(Token::new(TokenKind::Eof, Span::new(self.pos, self.pos, self.file_id)));
                Some(())
            }
        };

        while !self.at_end() {
            self.next();
        }

        if let Some(_) = res {
            diagnostics
        } else {
            self.lex_token(tokens)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum LexerError {
    InvalidDigit,
    NonDecimalFloatingPoint,
    MultipleFloatingPoints,
    UnterminatedString,
    InvalidEscapeSequence,
}

#[derive(Debug, Error, Diagnostic)]
#[error("invalid digit for base {base} literal")]
#[diagnostic(code(lexer::invalid_digit))]
pub struct InvalidDigitError {
    base: u8,
    #[label("invalid digit here")]
    span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("cannot have multiple floating points")]
#[diagnostic(code(lexer::multiple_floats))]
pub struct MultipleFloatingPointsError {
    #[label("another decimal point found here")]
    span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("base {base} floating point literals are not supported")]
#[diagnostic(code(lexer::nondecimal_float))]
pub struct NonDecimalFloatError {
    base: u8,
    #[label("non-decimal floating point here")]
    span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("unterminated string literal")]
#[diagnostic(code(lexer::unterminated_string))]
pub struct UnterminatedStringError {
    #[label("string started here")]
    span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("invalid escape sequence: `{escape}`")]
#[diagnostic(code(lexer::invalid_escape))]
pub struct InvalidEscapeError {
    escape: char,
    #[label("invalid escape here")]
    span: SourceSpan,
}
