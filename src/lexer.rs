use std::iter::Peekable;
use std::str::CharIndices;

use miette::Diagnostic;

use crate::error::{self, LexerError, WithContext};
use crate::session::Session;
use crate::source::{FileId, SourceMap, Span};
use crate::token::{Token, TokenKind};

/// Represents the Lexer.
#[derive(Debug)]
pub struct Lexer<'t> {
    chars: Peekable<CharIndices<'t>>,
    ch: Option<char>, // Current char 
    pos: usize, // Holds flat index of next char, line and cols are calculated when needed
    file_id: FileId,
    source_map: &'t SourceMap
}

impl<'t> Lexer<'t> {
    /// Creates a new `Lexer` for the main file.
    pub fn new(session: &'t mut Session) -> Self {        
        Self::with_file_id(session, *session.main_id())
    }

    /// Given `file_id` must be valid.
    pub fn with_file_id(session: &'t mut Session, file_id: FileId) -> Self {
        let source_map = session.source_map();
        let src = source_map.get(file_id).unwrap().source();
        let mut chars = src.char_indices().peekable();
        let mut ch = None;
        if chars.peek().is_some() {
            ch = Some(chars.next().unwrap().1);
        }
        
        let lexer = Self {
            chars,
            ch,
            pos: 0,
            file_id,
            source_map
        };


        lexer
    }

    /// Checks the current character is the last one.
    fn at_end(&mut self) -> bool {
        self.ch.is_none()
    }

    /// Advances iterator to next character and byte index.
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

    /// Just peeks at the next character.
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, ch)| ch)
    }

    /// Skips whitespace such that when it finishes, the current character is not whitespace.
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    /// Lexes a number, given the base. If `base` is 0, then it will figure out the base.
    /// 
    /// Must be called only when the current character in the iterator is a digit.
    fn lex_number(&mut self, tokens: &mut Vec<Token>, mut base: u8) -> Vec<WithContext<dyn Diagnostic>> {
        let start_pos = self.pos;
        let mut num_kind = TokenKind::Int;
        let mut diagnostics: Vec<WithContext<dyn Diagnostic>> = Vec::new();

        macro_rules! lex_base {
            ($n:expr) => {
                {
                    self.next().unwrap();
                    base = $n;
                }
            };
        }

        // If base=0, check to see if the number starts with 0_
        if base == 0 {
            if let Some('0') = self.ch {
                self.next();

                match self.ch {
                    // Base 2
                    Some('b') |
                    Some('B') => lex_base!(2),

                    // Base 8
                    Some('o') |
                    Some('O') => lex_base!(8),

                    // Base 10
                    Some('d') |
                    Some('D') => lex_base!(10),

                    // Base 16
                    Some('x') |
                    Some('X') => lex_base!(16),

                    // Don't deal with the other stuff; 
                    // This section is only for dealing with the base
                    Some(_) |
                    None    => base = 10,
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
                    let span = Span::new(self.pos, self.pos + 1, self.file_id);

                    tokens.push(Token::new(
                        TokenKind::Error(LexerError::InvalidDigit), 
                        span
                    ));

                    diagnostics.push(WithContext::new(
                        Box::new(error::InvalidDigit {
                            base,
                            span: span.into(),
                            file_id: span.file_id()
                        }),
                        self.source_map
                    ));
                                        
                    self.next();
                }
            // i for imaginary numbers
            } else if c == 'i' {
                self.next();
                num_kind = TokenKind::Imaginary;
                break;
            } else if c.is_ascii_alphabetic() && base == 16 {
                // Base 16 digits
                if c >= 'a' && c <= 'f' ||
                   c >= 'A' && c <= 'F' {
                    self.next();
                // Anything else
                } else {
                    break;
                }
            // for decimal points
            } else if c == '.' {
                if let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        if base == 10 {
                            if let TokenKind::Int = num_kind {
                                self.next();
                                num_kind = TokenKind::Real;
                            } else {
                                let span = Span::new(self.pos, self.pos + 1, self.file_id);

                                tokens.push(Token::new(
                                    TokenKind::Error(LexerError::MultipleFloatingPoints),
                                    span
                                ));

                                diagnostics.push(WithContext::new(
                                    Box::new(error::MultipleFloatingPoints {
                                        span: span.into(),
                                        file_id: span.file_id()
                                    }),
                                    self.source_map
                                ));

                                self.next();
                            }
                        } else {
                            let span = Span::new(self.pos, self.pos + 1, self.file_id);

                            tokens.push(Token::new(
                                TokenKind::Error(LexerError::NonDecimalFloatingPoint), 
                                span
                            ));
                            
                            diagnostics.push(WithContext::new(
                                Box::new(error::NonDecimalFloatingPoint {
                                    base,
                                    span: span.into(),
                                    file_id: span.file_id()
                                }),
                                self.source_map
                            ));

                            self.next();
                        }
                    } else if base == 16 && (
                        c >= 'a' && c <= 'f' ||
                        c >= 'A' && c <= 'F'
                    ) {
                        let span = Span::new(self.pos, self.pos + 1, self.file_id);

                        tokens.push(Token::new(
                            TokenKind::Error(LexerError::NonDecimalFloatingPoint),
                            span
                        ));

                        diagnostics.push(WithContext::new(
                            Box::new(error::NonDecimalFloatingPoint {
                                base,
                                span: span.into(),
                                file_id: span.file_id()
                            }),
                            self.source_map
                        ));
                                                
                        self.next();
                    } else if c.is_whitespace() {
                        self.next();
                        num_kind = TokenKind::Int;
                        break;
                    } else {
                        break;
                    }
                // Ends with .EOF, treat it as an int
                } else {
                    self.next();
                }
            // Anything else
            } else {
                break
            }
        }

        if diagnostics.len() == 0 {
            tokens.push(Token::new(
                num_kind, 
                Span::new(start_pos, self.pos, self.file_id)
            )); 
        }

        diagnostics
    }

    /// Lexes a regular string.
    fn lex_string(&mut self, tokens: &mut Vec<Token>) -> Vec<WithContext<dyn Diagnostic>> {
        let start_pos = self.pos;
        let mut diagnostics: Vec<WithContext<dyn Diagnostic>> = Vec::new();
        let end_last_line = self.pos;

        // Is currently at `"`.

        loop {
            // Go to next character
            self.next();

            match self.ch {
                Some('"') => {
                    self.next();
                    break
                }

                Some('\\') => {
                    let estart = self.pos;
                    self.next();

                    match self.ch {
                        // Single-char Escape Sequences
                        Some('\\') |
                        Some('\'') |
                        Some('"')  |
                        Some('a')  |
                        Some('b')  |
                        Some('e')  |
                        Some('f')  |
                        Some('n')  |
                        Some('r')  |
                        Some('s')  |
                        Some('t')  |
                        Some('v')  |
                        Some('0')  |
                        Some(' ')  |
                        Some('\r') | // Treated the same as below if the next character is \n
                        Some('\n') => (),

                        // Unicode Escape Sequence (in decimal)
                        Some('u') | 
                        Some('U') => {                            
                            self.next();

                            match self.ch {
                                Some(c) if c.is_ascii_alphanumeric() => (),
                                Some(c) => {
                                    let span = Span::new(estart, self.pos + 1, self.file_id);

                                    tokens.push(Token::new(
                                        TokenKind::Error(LexerError::InvalidEscapeSequence),
                                        span
                                    ));
                                    
                                    diagnostics.push(WithContext::new(
                                        Box::new(error::InvalidUnicodeEscapeSequence {
                                            escape: format!("\\u{c}"),
                                            span: span.into(),
                                            file_id: span.file_id()
                                        }),
                                        self.source_map
                                    ));
                                },
                                None => {
                                    let span = Span::new(start_pos, self.pos + 1, self.file_id);

                                    tokens.push(Token::new(
                                        TokenKind::Error(LexerError::UnterminatedString),
                                        span
                                    ));
                                    
                                    diagnostics.push(WithContext::new(
                                        Box::new(error::UnterminatedString {
                                            span: span.into(),
                                            file_id: span.file_id()
                                        }),
                                        self.source_map
                                    ));

                                    break   
                                } 
                            }
                        },

                        // Ascii Escape Sequence (in hexadecimal)
                        Some('x') => todo!(),

                        // Invalid
                        Some(c) => {
                            let span = Span::new(estart, self.pos + 1, self.file_id);

                            tokens.push(Token::new(
                                TokenKind::Error(LexerError::InvalidUnicodeEscapeSequence),
                                span));
                            
                            diagnostics.push(WithContext::new(
                                Box::new(error::InvalidEscapeSequence {
                                    escape: format!("\\{c}"),
                                    span: span.into(),
                                    file_id: span.file_id()
                                }),
                                self.source_map
                            ));
                        },

                        // EOF
                        None => todo!()
                    }
                }

                Some(_) => (),

                None => {
                    let span = Span::new(start_pos, self.pos + 1, self.file_id);

                    tokens.push(Token::new(
                        TokenKind::Error(LexerError::UnterminatedString),
                        Span::new(start_pos, start_pos+1, self.file_id)));
                    
                    diagnostics.push(WithContext::new(
                        Box::new(error::UnterminatedString {
                            span: span.into(),
                            file_id: span.file_id()
                        }),
                        self.source_map
                    ));

                    break
                }
            }
        }

        if diagnostics.len() == 0 {
            tokens.push(Token::new(
                TokenKind::String, 
                Span::new(start_pos, self.pos, self.file_id)));
        }

        diagnostics
    }

    /// Lexes an identifier or a keyword.
    fn lex_ident(&mut self, tokens: &mut Vec<Token>) -> Vec<WithContext<dyn Diagnostic>> {
        todo!()
    }

    /// Chooses either the underscore operators, or `_<ident>`.
    fn lex_underscore(&mut self, tokens: &mut Vec<Token>) -> Vec<WithContext<dyn Diagnostic>> {
        todo!()
    }

    /// Lexes an operator, a doc comment, or a comment, producing a token for either, including a regular comment. That should be skipped in a future phase.
    fn lex_operator(&mut self, tokens: &mut Vec<Token>) -> Vec<WithContext<dyn Diagnostic>> {
        todo!()
    }

    fn skip_comment(&mut self, tokens: &mut Vec<Token>) {
        todo!()
    }

    /// Lexes one token.
    pub fn lex_token(&mut self, tokens: &mut Vec<Token>) -> Vec<WithContext<dyn Diagnostic>> {
        let mut diagnostics = Vec::new();
        
        self.skip_whitespace();

        // None if passed over a comment, and thus should repeat
        match self.ch {
            // Number
            Some(c) if c.is_ascii_digit() => diagnostics.append(&mut self.lex_number(tokens, 0)),
            
            // String
            Some('"') => diagnostics.append(&mut self.lex_string(tokens)),
            
            // Identifiers, Keywords, Or Prefixed Strings
            Some(c) if c.is_alphabetic() => diagnostics.append(&mut self.lex_ident(tokens)),

            // Underscore operators (satisfies regex _+), or identifier prepended by underscores
            Some('_') => diagnostics.append(&mut self.lex_underscore(tokens)),
            
            // Operator Symbols (and Comments)
            // Operators satisfy the following regex: 
            //   [!@$#^&|*-+=<>.:'?/][~%!@$#^&|*-+=<>.:'?/]*
            // #, #(, ##, ##(, ##<, ##^, ##<(, ##^( are all valid comment starters
            Some('!')  |
            Some('@')  |
            Some('$')  |
            Some('#')  |
            Some('^')  |
            Some('&')  |
            Some('|')  |
            Some('*')  |
            Some('-')  |
            Some('+')  |
            Some('=')  |
            Some('<')  |
            Some('>')  |
            Some('.')  |
            Some(':')  |
            Some('\'') |
            Some('?')  |
            Some('/')  => diagnostics.append(&mut self.lex_operator(tokens)),

            Some(_) => todo!(),

            None => tokens.push(Token::new(
                TokenKind::Eof, 
                Span::new(self.pos, self.pos, self.file_id)
            ))
        };

        diagnostics
    }
} 
