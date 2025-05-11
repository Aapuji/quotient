use std::iter::Peekable;
use std::str::CharIndices;
use std::sync::{Arc, RwLock};

use crate::error::{self, DiagWrapper, LexerDiagnostic, LexerError, WithContext};
use crate::source::{FileId, SourceMap, Span};
use crate::token::{Token, TokenKind};

/// Represents the Lexer.
#[derive(Debug)]
pub struct Lexer {
    chars: Peekable<CharIndices<'static>>,
    ch: Option<char>, // Current char 
    pos: usize, // Holds flat index of next char, line and cols are calculated when needed
    file_id: FileId,
    source_map: Arc<RwLock<SourceMap>>,
    source: Arc<str> // Keeps the source alive to prevent UB
}

impl Lexer {
    /// Creates a new `Lexer` for the main file.
    pub fn new(session: (Arc<RwLock<SourceMap>>, FileId)) -> Self {        
        Self::with_file_id(session.0,session.1)
    }

    /// Given `file_id` must be valid.
    pub fn with_file_id(source_map: Arc<RwLock<SourceMap>>, file_id: FileId) -> Self {
        let source = {
            let map = source_map.read().unwrap();
            map.get(file_id).unwrap().source()
        };

        // Unsafely extend lifetime
        // Safe iff Arc<str> is stored to keep it alive
        let static_str: &'static str = unsafe {
            std::mem::transmute::<&str, &'static str>(&*source)
        };

        let mut chars = static_str
            .char_indices()
            .peekable();
        let ch = if chars.peek().is_some() {
            chars.next().map(|(_, c)| c)
        } else {
            None
        };

        let lexer = Self {
            chars: chars,
            ch,
            pos: 0,
            file_id,
            source_map,
            source // Kept to keep source alive
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
    fn lex_number(&mut self, diagnostics: &mut Vec<DiagWrapper>, tokens: &mut Vec<Token>, mut base: u8) {
        let start_pos = self.pos;
        let mut num_kind = TokenKind::Int;

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

                    diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::InvalidDigit(
                        error::InvalidDigit {
                            base,
                            span: span.into(),
                            file_id: span.file_id()
                        }),
                        Arc::clone(&self.source_map)
                    ))));
                                        
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

                                diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::MultipleFloatingPoints(
                                    error::MultipleFloatingPoints {
                                        span: span.into(),
                                        file_id: span.file_id()
                                    }),
                                    Arc::clone(&self.source_map)
                                ))));

                                self.next();
                            }
                        } else {
                            let span = Span::new(self.pos, self.pos + 1, self.file_id);

                            tokens.push(Token::new(
                                TokenKind::Error(LexerError::NonDecimalFloatingPoint), 
                                span
                            ));
                            
                            diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::NonDecimalFloatingPoint(
                                error::NonDecimalFloatingPoint {
                                    base,
                                    span: span.into(),
                                    file_id: span.file_id()
                                }),
                                Arc::clone(&self.source_map)
                            ))));

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

                        diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::NonDecimalFloatingPoint(
                            error::NonDecimalFloatingPoint {
                                base,
                                span: span.into(),
                                file_id: span.file_id()
                            }),
                            Arc::clone(&self.source_map)
                        ))));
                                                
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
    }

    /// Lexes a regular string.
    fn lex_string(&mut self, diagnostics: &mut Vec<DiagWrapper>, tokens: &mut Vec<Token>) {
        let start_pos = self.pos;
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
                                    
                                    diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::InvalidUnicodeEscapeSequence(
                                        error::InvalidUnicodeEscapeSequence {
                                            escape: format!("\\u{c}"),
                                            span: span.into(),
                                            file_id: span.file_id()
                                        }),
                                        Arc::clone(&self.source_map)
                                    ))));
                                },
                                None => {
                                    let span = Span::new(start_pos, self.pos + 1, self.file_id);

                                    tokens.push(Token::new(
                                        TokenKind::Error(LexerError::UnterminatedString),
                                        span
                                    ));
                                    
                                    diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::UnterminatedString(
                                        error::UnterminatedString {
                                            span: span.into(),
                                            file_id: span.file_id()
                                        }),
                                        Arc::clone(&self.source_map)
                                    ))));

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
                            
                            diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::InvalidEscapeSequence(
                                error::InvalidEscapeSequence {
                                    escape: format!("\\{c}"),
                                    span: span.into(),
                                    file_id: span.file_id()
                                }),
                                Arc::clone(&self.source_map)
                            ))));
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
                    
                    diagnostics.push(DiagWrapper(Arc::new(WithContext::new(LexerDiagnostic::UnterminatedString(
                        error::UnterminatedString {
                            span: span.into(),
                            file_id: span.file_id()
                        }),
                        Arc::clone(&self.source_map)
                    ))));

                    break
                }
            }
        }

        if diagnostics.len() == 0 {
            tokens.push(Token::new(
                TokenKind::String, 
                Span::new(start_pos, self.pos, self.file_id)));
        }
    }

    /// Lexes an identifier or a keyword.
    fn lex_ident(&mut self, diagnostics: &mut Vec<DiagWrapper>, tokens: &mut Vec<Token>) {
        todo!()
    }

    /// Chooses either the underscore operators, or `_<ident>`.
    fn lex_underscore(&mut self, diagnostics: &mut Vec<DiagWrapper>, tokens: &mut Vec<Token>) {
        todo!()
    }

    /// Lexes an operator, a doc comment, or a comment, producing a token for either, including a regular comment. That should be skipped in a future phase.
    fn lex_operator(&mut self, diagnostics: &mut Vec<DiagWrapper>, tokens: &mut Vec<Token>) {
        todo!()
    }

    /// Lexes one token.
    pub fn lex_token(&mut self, diagnostics: &mut Vec<DiagWrapper>, tokens: &mut Vec<Token>) {        
        self.skip_whitespace();

        // None if passed over a comment, and thus should repeat
        match self.ch {
            // Number
            Some(c) if c.is_ascii_digit() => self.lex_number(diagnostics, tokens, 0),
            
            // String
            Some('"') => self.lex_string(diagnostics, tokens),
            
            // Identifiers, Keywords, Or Prefixed Strings
            Some(c) if c.is_alphabetic() => self.lex_ident(diagnostics, tokens),

            // Underscore operators (satisfies regex _+), or identifier prepended by underscores
            Some('_') => self.lex_underscore(diagnostics, tokens),
            
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
            Some('/')  => self.lex_operator(diagnostics, tokens),

            Some(_) => todo!(),

            None => tokens.push(Token::new(
                TokenKind::Eof, 
                Span::new(self.pos, self.pos, self.file_id)
            ))
        };
    }
} 
