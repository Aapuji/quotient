use std::iter::Peekable;
use std::str::CharIndices;

use codespan_reporting::diagnostic::{self, Diagnostic, Label};

use crate::session::Session;
use crate::source::{FileId, Span};
use crate::token::{self, StringKind, Token, TokenKind, KEYWORDS};

/// Represents the Lexer.
#[derive(Debug)]
pub struct Lexer<'t> {
    src: &'t str,
    chars: Peekable<CharIndices<'t>>,
    ch: Option<char>, // Current char 
    pos: usize, // Holds flat index of next char, line and cols are calculated when needed
    file_id: FileId
}

impl<'t> Lexer<'t> {
    /// Creates a new `Lexer` for the main file.
    pub fn new(session: &'t mut Session) -> Self {        
        Self::with_file_id(session, *session.main_id())
    }

    /// Given `file_id` must be valid.
    pub fn with_file_id(session: &'t mut Session, file_id: FileId) -> Self {
        let src = session.source_map().get(file_id).unwrap().source();
        let mut chars = src.char_indices().peekable();
        let mut ch = None;
        if chars.peek().is_some() {
            ch = Some(chars.next().unwrap().1);
        }
        
        Self {
            src,
            chars,
            ch,
            pos: 0,
            file_id
        }
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
    fn lex_number(&mut self, tokens: &mut Vec<Token>, mut base: u8) -> Vec<Diagnostic<FileId>> {
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
                    let start = self.pos;
                    let end = self.pos + 1;

                    tokens.push(Token::new(
                        TokenKind::Error(LexerError::InvalidDigit), 
                        Span::new(start, end, self.file_id)));

                    diagnostics.push(Diagnostic::error()
                        .with_message(format!("invalid digit for base {} literal", base))
                        .with_label(Label::primary(self.file_id, start..end)));
                                        
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
                                let start = self.pos;
                                let end = self.pos + 1;

                                tokens.push(Token::new(
                                    TokenKind::Error(LexerError::MultipleFloatingPoints),
                                    Span::new(start, end, self.file_id)
                                ));

                                diagnostics.push(Diagnostic::error()
                                    .with_message("cannot have multiple floating points")
                                    .with_label(Label::primary(self.file_id, start..end)));

                                self.next();
                            }
                        } else {
                            let start = self.pos;
                            let end = self.pos + 1;

                            tokens.push(Token::new(
                                TokenKind::Error(LexerError::NonDecimalFloatingPoint), 
                                Span::new(start, end, self.file_id)));
                            
                            diagnostics.push(Diagnostic::error()
                                .with_message(format!("base {} floating point literals are not supported", base))
                                .with_labels(vec![
                                    Label::primary(self.file_id, self.pos..self.pos+1)
                                ]));
                                                    
                            self.next();
                        }
                    } else if base == 16 && (
                        c >= 'a' && c <= 'f' ||
                        c >= 'A' && c <= 'F'
                    ) {
                        let start = self.pos;
                        let end = self.pos + 1;

                        tokens.push(Token::new(
                            TokenKind::Error(LexerError::NonDecimalFloatingPoint),
                            Span::new(start, end, self.file_id)));

                        diagnostics.push(Diagnostic::error()
                            .with_message("base 16 floating point literals are not supported")
                            .with_labels(vec![
                                Label::primary(self.file_id, self.pos..self.pos+1)
                            ]));
                                                
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

    /// Lexes a string. Should be called when `self.ch` is at `"`.
    /// 
    /// The argument `start_pos` tells position of the first character of the string quotation.
    /// 
    /// Note: This function can emit multiple tokens.
    fn lex_string(&mut self, start_pos: usize, tokens: &mut Vec<Token>) -> Vec<Diagnostic<FileId>> {
        let mut segment_start = start_pos + 1;
        let mut set_segment_start = false;
        let num_hash = self.pos - start_pos;
        let mut diagnostics = Vec::<Diagnostic<FileId>>::new();

        macro_rules! push_segment {
            ( if $cond:expr ) => {
                if $cond {
                    tokens.push(Token::new(
                        TokenKind::StringSegment,
                        Span::new(segment_start, self.pos, self.file_id)));
                }
            };
        }

        macro_rules! push_unterminated_error {
            ( $start:expr, $end: expr ) => {
                let start = $start;
                let end = $end;

                tokens.push(Token::new(
                    TokenKind::Error(LexerError::UnterminatedString),
                    Span::new(start, end, self.file_id)));
                
                diagnostics.push(Diagnostic::error()
                    .with_message("unterminated string literal")
                    .with_label(Label::primary(self.file_id, start..end)));
            };
        }

        // Is currently at the `"`.

        'outer:
        loop {
            // Go to next character
            self.next();

            // Start new segment if needed
            if set_segment_start {
                segment_start = self.pos;
                set_segment_start = false;
            }

            match self.ch {
                Some('"') => {
                    let mut all_hashes = true; 
                    let end_start_pos = self.pos;

                    // If is the correct ending delimiter, push tokens and break
                    // Otherwise, all of these `next` calls work to advance the character pointer anyways
                    for _ in 0..num_hash {
                        self.next();
                        
                        match self.ch {
                            Some('#') => continue,

                            Some(_) => {
                                all_hashes = false;
                                break;
                            }

                            None => {
                                push_unterminated_error!(start_pos, self.pos);
                            }
                        }
                    }

                    if all_hashes {
                        self.next();

                        // Segment
                        push_segment!(if segment_start < end_start_pos);

                        // End
                        tokens.push(Token::new(
                            TokenKind::StringEnd,
                            Span::new(end_start_pos, self.pos, self.file_id)));
                    
                        break;
                    }
                }

                Some('\\') => {
                    let estart = self.pos;

                    // If some segment already, push that
                    push_segment!(if segment_start < estart);

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
                        Some('\n') => {
                            tokens.push(Token::new(
                                TokenKind::CharacterEsc,
                                Span::new(estart, self.pos + 1, self.file_id)));
                        },

                        // Treated the same as the `\n` escape sequence if it is followed by \n (darn Windows being special) 
                        Some('\r') => {
                            if let Some('\n') = self.peek() {
                                tokens.push(Token::new(
                                    TokenKind::CharacterEsc,
                                    Span::new(estart, self.pos + 2, self.file_id)));
                                
                                self.next();
                            } else {
                                let start = self.pos - 1;
                                let end = self.pos + 1;

                                tokens.push(Token::new(
                                    TokenKind::Error(LexerError::InvalidEscapeSequence),
                                    Span::new(start, end, self.file_id)));
                                
                                diagnostics.push(Diagnostic::error()
                                    .with_message(format!("invalid escape sequence: `\\r`"))
                                    .with_label(Label::primary(self.file_id, start..end)));

                                // Todo: Add a hint/note to say that the \r escape sequence is only supported if it is followed by a \n
                            }
                        }

                        // Unicode Escape Sequence (in decimal)
                        // TODO: Support 8-len unicode escapes
                        Some('u') | 
                        Some('U') => {
                            let ustart = self.pos;

                            match self.peek() {
                                Some(_) => {
                                    let seq_start = self.pos;
                                    let mut is_invalid = false;

                                    for _ in 0..4 {
                                        match self.peek() {
                                            Some('0'..='9') |
                                            Some('a'..='f') |
                                            Some('A'..='F') => (),

                                            Some('"') => {
                                                is_invalid = true;

                                                break;
                                            }

                                            Some(_) => is_invalid = true,

                                            None => {
                                                push_unterminated_error!(start_pos, self.pos);

                                                break 'outer;
                                            }
                                        }

                                        self.next();
                                    }

                                    if is_invalid {
                                        let start = estart;
                                        let end = self.pos + 1;

                                        tokens.push(Token::new(
                                            TokenKind::Error(LexerError::InvalidUnicodeEscapeSequence),
                                            Span::new(start, end, self.file_id)));

                                        diagnostics.push(Diagnostic::error()
                                            .with_message(format!("invalid unicode escape sequence: `{}`", &self.src[seq_start..=self.pos]))
                                            .with_label(Label::primary(self.file_id, start..end))
                                            .with_notes(vec![
                                                String::from("`u` escape sequences must precede a 4-digit hexadecimal value"),
                                                format!("expected 4 digits, found {}", self.pos - seq_start)
                                            ]));
                                    } else {
                                        tokens.push(Token::new(
                                            TokenKind::UnicodeEsc,
                                            Span::new(estart, self.pos + 1, self.file_id)));
                                    }
                                },
                                // TODO: Add checking if it is of form \u{} and then tell user to use an f string
                                // Some(c) => {
                                //     let start = ustart;
                                //     let end = self.pos + 1;

                                //     tokens.push(Token::new(
                                //         TokenKind::Error(LexerError::InvalidEscapeSequence),
                                //         Span::new(start, end, self.file_id)));
                                    
                                //     diagnostics.push(Diagnostic::error()
                                //         .with_message(format!("invalid unicode escape sequence: `{c}`"))
                                //         .with_label(Label::primary(self.file_id, ustart..end)));
                                // },
                                None => {
                                    push_unterminated_error!(start_pos, self.pos);

                                    break   
                                } 
                            }
                        },

                        // Ascii Escape Sequence (in hexadecimal)
                        Some('x') => {
                            let seq_start = self.pos;
                            let mut is_invalid = false;

                            for _ in 0..2 {
                                match self.peek() {
                                    Some('0'..='9') |
                                    Some('a'..='f') |
                                    Some('A'..='F') => (),
                                    
                                    Some('"') => {
                                        is_invalid = true;
                                        
                                        break
                                    },
                                    
                                    Some(_) => is_invalid = true,
                                    
                                    None => {
                                        self.next();
                                        push_unterminated_error!(start_pos, self.pos);

                                        break 'outer;
                                    }
                                }

                                self.next();
                            }

                            if is_invalid {
                                let start = estart;
                                let end = self.pos + 1;

                                tokens.push(Token::new(
                                    TokenKind::Error(LexerError::InvalidUnicodeEscapeSequence),
                                    Span::new(start, end, self.file_id)));

                                diagnostics.push(Diagnostic::error()
                                    .with_message(format!("invalid escape sequence: `{}`", &self.src[seq_start..=self.pos]))
                                    .with_label(Label::primary(self.file_id, start..end))
                                    .with_notes(vec![
                                        String::from("`x` escape sequences must precede a 2-digit hexadecimal value"),
                                        format!("expected 2 digits, found {}", self.pos - seq_start)
                                    ]));
                            } else {
                                tokens.push(Token::new(
                                    TokenKind::CharacterEsc,
                                    Span::new(estart, self.pos + 1, self.file_id)));
                            }
                        },

                        // Invalid
                        Some(c) => {
                            let start = self.pos-1;
                            let end = self.pos + 1;

                            tokens.push(Token::new(
                                TokenKind::Error(LexerError::InvalidEscapeSequence),
                                Span::new(start, end, self.file_id)));
                            
                            diagnostics.push(Diagnostic::error()
                                .with_message(format!("invalid escape sequence: `{c}`"))
                                .with_label(Label::primary(self.file_id, start..end)));
                        },

                        // EOF
                        None => todo!()
                    }

                    set_segment_start = true;
                }

                Some(_) => (),

                None => {
                    push_segment!(if segment_start < self.pos);

                    push_unterminated_error!(start_pos, self.pos);

                    break
                }
            }
        }

        diagnostics
    }

    // Delegates to the correct string lexing function based on the prefix, or outputs None if the prefix is invalid
    fn delegate_string(&mut self, prefix_start: usize, string_start_pos: usize, quote_pos: usize) -> Option<Vec<Diagnostic<FileId>>> {
        todo!()
    }

    /// Lexes an identifier, a keyword, an underscore operator (matching regex `_+`).
    fn lex_ident(&mut self, tokens: &mut Vec<Token>) -> Vec<Diagnostic<FileId>> {
        let start_pos = self.pos;
        let mut all_underscores = true;

        loop {
            match self.ch {
                Some(c) if c.is_alphabetic() => all_underscores = false,
                Some(c) if c.is_ascii_digit() => all_underscores = false,
                Some('\'') => all_underscores = false,
                Some('_') => all_underscores = all_underscores && true,

                Some(_) |
                None    => {
                    break;
                }
            }

            self.next();
        }


        tokens.push(Token::new(
            if let Some(&keyword_kind) = KEYWORDS.get(&self.src[start_pos..self.pos]) {
                keyword_kind
            } else if all_underscores {
                TokenKind::Underscore
            } else {
                TokenKind::Ident
            }, 
            Span::new(start_pos, self.pos, self.file_id)));

        Vec::new()
    }

    /// Lexes an operator or a doc comment and returns `Some(token)`, or skips a regular comment and returns `None`.
    fn lex_operator(&mut self, tokens: &mut Vec<Token>) -> Option<Vec<Diagnostic<FileId>>> {
        todo!()
    }

    fn skip_comment(&mut self, tokens: &mut Vec<Token>) {
        todo!()
    }

    /// Lexes one token.
    pub fn lex_token(&mut self, tokens: &mut Vec<Token>) -> Vec<Diagnostic<FileId>> {
        let mut diagnostics = Vec::new();
        
        self.skip_whitespace();

        // None if passed over a comment, and thus should repeat
        let res = match self.ch {
            // Number
            Some(c) if c.is_ascii_digit() => Some(diagnostics.append(&mut self.lex_number(tokens, 0))),
            
            // String
            Some('"') => {
                tokens.push(Token::new(
                    TokenKind::StringStart(StringKind::Normal),
                    Span::new(self.pos, self.pos + 1, self.file_id)));
                
                Some(diagnostics.append(&mut self.lex_string(self.pos, tokens)))
            }
            
            // Identifiers, Keywords, Or Prefixed Strings
            Some(c) if c.is_alphabetic() => Some(diagnostics.append(&mut self.lex_ident(tokens))),

            // Underscore operators (satisfies regex _+), or identifier prepended by underscores
            Some('_') => Some(diagnostics.append(&mut self.lex_ident(tokens))),
            
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
            Some('/')  => if let Some(ds) = &mut self.lex_operator(tokens) {
                Some(diagnostics.append(ds))
            } else {
                None
            }

            Some(_) => todo!(),

            None => {
                tokens.push(Token::new(
                    TokenKind::Eof, 
                    Span::new(self.pos, self.pos, self.file_id)
                ));

                Some(())
            }
        };

        println!("At End: {:?}", self.ch);
        while !self.at_end() {
            self.next();
            println!("And Then: {:?}", self.ch);
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
    // Numbers
    InvalidDigit,
    NonDecimalFloatingPoint,
    MultipleFloatingPoints,
    // Strings
    UnterminatedString,
    InvalidEscapeSequence,
    InvalidUnicodeEscapeSequence
}
