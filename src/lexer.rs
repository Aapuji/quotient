use std::iter::Peekable;
use std::str::CharIndices;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::session::Session;
use crate::source::{FileId, Span};
use crate::token::{self, Token, TokenKind};

/// Represents the Lexer.
#[derive(Debug)]
pub struct Lexer<'t> {
    //src: &'t str,
    chars: Peekable<CharIndices<'t>>,
    pos: usize, // Holds flat index of next char, line and cols are calculated when needed
    file_id: FileId
}

impl<'t> Lexer<'t> {
    /// Creates a new `Lexer` for the main file.
    pub fn new(session: &'t mut Session) -> Self {        
        Self {
           // src: session.main_src(),
            chars: session.main_src().char_indices().peekable(),
            pos: 0,
            file_id: *session.main_id()
        }
    }

    /// Given `file_id` must be valid.
    pub fn with_file_id(session: &'t mut Session, file_id: FileId) -> Self {
        let src = session.source_map().get(file_id).unwrap().source();
        Self {
           // src: src,
            chars: src.char_indices().peekable(),
            pos: 0,
            file_id
        }
    }

    /// Checks the current character is the last one.
    fn at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    /// Advances iterator to next character and byte index.
    fn next(&mut self) -> Option<(usize, char)> {
        if let Some((i, ch)) = self.chars.next() {
            self.pos = i + ch.len_utf8();
            Some((i, ch))
        } else {
            None
        }
    }

    /// Just peeks at the next character.
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, ch)| ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    /// Lexes a number, given the base. If `base` is 0, then it will figure out the base.
    fn lex_number(&mut self, tokens: &mut Vec<Token>, mut base: u8) -> Vec<Diagnostic<FileId>> {
        let start_pos = self.pos;
        let mut i: usize;
        let mut ch: char;
        let mut num_kind = TokenKind::Int;
        let mut diagnostics = Vec::new();

        macro_rules! lex_base {
            ($n:expr) => {
                {
                    (i, ch) = self.next().unwrap();
                    base = $n;
                }
            };
        }

        // If base=0, check to see if the number starts with 0_
        if base == 0 {
            if let Some('0') = self.peek() {
                (i, ch) = self.next().unwrap();

                match self.peek() {
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
                    
                    Some(c) if c.is_ascii_digit() => {
                        base = 10;
                    }

                    Some('_') => {
                        base = 10;
                    }

                    // Don't deal with the other stuff; 
                    // This section is only for dealing with the base
                    Some(_) |
                    None    => ()
                }
            } else {
                base = 10;
            }
        }

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '_' {
                if c >= '0' && c < ('0' as u8 + base) as char {
                    (i, ch) = self.next().unwrap();
                } else {
                    let diagnostic = Diagnostic::error()
                        .with_message(format!("invalid digit for base {} literal", base))
                        .with_labels(vec![
                            Label::primary(self.file_id, self.pos..self.pos+1)
                        ]);
                    
                    diagnostics.push(diagnostic);
                    break;
                }
            } else if c.is_ascii_alphabetic() && base == 16 {
                // Base 16 digits
                if c >= 'a' && c <= 'f' ||
                   c >= 'A' && c <= 'F' {
                    (i, ch) = self.next().unwrap();
                // i for imaginary numbers
                } else if c == 'i' {
                    (i, ch) = self.next().unwrap();
                    num_kind = TokenKind::Imaginary;
                    break;
                // Anything else
                } else {
                    break;
                }
            // Anything else
            } else {
                break;
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

    /// Lexes a string, given the prefix (or `None` if it is a regular string).
    fn lex_string(&mut self, tokens: &mut Vec<Token>, prefix: Option<&str>) -> Vec<Diagnostic<FileId>> {
        todo!()
    }

    /// Lexes an identifier or a keyword.
    fn lex_ident(&mut self, tokens: &mut Vec<Token>) -> Vec<Diagnostic<FileId>> {
        todo!()
    }

    /// Chooses either the underscore operators, or `_<ident>`.
    fn lex_underscore(&mut self, tokens: &mut Vec<Token>) -> Vec<Diagnostic<FileId>> {
        todo!()
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

        // None if passed over a comment, and thus should repeat.
        let res = match self.peek() {
            // Number
            Some(c) if c.is_ascii_digit() => Some(diagnostics.append(&mut self.lex_number(tokens, 0))),
            
            // String
            Some('"') => Some(diagnostics.append(&mut self.lex_string(tokens, None))),
            
            // Identifiers, Keywords, Or Prefixed Strings
            Some(c) if c.is_alphabetic() => Some(diagnostics.append(&mut self.lex_ident(tokens))),

            // Underscore operators (satisfies regex _+), or identifier prepended by underscores
            Some('_') => Some(diagnostics.append(&mut self.lex_underscore(tokens))),
            
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

        // diagnostics

        if let Some(_) = res {
            diagnostics
        } else {
            self.lex_token(tokens)
        }
    }
} 
