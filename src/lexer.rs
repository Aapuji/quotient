use std::iter::Peekable;
use std::str::CharIndices;

use crate::session::Session;
use crate::source::FileId;
use crate::token::Token;

/// Represents the Lexer.
#[derive(Debug)]
pub struct Lexer<'t> {
    src: &'t str,
    chars: Peekable<CharIndices<'t>>,
    pos: usize, // Holds flat index of next char, line and cols are calculated when needed
    file_id: FileId
}

impl<'t> Lexer<'t> {
    /// Creates a new `Lexer` for the main file.
    pub fn new(session: &'t mut Session) -> Self {        
        Self {
            src: session.main_src(),
            chars: session.main_src().char_indices().peekable(),
            pos: 0,
            file_id: *session.main_id()
        }
    }

    /// Given `file_id` must be valid.
    pub fn with_file_id(session: &'t mut Session, file_id: FileId) -> Self {
        let src = session.source_map().get(file_id).unwrap().source();
        Self {
            src: src,
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
    fn lex_number(&mut self, base: u32) -> Token {
        todo!()
    }

    /// Lexes a string, given the prefix (or `None` if it is a regular string).
    fn lex_string(&mut self, prefix: Option<&str>) -> Token {
        todo!()
    }

    /// Lexes an identifier or a keyword.
    fn lex_ident(&mut self) -> Token {
        todo!()
    }

    /// Lexes an operator or a doc comment and returns `Some(token)`, or skips a regular comment and returns `None`.
    fn lex_operator(&mut self) -> Option<Token> {
        todo!()
    }

    fn skip_comment(&mut self) {
        todo!()
    }

    /// Lexes one token.
    fn lex_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(tok) = match self.peek() {
            // Number
            Some(c) if c.is_ascii_digit() => Some(self.lex_number(0)),
            
            // String
            Some('"') => Some(self.lex_string(None)),
            
            // Identifiers, Keywords, Or Prefixed Strings
            Some('a'..='z') |
            Some('A'..='Z') |
            Some('_') => Some(self.lex_ident()),
            
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
            Some('/')  => self.lex_operator()
        } {
            tok
        } else {
            self.lex_token()
        }
    }
} 
