use std::collections::HashMap;
use std::iter::Peekable;
use std::str::CharIndices;
use std::u8;

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
    file_id: FileId,
    comment_depth: u32
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
            file_id,
            comment_depth: 0
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
                break
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
    /// The argument `start_pos` is the position of the first character of the string start, including the prefix. If there is a prefix, it is the position of the first character of the prefix. Otherwise it is the first character of the string start.
    /// 
    /// Note: This function can emit multiple tokens.
    fn lex_string(&mut self, tokens: &mut Vec<Token>, start_pos: usize, num_hash: u8, prefix: StringKind) -> Vec<Diagnostic<FileId>> {
        println!("NUM #: {num_hash}");
        let mut segment_start = self.pos + 1;
        let mut set_segment_start = false;
        let mut last_segment_end = self.pos + 1;
        let mut diagnostics = Vec::<Diagnostic<FileId>>::new();

        macro_rules! push_segment {
            ( if $cond:expr ) => {
                if $cond {
                    tokens.push(Token::new(
                        TokenKind::StringSegment,
                        Span::new(start_pos, self.pos, self.file_id)));
                }
            }
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
                    last_segment_end = self.pos;

                    // If is the correct ending delimiter, push tokens and break
                    // Otherwise, break
                    // Cannot continue for the remaining because if the next character is a valid end delimiter, it won't be found
                    for _ in 0..num_hash {                        
                        match self.peek() {
                            Some('#') => (),

                            Some(_) => {
                                all_hashes = false;
                                break
                            }

                            None => {
                                push_unterminated_error!(start_pos, self.pos + 1);
                                break
                            }
                        }

                        self.next();
                    }

                    if all_hashes {
                        // Previous Segment
                        if segment_start < last_segment_end {
                            tokens.push(Token::new(
                                TokenKind::StringSegment,
                                Span::new(segment_start, last_segment_end, self.file_id)));
                        }

                        self.next();

                        // End
                        tokens.push(Token::new(
                            TokenKind::StringEnd,
                            Span::new(last_segment_end, self.pos, self.file_id)));
                    
                        break;
                    }
                }

                Some('\\') => {
                    let estart = self.pos;

                    // Previous Segment
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
                    push_unterminated_error!(start_pos, self.pos);

                    break
                }
            }
        }

        diagnostics
    }

    /// Lexes an identifier, a keyword, an underscore operator (matching regex `_+`), or i (imaginary number).
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
            } else if self.pos - 1 == start_pos && &self.src[0..=0] == "i" {
                TokenKind::Imaginary
            } else {
                TokenKind::Ident
            }, 
            Span::new(start_pos, self.pos, self.file_id)));

        Vec::new()
    }

    fn is_directly_after_ident(&self, tokens: &Vec<Token>) -> bool {
        self.is_pos_directly_after_ident(tokens, self.pos)
    }

    fn is_pos_directly_after_ident(&self, tokens: &Vec<Token>, pos: usize) -> bool {
        tokens
            .last()
            .map_or_else(|| false, |t| t.kind() == &TokenKind::Ident && t.span().end() == pos)
    }

    /// Validates and records a string prefix, and pushes the correct string start token. Then it calls `lex_string`.
    /// 
    /// Outputs `Some` containing a `Vec` of `Diagnotic`s generated, or `None` if the last token in `tokens` doesn't hold a valid prefix.
    fn lex_string_with_prefix(&mut self, tokens: &mut Vec<Token>, num_hash: u8) -> Option<Vec<Diagnostic<FileId>>> {
        let mut diagnostics = Vec::new();
        let mut had_invalid_prefix = false;
        
        // Checking if there even is a prefix
        let Some(last) = tokens.last() else { 
            return None
        };

        let kind = *last.kind();
        let span = *last.span();

        if kind != TokenKind::Ident {
            return None;
        }

        // Checking if all prefix characters are valid
        let mut prefix_map: HashMap<char, Vec<usize>> = HashMap::new();
        prefix_map.insert('b', vec![]);
        prefix_map.insert('c', vec![]);
        prefix_map.insert('f', vec![]);
        prefix_map.insert('F', vec![]);
        prefix_map.insert('r', vec![]);
        prefix_map.insert('t', vec![]);

        for (i, c) in self.src[span.range()].char_indices() {
            if let Some(v) = prefix_map.get_mut(&c) {
                v.push(i);
            } else {
                return None;
            }
        }

        // Invalid Prefix Combinations
        
        // rfF
        // Incompatible combination, so treat it as if there was no prefix
        if !prefix_map.get(&'r').unwrap().is_empty() && 
            !prefix_map.get(&'f').unwrap().is_empty() && 
            !prefix_map.get(&'F').unwrap().is_empty() 
        {
            let start = span.start();
            let end = span.end();

            tokens.push(Token::new(
                TokenKind::Error(LexerError::IncompatibleStringPrefixes),
                Span::new(start, end, self.file_id)));
            
            diagnostics.push(Diagnostic::error()
                .with_message(format!("incompatible string prefixes: `r`, `f`, `F`"))
                .with_label(Label::primary(self.file_id, start..end))
                .with_notes(vec![
                    String::from("the `r` string prefix is incompatible with the `f` or `F` prefixes"),
                    String::from("the `f` string prefix is incompatible with the `F` prefix")
                ]));
            
            had_invalid_prefix = true;
        // rf
        // Incompatible combination, so treat it as if there was no prefix
        } else if !prefix_map.get(&'r').unwrap().is_empty() && 
            !prefix_map.get(&'f').unwrap().is_empty() 
        {
            let start = span.start();
            let end = span.end();

            tokens.push(Token::new(
                TokenKind::Error(LexerError::IncompatibleStringPrefixes),
                Span::new(start, end, self.file_id)));

            diagnostics.push(Diagnostic::error()
                .with_message(format!("incompatible string prefixes: `r`, `f`"))
                .with_label(Label::primary(self.file_id, start..end))
                .with_note("the `r` string prefix is incompatible with the `f` prefix"));

            had_invalid_prefix = true;
        // rF
        // Incompatible combination, so treat it as if there was no prefix
        } else if !prefix_map.get(&'r').unwrap().is_empty() &&
            !prefix_map.get(&'F').unwrap().is_empty()
        {
            let start = span.start();
            let end = span.end();

            tokens.push(Token::new(
                TokenKind::Error(LexerError::IncompatibleStringPrefixes),
                Span::new(start, end, self.file_id)));

            diagnostics.push(Diagnostic::error()
                .with_message(format!("incompatible string prefixes: `r`, `F`"))
                .with_label(Label::primary(self.file_id, start..end))
                .with_note("the `r` string prefix is incompatible with the `F` prefix"));

            had_invalid_prefix = true;
        // fF
        // Incompatible combination, so treat it as if there was no prefix
        } else if !prefix_map.get(&'f').unwrap().is_empty() && 
            !prefix_map.get(&'F').unwrap().is_empty()
        {
            let start = span.start();
            let end = span.end();

            tokens.push(Token::new(
                TokenKind::Error(LexerError::IncompatibleStringPrefixes),
                Span::new(start, end, self.file_id)));

            diagnostics.push(Diagnostic::error()
                .with_message(format!("incompatible string prefixes: `f`, `F`"))
                .with_label(Label::primary(self.file_id, start..end))
                .with_note("the `f` string prefix is incompatible with the `F` prefix"));

            had_invalid_prefix = true;
        }

        // If no invalid prefix, then remove the last Ident token from tokens as it is merged with the StringStart
        if !had_invalid_prefix {
            println!("BEFORE: {}", tokens.len());
            tokens.pop();
            println!("AFER: {}", tokens.len());
        }

        // Repeats
        // Though they may be invalid, it can still continue normally, as repeated prefixes are invalid but don't corrupt the meaning
        for key in prefix_map.keys() {
            if prefix_map.get(key).unwrap().len() > 1 {
                let start = span.start();

                tokens.push(Token::new(
                    TokenKind::Error(LexerError::RepeatedStringPrefixes),
                    Span::new(start, span.end(), self.file_id)));

                diagnostics.push(Diagnostic::error()
                    .with_message("repeated prefixes are not allowed")
                    .with_labels(prefix_map
                        .get(key)
                        .unwrap()
                        .iter()
                        .map(|i| Label::primary(self.file_id, start + i..start + i))
                        .collect()
                    ));
            }
        }

        if had_invalid_prefix {
            println!("IN PRE: {num_hash}");
            tokens.push(Token::new(
                TokenKind::StringStart(StringKind::Normal, num_hash),
                Span::new(span.start(), self.pos, self.file_id)));
        } else {
            let str_kind = {
                let mut kind = StringKind::Normal; // 0

                let has_prefix = |prefix: char| -> bool {
                    match prefix_map.get(&prefix) {
                        Some(v) => !v.is_empty(),
                        None => false
                    }
                };

                if has_prefix('b') {
                    kind |= StringKind::Byte;
                }
                
                if has_prefix('c') {
                    kind |= StringKind::Char;
                }
                
                if has_prefix('f') {
                    kind |= StringKind::Format;
                }
                
                if has_prefix('F') {
                    kind |= StringKind::BigFormat;
                }

                if has_prefix('r') {
                    kind |= StringKind::Raw;
                }

                if has_prefix('t') {
                    kind |= StringKind::Trim;
                }

                kind
            };

            tokens.push(Token::new(
                TokenKind::StringStart(str_kind, num_hash), 
                Span::new(span.start(), self.pos, self.file_id)));
            
            diagnostics.append(&mut self.lex_string(tokens, span.start(), num_hash, str_kind));
        }

        Some(diagnostics)
    }

    /// Lexes an operator, a doc comment, a regular comment, a directive, or initiates a string (if it is all `#`s and is immediately followed by `"`).
    /// 
    /// Should be called when the current character satisfies `[!@$#^&|*-+=<>.:'?/]` ~%!@$#^&|*-+=<>.:'?/
    fn lex_operator(&mut self, tokens: &mut Vec<Token>) -> Vec<Diagnostic<FileId>> {
        let start_pos = self.pos;
        let mut all_hashes = true;
        let mut diagnostics = Vec::new();

        loop {
            if let Some('#') = self.ch {
                all_hashes = all_hashes && true;
            } else {
                all_hashes = false;
            }

            self.next();

            match self.ch {
                Some('~')  |
                Some('%')  |
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
                Some('/')  => (),

                Some(_) |
                None    => break,
            }
        }

        let len = (self.pos - start_pos) as u8;

        if all_hashes {
            match self.ch {
                Some('"') => if self.is_pos_directly_after_ident(tokens, start_pos) {
                    match self.lex_string_with_prefix(tokens, len) {
                        Some(mut ds) => {
                            diagnostics.append(&mut ds)
                        },
                        None => {
                            tokens.push(Token::new(
                                TokenKind::StringStart(StringKind::Normal, len), 
                                Span::new(start_pos, self.pos, self.file_id)));

                            diagnostics.append(&mut self.lex_string(tokens, start_pos, len, StringKind::Normal));
                        }
                    }
                } else {
                    tokens.push(Token::new(
                        TokenKind::StringStart(StringKind::Normal, len), 
                        Span::new(start_pos, self.pos, self.file_id)));

                    diagnostics.append(&mut self.lex_string(tokens, start_pos, len, StringKind::Normal));
                }
                
                // ---//if self.is_directly_after_ident(tokens) {
                //     match self.lex_string_with_prefix(tokens, len) {
                //         Some(mut ds) => diagnostics.append(&mut ds),
                //         None => ()
                //     }
                // } else {
                //     tokens.push(Token::new(
                //         TokenKind::StringStart(StringKind::Normal, len),
                //         Span::new(start_pos, self.pos + 1, self.file_id)));
                    
                //     diagnostics.append(&mut self.lex_string(tokens, start_pos, len, StringKind::Normal))
                // }

                Some('(') if len == 1 => todo!("#( ... )#"),

                _ if len == 1 => todo!("# ..."),

                Some('(') if len == 2 => todo!("##( ... )#"),
                
                _ if len == 1 => todo!("## ..."),
                
                _ => todo!("####...####") 
            }
        } else {
            tokens.push(Token::new(
                TokenKind::Operator,
                Span::new(start_pos, self.pos, self.file_id)));
        }

        diagnostics
    }

    /// Lexes one token.
    pub fn lex_token(&mut self, tokens: &mut Vec<Token>) -> Vec<Diagnostic<FileId>> {
        let mut diagnostics = Vec::new();
        
        self.skip_whitespace();

        // None if passed over a comment, and thus should repeat
        match self.ch {
            // Number
            Some(c) if c.is_ascii_digit() => diagnostics.append(&mut self.lex_number(tokens, 0)),
            
            // String
            Some('"') => {
                let num_hash = 0;

                if self.is_directly_after_ident(tokens) {
                    match self.lex_string_with_prefix(tokens, num_hash) {
                        Some(mut ds) => {
                            diagnostics.append(&mut ds)
                        },
                        None => {
                            tokens.push(Token::new(
                                TokenKind::StringStart(StringKind::Normal, num_hash), 
                                Span::new(self.pos, self.pos + 1, self.file_id)));

                            diagnostics.append(&mut self.lex_string(tokens, self.pos, num_hash, StringKind::Normal));
                        }
                    }
                } else {
                    tokens.push(Token::new(
                        TokenKind::StringStart(StringKind::Normal, num_hash), 
                        Span::new(self.pos, self.pos + 1, self.file_id)));

                    diagnostics.append(&mut self.lex_string(tokens, self.pos, num_hash, StringKind::Normal));
                }
            }
            
            // Identifiers, Keywords, Or Prefixed Strings
            Some(c) if c.is_alphabetic() => diagnostics.append(&mut self.lex_ident(tokens)),

            // Underscore operators (satisfies regex _+), or identifier prepended by underscores
            Some('_') => diagnostics.append(&mut self.lex_ident(tokens)),
            
            // Operator Symbols (and Comments)
            // Operators satisfy the following regex: [!@$#^&|*-+=<>.:'?/][~%!@$#^&|*-+=<>.:'?/]*
            // Valid comment signifiers are: #, #(, ##, ##(, ##<, ##^, ##<(, ##^(
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

            Some('~') => tokens.push(Token::new(
                TokenKind::Tilde,
                Span::new(self.pos, self.pos + 1, self.file_id))),

            Some('(') => tokens.push(Token::new(
                TokenKind::LParen, 
                Span::new(self.pos, self.pos + 1, self.file_id))),
            
            Some(')') => tokens.push(Token::new(
                TokenKind::RParen, 
                Span::new(self.pos, self.pos + 1, self.file_id))),
            
            Some('[') => tokens.push(Token::new(
                TokenKind::LBracket, 
                Span::new(self.pos, self.pos + 1, self.file_id))),
            
            Some(']') => tokens.push(Token::new(
                TokenKind::RBracket, 
                Span::new(self.pos, self.pos + 1, self.file_id))),
            
            Some('{') => tokens.push(Token::new(
                TokenKind::LBrace, 
                Span::new(self.pos, self.pos + 1, self.file_id))),
            
            Some('}') => tokens.push(Token::new(
                TokenKind::RBrace, 
                Span::new(self.pos, self.pos + 1, self.file_id))),
            
            Some(',') => tokens.push(Token::new(
                TokenKind::Comma, 
                Span::new(self.pos, self.pos + 1, self.file_id))),
            
            Some(';') => tokens.push(Token::new(
                TokenKind::Semicolon, 
                Span::new(self.pos, self.pos + 1, self.file_id))),

            Some(c) => println!("{c}:TODO!"),

            None => tokens.push(Token::new(
                TokenKind::Eof, 
                Span::new(self.pos, self.pos, self.file_id)
            )),
        }

        diagnostics
    }

    pub fn lex(&mut self) -> (Vec<Token>, Vec<Diagnostic<FileId>>) {
        let mut tokens = Vec::new();
        let mut diagnostics = Vec::new();

        while let Some(_) = self.ch {
            diagnostics.append(&mut self.lex_token(&mut tokens));
        }

        (tokens, diagnostics)
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
    InvalidUnicodeEscapeSequence,
    IncompatibleStringPrefixes,
    RepeatedStringPrefixes,
    TooManyHashes
}
