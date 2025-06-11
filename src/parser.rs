use codespan_reporting::diagnostic::Diagnostic;

use crate::{ast::{Expr, Literal, TopLevel}, source::FileId, token::{Token, TokenKind}};

#[derive(Debug, Clone)]
pub struct Parser<'s, 't> {
    src: &'s str,
    file_id: FileId,
    tokens: &'t [Token],
    ptr: usize
}

impl<'s, 't> Parser<'s, 't> {
    /// Given the slice of tokens, which should be non-empty.
    pub fn new(src: &'s str, file_id: FileId, tokens: &'t [Token]) -> Self {
        Self {
            src,
            file_id,
            tokens,
            ptr: 0
        }
    }

    fn current(&self) -> Option<Token> {
        self.tokens.get(self.ptr).copied()
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.ptr + 1).copied()
    }

    fn next(&mut self) -> Option<Token> {
        let mut token = None;
        
        while self.ptr < self.tokens.len() {
            self.ptr += 1;

            token = self.tokens.get(self.ptr).copied();
            if token.is_some() && token.unwrap().kind() == &TokenKind::Comment {
                ()
            } else {
                break
            }
        }

        token
    }

    fn at_end(&self) -> bool {
        self.current().is_none() || self.current().unwrap().kind() == &TokenKind::Eof
    }

    fn parse_top(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Option<TopLevel> {
        Some(TopLevel::Expr(self.parse_expr(diagnostics)))
    }

    fn parse_expr(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {
        Expr::Literal(self.parse_literal(diagnostics))
    }

    fn parse_literal(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Literal {
        match self.current() {
            Some(t) if t.kind() == &TokenKind::Int => {
                self.next();

                Literal::Int(t
                    .span()
                    .resolve_content(self.src)
                    .unwrap() // perhaps add checking here, though there shouldn't be problem as tokens should be from same file
                    .parse()
                    .unwrap())
            } // perhaps add checking here as well, but it shouldn't be needed

            Some(t) if t.kind() == &TokenKind::Real => {
                self.next();

                Literal::Real(t
                    .span()
                    .resolve_content(self.src)
                    .unwrap()
                    .parse()
                    .unwrap())
            }

            Some(t) if t.kind() == &TokenKind::Imaginary => {
                self.next();

                let mut s = t
                    .span()
                    .resolve_content(self.src)
                    .unwrap()
                    .chars();

                s.next_back();

                Literal::Imaginary(s
                    .as_str()
                    .parse()
                    .unwrap())
            }

            Some(t) if t.kind() == &TokenKind::True => {
                self.next();

                Literal::Bool(true)
            }

            Some(t) if t.kind() == &TokenKind::False => {
                self.next();
                
                Literal::Bool(false)
            }

            Some(t) if t.kind() == &TokenKind::Unit => {
                self.next();
                
                Literal::Unit
            }

            _ => todo!("here")
        }
    }

    fn recover(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> () {
        todo!()
    }

    pub fn parse(&mut self) -> (Vec<TopLevel>, Vec<Diagnostic<FileId>>) {
        let mut diagnostics = Vec::new();
        let mut top_items = Vec::new();

        while !self.at_end() {
            if let Some(item) = self.parse_top(&mut diagnostics) {
                top_items.push(item);
            } else {
                self.recover(&mut diagnostics);
            }
        }

        (top_items, diagnostics)
    }
}
