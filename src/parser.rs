use codespan_reporting::diagnostic::Diagnostic;

use crate::{ast::{Expr, Literal, OpListItem, TopLevel}, source::FileId, token::{Token, TokenKind}};

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

    fn current_kind(&self) -> Option<TokenKind> {
        self.current().map(|t| *t.kind())
    }

    fn current_tk(&self) -> Option<(Token, TokenKind)> {
        self.current().map(|t| (t, *t.kind()))
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

    fn at(&self, kind: TokenKind) -> bool {
        self.current()
            .map(|t| t.kind() == &kind)
            .unwrap_or(false)
    }

    fn at_end(&self) -> bool {
        self.current().is_none() || self.current().unwrap().kind() == &TokenKind::Eof
    }

    fn parse_top(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Option<TopLevel> {
        Some(TopLevel::Expr(self.parse_expr(diagnostics)))
    }

    fn parse_expr(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {
        let mut expr = self.parse_no_chain_expr(vec![], diagnostics);

        while self.at(TokenKind::Semicolon) {
            self.next();

            let rhs = self.parse_expr(diagnostics);
            expr = Expr::Chain(Box::new(expr), Box::new(rhs));
        }

        expr
    }

    fn parse_no_chain_expr(&mut self, mut operator_list: Vec<OpListItem>, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {
        let expr = match self.current_tk() {
            Some((_, TokenKind::LParen)) => Some(self.parse_grouping(diagnostics)),
            Some((_, TokenKind::Do)) => Some(self.parse_do_expr(diagnostics)),

            // Literals
            Some((t, TokenKind::Int)) => {
                self.next();

                Some(Expr::Literal(Literal::Int(t
                    .span()
                    .resolve_content(self.src)
                    .unwrap() // perhaps add checking here, though there shouldn't be problem as tokens should be from same file
                    .parse()
                    .unwrap())))
            }

            Some((t, TokenKind::Real)) => {
                self.next();

                Some(Expr::Literal(Literal::Real(t
                    .span()
                    .resolve_content(self.src)
                    .unwrap()
                    .parse()
                    .unwrap())))
            }

            Some((t, TokenKind::Imaginary)) => {
                self.next();

                let mut s = t
                    .span()
                    .resolve_content(self.src)
                    .unwrap()
                    .chars();

                s.next_back();

                Some(Expr::Literal(Literal::Imaginary(s
                    .as_str()
                    .parse()
                    .unwrap())))
            }

            Some((t, TokenKind::True)) => {
                self.next();

                Some(Expr::Literal(Literal::Bool(true)))
            }

            Some((t, TokenKind::False)) => {
                self.next();
                
                Some(Expr::Literal(Literal::Bool(false)))
            }

            Some((t, TokenKind::Unit)) => {
                self.next();
                
                Some(Expr::Literal(Literal::Unit))
            }

            Some((t, TokenKind::Operator)) => None,

            Some(_) => panic!("uh oh nocha {:?}", self.current()),

            None => todo!("expected expression")
            // } Expr::Literal(self.parse_literal(diagnostics))
        };

        if operator_list.is_empty() {
            if let Some(x) = expr {
                if let Some(TokenKind::Operator) = self.current_kind() {
                    operator_list.push(OpListItem::Expr(Box::new(x)));

                    self.parse_operators(operator_list, diagnostics)
                } else {
                    x
                }
            } else {
                self.parse_operators(operator_list, diagnostics)
            }
        } else {
            if let Some(x) = expr {
                operator_list.push(OpListItem::Expr(Box::new(x)));
                if let Some(TokenKind::Operator) = self.current_kind() {
                    self.parse_operators(operator_list, diagnostics)
                } else {
                    Expr::OperationList(operator_list)
                }
            } else {
                self.parse_operators(operator_list, diagnostics)
            }
        }


        // if let Some(x) = expr {
        //     operator_list.push(OpListItem::Expr(Box::new(x)));

        //     if let Some(TokenKind::Operator) = self.current_kind() {
        //         self.parse_operators(operator_list, diagnostics)
        //     } else {
        //         Expr::OperationList(operator_list)
        //     }
        // } else if let Some(TokenKind::Operator) = self.current_kind() {
        //     self.parse_operators(operator_list, diagnostics)
        // } else {
        //     expr.expect("expected expression [expect]")
        // }
    }

    fn parse_grouping(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {
        todo!()
    }

    fn parse_do_expr(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {
        todo!()
    }

    /// Should be called when current's kind is `TokenKind::Operator`
    fn parse_operators(&mut self, mut operator_list: Vec<OpListItem>, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {        
        if let Some((t, TokenKind::Operator)) = self.current_tk() {
            operator_list.push(OpListItem::Operation(t));

            self.next();

            if !self.at_end() {
                self.parse_no_chain_expr(operator_list, diagnostics)
            } else {
                Expr::OperationList(operator_list)
            }
        } else {
            panic!("invalid call");
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
