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

    fn consume(&mut self, kind: TokenKind) -> Option<Token> {
        if self.at(kind) {
            self.next()
        } else {
            todo!("expected {:?}", kind)
        }
    }

    fn at_end(&self) -> bool {
        self.current().is_none() || self.current().unwrap().kind() == &TokenKind::Eof
    }

    fn parse_top(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Option<TopLevel> {
        Some(TopLevel::Expr(self.parse_expr(diagnostics)))
    }

    fn parse_expr(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {
        let mut expr = self.parse_no_chain_expr(diagnostics);

        while self.at(TokenKind::Semicolon) {
            self.next();

            let rhs = self.parse_expr(diagnostics);
            expr = Expr::Chain(Box::new(expr), Box::new(rhs));
        }

        expr
    }

    fn parse_no_chain_expr(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {
        let expr = self.parse_operators(diagnostics);

        expr
    }

    fn parse_operators(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {        
        let mut operator_list = Vec::with_capacity(2);
        
        while !self.at_end() {
            if let Some((t, TokenKind::Operator)) = self.current_tk() {
                operator_list.push(OpListItem::Operation(t));

                self.next();
            } else if operator_list.is_empty() {
                let expr = if let Ok(x) = self.parse_primary(diagnostics) {
                    x
                } else {
                    panic!("[uh oh ope] expected primary expression")
                };

                if let Some(TokenKind::Operator) = self.current_kind() {
                    operator_list.push(OpListItem::Expr(Box::new(expr)));
                    continue
                } else {
                    return expr;
                }
            } else {
                if let Ok(x) = self.parse_primary(diagnostics) {
                    operator_list.push(OpListItem::Expr(Box::new(x)));
                } else {
                    break
                }

                if !self.at_end() {
                    if let Some(TokenKind::Operator) = self.current_kind() {
                        continue
                    } else {
                        if let Ok(x) = self.parse_primary(diagnostics) {
                            operator_list.push(OpListItem::Expr(Box::new(x)));
                        } else {
                            break
                        }
                    }
                }
            }
        }
        
        Expr::OperationList(operator_list)
    }

    /// Attempts to parse a primary expression. If it fails, it outputs an error. This diesn't mean it is an invalid expression, only that it is an invalid _primary_ expression.
    fn parse_primary(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Result<Expr, ()> {
        Ok(match self.current_tk() {
            Some((_, TokenKind::LParen)) => {
                self.next();

                if let Some(TokenKind::RParen) = self.current_kind() {
                    return Ok(Expr::Literal(Literal::Unit));
                }

                let expr = self.parse_expr(diagnostics);

                match self.current_kind() {
                    // Grouping
                    Some(TokenKind::RParen) => {
                        self.next();

                        expr
                    }

                    // Tuple
                    Some(TokenKind::Comma) => {
                        self.next();
                        
                        let mut elems = vec![expr, self.parse_expr(diagnostics)];

                        while let Some(TokenKind::Comma) = self.current_kind() {
                            self.next();

                            elems.push(self.parse_expr(diagnostics));
                        }

                        self.consume(TokenKind::RParen);

                        Expr::Tuple(elems)
                    }

                    _ => todo!("report -- expected ')' found {:?}", self.current())
                }
            }

            Some((_, TokenKind::Do)) => {
                self.next();

                let expr = self.parse_expr(diagnostics);

                self.consume(TokenKind::End);

                Expr::Block(Box::new(expr))
            }

            // Literals
            Some((t, TokenKind::Int)) => {
                self.next();

                Expr::Literal(Literal::Int(t
                    .span()
                    .resolve_content(self.src)
                    .unwrap() // perhaps add checking here, though there shouldn't be problem as tokens should be from same file
                    .parse()
                    .unwrap()))
            }

            Some((t, TokenKind::Real)) => {
                self.next();

                Expr::Literal(Literal::Real(t
                    .span()
                    .resolve_content(self.src)
                    .unwrap()
                    .parse()
                    .unwrap()))
            }

            Some((t, TokenKind::Imaginary)) => {
                self.next();

                let mut s = t
                    .span()
                    .resolve_content(self.src)
                    .unwrap()
                    .chars();

                s.next_back();

                Expr::Literal(Literal::Imaginary(s
                    .as_str()
                    .parse()
                    .unwrap()))
            }

            Some((_, TokenKind::True)) => {
                self.next();

                Expr::Literal(Literal::Bool(true))
            }

            Some((_, TokenKind::False)) => {
                self.next();
                
                Expr::Literal(Literal::Bool(false))
            }

            Some((_, TokenKind::Unit)) => {
                self.next();
                
                Expr::Literal(Literal::Unit)
            }

            Some((t, TokenKind::LBracket)) => {
                self.next();

                todo!("vector & matrix")
            }

            _ => Err(())?,
        })
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
