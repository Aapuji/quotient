use codespan_reporting::diagnostic::Diagnostic;

use crate::{ast::{Expr, GenericSymbol, Let, Literal, OpListItem, Pattern, TopLevel}, num::Complex, source::FileId, token::{Token, TokenKind}};

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
        self.current().map(|t| t.kind())
    }

    fn current_tk(&self) -> Option<(Token, TokenKind)> {
        self.current().map(|t| (t, t.kind()))
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.ptr + 1).copied()
    }

    fn next(&mut self) -> Option<Token> {
        let mut token = None;
        
        while self.ptr < self.tokens.len() {
            self.ptr += 1;

            token = self.tokens.get(self.ptr).copied();
            if token.is_some() && token.unwrap().kind() == TokenKind::Comment {
                ()
            } else {
                break
            }
        }

        token
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current()
            .map(|t| t.kind() == kind)
            .unwrap_or(false)
    }

    /// Check if at an operator.
    /// 
    /// NOTE: Cannot be used for colons, semicolons, commas, tildes, parens, brackets, and braces.
    fn at_op(&self, op: &str) -> bool {
        self.at(TokenKind::Operator) && op == self.current()
                .unwrap()
                .span()
                .resolve_content(self.src)
                .unwrap()
    }

    fn at_end(&self) -> bool {
        self.current().is_none() || self.current().unwrap().kind() == TokenKind::Eof
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        self.peek()
            .map(|t| t.kind())
            .map_or_else(|| false, |k| k == kind)
    }

    fn next_is_op(&self, op: &str) -> bool {
        self.next_is(TokenKind::Operator) && op == self.peek()
            .unwrap()
            .span()
            .resolve_content(self.src)
            .unwrap()
    }

    fn consume(&mut self, kind: TokenKind) -> Option<Token> {
        if self.at(kind) {
            self.next()
        } else {
            todo!("report -- expected {:?} found {:?}", kind, self.current_kind())
        }
    }

    fn consume_op(&mut self, op: &str) -> Option<Token> {
        if self.at_op(op) {
            self.next()
        } else {
            todo!("report -- expected {:?} found {:?}", op, self.current())
        }
    }

    fn parse_top(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Option<TopLevel> {
        Some(TopLevel::Expr(self.parse_expr(diagnostics)))
    }

    fn parse_let(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Let {
        self.consume(TokenKind::Let);

        let pat = self.parse_pat(diagnostics);

        self.consume_op("=");

        let expr = self.parse_expr(diagnostics);

        Let {
            lhs: Box::new(pat),
            rhs: Box::new(expr)
        }
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
        match self.current_kind() {
            Some(TokenKind::Let) => Expr::Let(self.parse_let(diagnostics)),
            _ => self.parse_operators(diagnostics)
        }
    }

    fn parse_operators(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Expr {        
        let mut operator_list = Vec::with_capacity(2);
        
        while !self.at_end() {
            if let Some((t, TokenKind::Operator)) = self.current_tk() {
                operator_list.push(OpListItem::Operation(t));

                self.next();
            } else if operator_list.is_empty() {
                let expr = if let Ok(x) = self.parse_primary_expr(diagnostics) {
                    x
                } else {
                    panic!("[uh oh ope] expected primary expression at {:?}", self.current())
                };

                match self.current_kind() {
                    Some(TokenKind::Operator) |
                    Some(TokenKind::LParen)   => operator_list.push(OpListItem::Expr(Box::new(expr))),

                    _ => return expr
                }
            } else {
                if let Ok(item) = self.parse_primary_in_oplist(diagnostics) {
                    operator_list.push(item);
                } else {
                    break
                }

                if !self.at_end() {
                    if let Some(TokenKind::Operator) = self.current_kind() {
                        continue
                    } else {
                        if let Ok(item) = self.parse_primary_in_oplist(diagnostics) {
                            operator_list.push(item);
                        } else {
                            break
                        }
                    }
                }
            }
        }
        
        Expr::OperationList(operator_list)
    }

    /// If it is at a `(` (or `[`) and inside an operator list, then it instead parses it as an `OpListItem::ParenGroup` (or `OpListItem::BracketGroup`), otherwise delegates to `parse_primary`.
    fn parse_primary_in_oplist(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Result<OpListItem, ()> {
        match self.current_kind() {
            Some(TokenKind::LParen) => {
                self.next();

                // case "()"
                if let Some(TokenKind::RParen) = self.current_kind() {
                    self.next();

                    return Ok(OpListItem::ParenGroup(vec![]));
                }

                let mut elems = vec![];
                let mut expr = None;
                loop {
                    match self.current_kind() {
                        Some(TokenKind::RParen) => {
                            elems.push(expr);
                            self.next();
                            break
                        }

                        Some(TokenKind::Comma) => {
                            elems.push(expr);
                            expr = None;
                            self.next();
                        }

                        _ if self.at_end() => todo!("report -- unterminated parenthetical group"),

                        _ => {
                            expr = Some(self.parse_expr(diagnostics))
                        }
                    }
                }

                Ok(OpListItem::ParenGroup(elems))
            }
            _ => self.parse_primary_expr(diagnostics).map(|x| OpListItem::Expr(Box::new(x)))
        }
    }

    /// Attempts to parse a primary expression. If it fails, it outputs an error. This diesn't mean it is an invalid expression, only that it is an invalid _primary_ expression.
    fn parse_primary_expr(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Result<Expr, ()> {
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

                        if let Some(TokenKind::RParen) = self.current_kind() {
                            self.next();

                            return Ok(Expr::Tuple(vec![expr]))
                        }
                        
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
                
                let s = s.as_str();

                // case i
                if s.len() == 0 {
                    Expr::Literal(Literal::Imaginary(
                        Complex(0.into(), 1.into())
                    ))
                } else {
                    Expr::Literal(Literal::Imaginary(s
                    .parse()
                    .unwrap()))
                }
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

            Some((_, TokenKind::LBracket)) => {
                self.next();

                todo!("vector & matrix")
            }
            
            // Symbols
            Some((t, TokenKind::Ident)) => {
                let expr = Expr::Symbol(GenericSymbol::SymbolToken(t));

                self.next();

                expr
            }

            _ => Err(())?,
        })
    }

    fn parse_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        self.parse_or_pat(diagnostics)
    }

    fn parse_or_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        let lhs = self.parse_with_pat(diagnostics);

        if self.at_op("|") {
            self.next();
            let rhs = self.parse_or_pat(diagnostics);

            Pattern::Or(Box::new(lhs), Box::new(rhs))
        } else {
            lhs
        }
    }

    fn parse_with_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        let pat = self.parse_and_pat(diagnostics);

        if self.at(TokenKind::With) {
            todo!()
        } else {
            pat
        }
    }

    fn parse_and_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        let lhs = self.parse_view_pat(diagnostics);

        if self.at_op("&") {
            self.next();
            
            let rhs = self.parse_and_pat(diagnostics);

            Pattern::And(Box::new(lhs), Box::new(rhs))
        } else {
            lhs
        }
    }

    fn parse_view_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        if self.at(TokenKind::Ident) {
            let tok = self.current().unwrap();

            // case "f -> " 
            if self.next_is_op("->") {
                    self.next();
                    self.next();

                    let rhs = self.parse_view_pat(diagnostics);

                    Pattern::View(Expr::Symbol(GenericSymbol::SymbolToken(tok)), Box::new(rhs))
            // just ident; fallback to next stage of pattern parsing
            } else {
                self.parse_as_pat(diagnostics)
            }
        } else if self.at_op("$") {
            let pin = self.parse_pin_pat(diagnostics);

            // case "$... ->" 
            if self.at_op("->") {
                self.next();

                let Pattern::Pin(lhs) = pin else {
                    unreachable!()
                };

                let rhs = self.parse_view_pat(diagnostics);

                Pattern::View(lhs, Box::new(rhs))
            // case "$..." just act as a regular pin pattern
            } else {
                self.parse_as_pat(diagnostics)
            }
        // otherwise, no valid view pattern, so continue up the chain of calls
        } else {
            self.parse_as_pat(diagnostics)
        }
    }

    fn parse_as_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        if let Some((t, TokenKind::Ident)) = self.current_tk() {
            if self.next_is_op("@") {
                self.next();
                self.next();

                let rhs = self.parse_as_pat(diagnostics);

                Pattern::As(GenericSymbol::SymbolToken(t), Box::new(rhs))
            } else {
                self.parse_cons_pat(diagnostics)
            }
        } else {
            self.parse_cons_pat(diagnostics)
        }
    }

    fn parse_cons_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        let lhs = self.parse_typed_pat(diagnostics);

        if self.at_op("::") {
            self.next();

            let rhs = self.parse_cons_pat(diagnostics);

            Pattern::Cons(Box::new(lhs), Box::new(rhs))
        } else {
            lhs
        }
    }

    fn parse_typed_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        let lhs = self.parse_unary_pat(diagnostics);

        if self.at(TokenKind::Colon) {
            if self.next_is(TokenKind::Ident) {
                let rhs = self.peek().unwrap();
                
                self.next();
                self.next();

                Pattern::Typed(Box::new(lhs), Expr::Symbol(GenericSymbol::SymbolToken(rhs)))
            } else if self.next_is_op("$") {
                self.next();

                let Pattern::Pin(rhs) = self.parse_pin_pat(diagnostics) else { unreachable!() };
                Pattern::Typed(Box::new(lhs), rhs)
            } else {
                todo!("report -- rhs of type pattern must either be a symbol or pin")
            }
        } else {
            lhs
        }
    }

    fn parse_unary_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        match self.current_kind() {
            Some(TokenKind::Operator) => match self.current()
                .unwrap()
                .span()
                .resolve_content(self.src)
                .unwrap() {
                    "!" => {
                        self.next();

                        Pattern::Not(Box::new(self.parse_primary_pat(diagnostics)))
                    }

                    "$" => {
                        self.parse_pin_pat(diagnostics)
                    }

                    c => todo!("report -- unknown char {c:?}")
            }

            _ => self.parse_primary_pat(diagnostics)
        }
    }

    fn parse_pin_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        self.consume_op("$");

        match self.current_kind() {
            Some(TokenKind::LParen) => {
                self.next();

                let expr = self.parse_expr(diagnostics);
                self.consume(TokenKind::RParen);

                Pattern::Pin(expr)
            }

            Some(TokenKind::Ident) => {
                Pattern::Pin(Expr::Symbol(GenericSymbol::SymbolToken(self.current().unwrap())))
            }

            _ => todo!("report -- expected identifier")
        }
    }

    fn parse_primary_pat(&mut self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> Pattern {
        match self.current_tk() {
            Some((t, TokenKind::Ident)) => {
                self.next();

                Pattern::Binding(GenericSymbol::SymbolToken(t))
            }

            Some((_, TokenKind::LParen)) => {
                self.next();

                // case ()
                if let Some(TokenKind::RParen) = self.current_kind() {
                    return Pattern::Literal(Literal::Unit);
                }

                let pat = self.parse_pat(diagnostics);

                match self.current_kind() {
                    Some(TokenKind::RParen) => {
                        self.next();

                        pat
                    }

                    Some(TokenKind::Comma) => {
                        self.next();

                        // case (p,)
                        if let Some(TokenKind::RParen) = self.current_kind() {
                            self.next();

                            Pattern::Tuple(vec![pat])
                        } else {
                            // case (p1, p2, ...)
                            let mut patterns = vec![];
                            loop {
                                patterns.push(self.parse_pat(diagnostics));
                                
                                match self.current_kind() {
                                    Some(TokenKind::Comma) => { self.next(); }
                                    Some(TokenKind::RParen) => { self.next(); break }
                                    _ => todo!("report -- expected ')' found {:?}", self.current())
                                }
                            }

                            Pattern::Tuple(patterns)
                        }
                    }

                    _ => todo!("report -- expected ')', found {:?}", self.current())
                }              
            }

            _ => todo!("report -- expected pattern, found {:?}", self.current())
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
