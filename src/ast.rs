// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct Symbol;

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub enum Fixity {
//     Prefix,
//     Postfix,
//     InfixLeft,
//     InfixRight,
//     Infix
// }

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum Prec {
//     Raw(u8),
//     Eq(Symbol)
// }

use crate::{num::{Complex, Int, Real}, token::Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TopLevel {
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OpListItem {
    Expr(Box<Expr>),
    Operation(Token),
    ParenGroup(Vec<Option<Expr>>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Literal(Literal),
    Symbol(GenericSymbol), // only holds the identifier token
    Block(Box<Expr>),
    Tuple(Vec<Expr>),
    Chain(Box<Expr>, Box<Expr>),
    OperationList(Vec<OpListItem>),
    Let(Let)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GenericSymbol {
    SymbolToken(Token),
    SymbolId(()) // TODO
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(Int),
    Real(Real),
    Imaginary(Complex),
    Bool(bool),
    Unit
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Literal(Literal),
    Binding(GenericSymbol),
    Tuple(Vec<Pattern>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub lhs: Pattern,
    pub rhs: Box<Expr>
}
