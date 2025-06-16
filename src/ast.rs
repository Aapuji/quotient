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
    Operation(Token)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Literal(Literal),
    Block(Box<Expr>),
    Tuple(Vec<Expr>),
    Chain(Box<Expr>, Box<Expr>),
    OperationList(Vec<OpListItem>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(Int),
    Real(Real),
    Imaginary(Complex),
    Bool(bool),
    Unit
}
