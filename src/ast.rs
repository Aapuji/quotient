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

use crate::num::{Int, Real, Complex};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TopLevel {
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Expr {
    Literal(Literal)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(Int),
    Real(Real),
    Imaginary(Complex),
    Bool(bool),
    Unit
}
