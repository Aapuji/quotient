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
    Let(Let),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Box<Pattern>, Box<Expr>)>),
    Using(Box<Expr>, Box<Pattern>, Option<(Box<Pattern>, Box<Expr>)>, Box<Expr>)
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
    Underscore,
    Binding(GenericSymbol),
    Tuple(Vec<Pattern>),
    // record here as well
    Pin(Expr), // a pin pattern here is one that holds an expression. In other places that expect an expression, a pin is valid too, but will be parsed as that expression, skipping the pin node.
    Not(Box<Pattern>),
    Typed(Box<Pattern>, Expr),
    Cons(Box<Pattern>, Box<Pattern>),
    As(GenericSymbol, Box<Pattern>),
    View(Expr, Box<Pattern>),
    And(Box<Pattern>, Box<Pattern>),
    With(Box<Pattern>, Vec<(GenericSymbol, Expr)>), // expressions here are required to be pinned, but the pins will be skipped
    Or(Box<Pattern>, Box<Pattern>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub lhs: Box<Pattern>,
    pub rhs: Box<Expr>
}
