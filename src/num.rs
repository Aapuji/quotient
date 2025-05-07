pub trait Num {

}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Int(u64);

impl Num for Int {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Real(u64, u64);

impl Num for Real {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Complex(Real, Real);

impl Num for Complex {}
