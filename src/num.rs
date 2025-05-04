use std::hash::Hash;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Int(u64);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Real(u64, u64);
