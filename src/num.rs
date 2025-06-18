use std::{num::ParseIntError, str::FromStr};

use ordered_float::NotNan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int(pub i64);

impl FromStr for Int {
    type Err = <i64 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        <i64 as FromStr>::from_str(s).map(|n| Self(n))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Real(pub NotNan<f64>);

impl FromStr for Real {
    type Err = <NotNan<f64> as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        <NotNan<f64> as FromStr>::from_str(s).map(|n| Self(n))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Complex(pub NotNan<f64>, pub NotNan<f64>);

// Is actually for imaginary
impl FromStr for Complex {
    type Err = <NotNan<f64> as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        <NotNan<f64> as FromStr>::from_str(s).map(|n| Self(NotNan::<f64>::new(0.).unwrap(), n))
    }
}
