use std::ops::{Add, Div, Mul, Rem, Sub};

pub struct Rational<T> {
    pub numerator: T,
    pub denominator: T,
}

impl<T> Rational<T>
where
    T: Mul<Output = T> + Add<Output = T> + Sub<Output = T> + Rem<Output = T> + Div<Output = T> + Default + PartialEq + Copy,
{
    pub fn new(numerator: T, denominator: T) -> Self {
        Self {
            numerator,
            denominator,
        }
    }
    pub fn is_integer(&self) -> bool {
        self.numerator % self.denominator == T::default()
    }
    pub fn get_integer_value(&self) -> T {
        self.numerator / self.denominator
    }
    pub fn add(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator * other.denominator + other.numerator * self.denominator,
            denominator: self.denominator * other.denominator,
        }
    }
    pub fn sub(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator * other.denominator - other.numerator * self.denominator,
            denominator: self.denominator * other.denominator,
        }
    }
    #[allow(dead_code)]
    pub fn mul(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator * other.numerator,
            denominator: self.denominator * other.denominator,
        }
    }
    pub fn mul_scalar(&self, scalar: T) -> Self {
        Self {
            numerator: self.numerator * scalar,
            denominator: self.denominator,
        }
    }
}