use std::ops::{self, Index, IndexMut};

use super::constants::{COEFFICIENTS_IN_RING_ELEMENT, FIELD_MODULUS};

pub(crate) type KyberFieldElement = i32;

const BARRETT_SHIFT: i32 = 26;
const BARRETT_R: i32 = 1i32 << BARRETT_SHIFT;
const BARRETT_MULTIPLIER: i32 = 20159; // floor((BARRETT_R / FIELD_MODULUS) + 0.5)

pub(crate) fn barrett_reduce(value: KyberFieldElement) -> KyberFieldElement {
    let quotient = ((value * BARRETT_MULTIPLIER) + (BARRETT_R >> 1)) >> BARRETT_SHIFT;

    value - (quotient * FIELD_MODULUS)
}

const MONTGOMERY_SHIFT: i64 = 16;
const MONTGOMERY_R: i64 = 1i64 << MONTGOMERY_SHIFT;
const INVERSE_OF_MODULUS_MOD_R: i64 = -3327; // FIELD_MODULUS^{-1} mod MONTGOMERY_R

pub(crate) fn montgomery_reduce(value: KyberFieldElement) -> KyberFieldElement {
    debug_assert!(value > -FIELD_MODULUS * (MONTGOMERY_R as i32));
    debug_assert!(value < FIELD_MODULUS * (MONTGOMERY_R as i32));

    let t = i64::from(value) * INVERSE_OF_MODULUS_MOD_R;
    let t = (t & (MONTGOMERY_R - 1)) as i16;

    let reduced =
        (value >> MONTGOMERY_SHIFT) - ((i32::from(t) * FIELD_MODULUS) >> MONTGOMERY_SHIFT);

    // If -qR < value < qR, we must have:
    // -3q/2 < reduced < 3q/2 ==> -3q < 2 * reduced < 3q
    debug_assert!(2 * reduced > -FIELD_MODULUS * 3);
    debug_assert!(2 * reduced < FIELD_MODULUS * 3);

    reduced
}

// Given a |value|, return |value|*R mod q. Notice that montgomery_reduce
// returns a value aR^{-1} mod q, and so montgomery_reduce(|value| * R^2)
// returns |value| * R^2 & R^{-1} mod q  = |value| * R mod q.
pub(crate) fn to_montgomery_domain(value: KyberFieldElement) -> KyberFieldElement {
    // R^2 mod q = (2^16)^2 mod 3329 = 1353
    montgomery_reduce(1353 * value)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KyberPolynomialRingElement {
    pub(crate) coefficients: [KyberFieldElement; COEFFICIENTS_IN_RING_ELEMENT],
}

impl KyberPolynomialRingElement {
    pub const ZERO: Self = Self {
        coefficients: [0i32; 256], // FIXME: hax issue, this is COEFFICIENTS_IN_RING_ELEMENT
    };
}

// Adding this to a module to ignore it for extraction.
mod mutable_operations {
    use super::*;

    impl IndexMut<usize> for KyberPolynomialRingElement {
        fn index_mut(&mut self, index: usize) -> &mut Self::Output {
            &mut self.coefficients[index]
        }
    }
}

impl Index<usize> for KyberPolynomialRingElement {
    type Output = KyberFieldElement;

    fn index(&self, index: usize) -> &Self::Output {
        &self.coefficients[index]
    }
}

impl IntoIterator for KyberPolynomialRingElement {
    type Item = KyberFieldElement;

    type IntoIter = std::array::IntoIter<KyberFieldElement, COEFFICIENTS_IN_RING_ELEMENT>;

    fn into_iter(self) -> Self::IntoIter {
        self.coefficients.into_iter()
    }
}

impl ops::Add for KyberPolynomialRingElement {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let mut result = KyberPolynomialRingElement::ZERO;
        for i in 0..COEFFICIENTS_IN_RING_ELEMENT {
            result.coefficients[i] = self.coefficients[i] + other.coefficients[i];
        }
        result
    }
}

impl ops::Sub for KyberPolynomialRingElement {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        let mut result = KyberPolynomialRingElement::ZERO;
        for i in 0..COEFFICIENTS_IN_RING_ELEMENT {
            result.coefficients[i] = self.coefficients[i] - other.coefficients[i];
        }
        result
    }
}
