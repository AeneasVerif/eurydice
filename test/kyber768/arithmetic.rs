use super::constants::{COEFFICIENTS_IN_RING_ELEMENT, FIELD_MODULUS};

pub(crate) type KyberFieldElement = i32;

const BARRETT_SHIFT: i32 = 26;
const BARRETT_R: i32 = 1i32 << BARRETT_SHIFT;
const BARRETT_MULTIPLIER: i32 = 20159; // floor((BARRETT_R / FIELD_MODULUS) + 0.5)

#[cfg_attr(hax, hax_lib_macros::requires(value >= -106527 && value <= 104862))]
#[cfg_attr(hax, hax_lib_macros::ensures(|result| result > -FIELD_MODULUS && result < FIELD_MODULUS))]
pub(crate) fn barrett_reduce(value: KyberFieldElement) -> KyberFieldElement {
    let quotient = ((value * BARRETT_MULTIPLIER) + (BARRETT_R >> 1)) >> BARRETT_SHIFT;

    value - (quotient * FIELD_MODULUS)
}

const MONTGOMERY_SHIFT: i32 = 16;
const MONTGOMERY_R: i32 = 1i32 << MONTGOMERY_SHIFT;
const INVERSE_OF_MODULUS_MOD_R: i32 = -3327; // FIELD_MODULUS^{-1} mod MONTGOMERY_R

#[cfg_attr(hax, hax_lib_macros::requires(value > -FIELD_MODULUS * MONTGOMERY_R / 2 && value < FIELD_MODULUS * MONTGOMERY_R / 2))]
#[cfg_attr(hax, hax_lib_macros::ensures(|result| result > -FIELD_MODULUS && result < FIELD_MODULUS))]
pub(crate) fn montgomery_reduce(value: KyberFieldElement) -> KyberFieldElement {
    let k = (value & (MONTGOMERY_R - 1)) * INVERSE_OF_MODULUS_MOD_R;
    let k = (k & (MONTGOMERY_R - 1)) as i16;

    let c = (i32::from(k) * FIELD_MODULUS) >> MONTGOMERY_SHIFT;
    let value_high = value >> MONTGOMERY_SHIFT;

    value_high - c
}

#[cfg_attr(hax, hax_lib_macros::requires(fe >= -FIELD_MODULUS && fe < FIELD_MODULUS))]
#[cfg_attr(hax, hax_lib_macros::ensures(|result| result >= 0 && i32::from(result) < FIELD_MODULUS))]
#[inline(always)]
pub(crate) fn to_unsigned_representative(fe: KyberFieldElement) -> u16 {
    (fe + ((fe >> 15) & FIELD_MODULUS)) as u16
}

#[derive(Clone, Copy)]
pub struct KyberPolynomialRingElement {
    pub(crate) coefficients: [KyberFieldElement; COEFFICIENTS_IN_RING_ELEMENT],
}

impl KyberPolynomialRingElement {
    pub const ZERO: Self = Self {
        coefficients: [0i32; 256], // FIXME: hax issue, this is COEFFICIENTS_IN_RING_ELEMENT
    };
}

pub(crate) fn add_to_ring_element(
    mut lhs: KyberPolynomialRingElement,
    rhs: &KyberPolynomialRingElement,
) -> KyberPolynomialRingElement {
    for i in 0..lhs.coefficients.len() {
        lhs.coefficients[i] += rhs.coefficients[i];
    }

    lhs
}
