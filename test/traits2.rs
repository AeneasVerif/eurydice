pub trait Operations: Copy + Clone {
    fn ZERO() -> Self;
    fn from_i16_array(array: &[i16]) -> Self;
}

pub(crate) struct PolynomialRingElement<Vector: Operations> {
    pub(crate) coefficients: [Vector; 16],
}

fn ZERO<Vector: Operations>() -> PolynomialRingElement<Vector> {
    PolynomialRingElement {
        // https://github.com/hacspec/hax/issues/27
        // FIXME:  The THIR body of item DefId(0:415 ~ libcrux_ml_kem[9000]::polynomial::{impl#0}::ZERO::{constant#0}) was stolen.
        coefficients: [Vector::ZERO(); 16],
    }
}

fn from_i16_array<Vector: Operations>(a: &[i16]) -> PolynomialRingElement<Vector> {
    let mut result = ZERO();
    for i in 0..16 {
        result.coefficients[i] = Vector::from_i16_array(&a[i * 16..(i + 1) * 16]);
    }
    result
}

impl<Vector: Operations> PolynomialRingElement<Vector> {
    pub(crate) fn ZERO() -> Self {
        Self {
            coefficients: [Vector::ZERO(); 16],
        }
    }
    pub fn from_i16_array(a: &[i16]) -> Self {
        from_i16_array(a)
    }
}

fn sample_from_xof<const K: usize, Vector: Operations>(
    seeds: &[[u8; 34]; K],
) -> [PolynomialRingElement<Vector>; K] {

    let mut out: [[i16; 272]; K] = [[0; 272]; K];
    out.map(|s| PolynomialRingElement::<Vector>::from_i16_array(&s[0..256]))
}

fn main() {}
