pub trait Operations: Copy + Clone {
    fn zero() -> Self;
    fn from_i16_array(array: &[i16]) -> Self;
}

pub(crate) struct PolynomialRingElement<Vector: Operations> {
    pub(crate) coefficients: [Vector; 16],
}

fn re_zero<Vector: Operations>() -> PolynomialRingElement<Vector> {
    PolynomialRingElement {
        coefficients: [Vector::zero(); 16],
    }
}

fn re_from_i16_array<Vector: Operations>(a: &[i16]) -> PolynomialRingElement<Vector> {
    let mut result = re_zero();
    for i in 0..16 {
        result.coefficients[i] = Vector::from_i16_array(&a[i * 16..(i + 1) * 16]);
    }
    result
}

impl<Vector: Operations> PolynomialRingElement<Vector> {
    pub(crate) fn zero() -> Self {
        re_zero()
    }
    pub fn from_i16_array(a: &[i16]) -> Self {
        re_from_i16_array(a)
    }
}

fn sample_from_xof<const K: usize, Vector: Operations>() -> [PolynomialRingElement<Vector>; K] {

    let mut out: [[i16; 272]; K] = [[0; 272]; K];
    out.map(|s| PolynomialRingElement::<Vector>::from_i16_array(&s[0..256]))
}

fn main() {}
