use crate::{
    arithmetic::{
        barrett_reduce, montgomery_reduce, to_montgomery_domain, KyberFieldElement,
        KyberPolynomialRingElement,
    },
    constants::COEFFICIENTS_IN_RING_ELEMENT,
};

const ZETAS_MONTGOMERY_DOMAIN: [KyberFieldElement; 128] = [
    -1044, -758, -359, -1517, 1493, 1422, 287, 202, -171, 622, 1577, 182, 962, -1202, -1474, 1468,
    573, -1325, 264, 383, -829, 1458, -1602, -130, -681, 1017, 732, 608, -1542, 411, -205, -1571,
    1223, 652, -552, 1015, -1293, 1491, -282, -1544, 516, -8, -320, -666, -1618, -1162, 126, 1469,
    -853, -90, -271, 830, 107, -1421, -247, -951, -398, 961, -1508, -725, 448, -1065, 677, -1275,
    -1103, 430, 555, 843, -1251, 871, 1550, 105, 422, 587, 177, -235, -291, -460, 1574, 1653, -246,
    778, 1159, -147, -777, 1483, -602, 1119, -1590, 644, -872, 349, 418, 329, -156, -75, 817, 1097,
    603, 610, 1322, -1285, -1465, 384, -1215, -136, 1218, -1335, -874, 220, -1187, -1659, -1185,
    -1530, -1278, 794, -1510, -854, -870, 478, -108, -308, 996, 991, 958, -1460, 1522, 1628,
];

#[inline(always)]
pub fn ntt_representation(
    mut re: KyberPolynomialRingElement,
) -> KyberPolynomialRingElement {
    let mut zeta_i = 0;

    macro_rules! layers {
        ($layer:literal) => {
            for offset in (0..(COEFFICIENTS_IN_RING_ELEMENT - $layer)).step_by(2 * $layer) {
                zeta_i += 1;

                for j in offset..offset + $layer {
                    let t = montgomery_reduce(re[j + $layer] * ZETAS_MONTGOMERY_DOMAIN[zeta_i]);
                    re[j + $layer] = re[j] - t;
                    re[j] = re[j] + t;
                }
            }
        };
    }

    layers!(128);
    layers!(64);
    layers!(32);
    layers!(16);
    layers!(8);
    layers!(4);
    layers!(2);

    re.coefficients = re.coefficients.map(barrett_reduce);

    re
}

#[inline(always)]
pub fn invert_ntt_montgomery(
    mut re: KyberPolynomialRingElement,
) -> KyberPolynomialRingElement {
    let mut zeta_i = COEFFICIENTS_IN_RING_ELEMENT / 2;

    macro_rules! layers {
        ($layer:literal) => {
            for offset in (0..(COEFFICIENTS_IN_RING_ELEMENT - $layer)).step_by(2 * $layer) {
                zeta_i -= 1;
                let zeta_i_value = ZETAS_MONTGOMERY_DOMAIN[zeta_i];
                let end = offset + $layer;

                for j in offset..end {
                    let a_minus_b = re[j + $layer] - re[j];

                    // Instead of dividing by 2 here, we just divide by
                    // 2^7 in one go in the end.
                    re[j] = re[j] + re[j + $layer];
                    re[j + $layer] = montgomery_reduce(a_minus_b * zeta_i_value);
                }
            }
        };
    }

    layers!(2);
    layers!(4);
    layers!(8);
    layers!(16);
    layers!(32);
    layers!(64);
    layers!(128);

    re.coefficients = re
        .coefficients
        .map(|coefficient| barrett_reduce(montgomery_reduce(coefficient * 1441)));

    re
}

#[inline(always)]
fn ntt_multiply_binomials(
    (a0, a1): (KyberFieldElement, KyberFieldElement),
    (b0, b1): (KyberFieldElement, KyberFieldElement),
    zeta: i32,
) -> (KyberFieldElement, KyberFieldElement) {
    (
        montgomery_reduce(a0 * b0) + montgomery_reduce(montgomery_reduce(a1 * b1) * zeta),
        montgomery_reduce(a0 * b1) + montgomery_reduce(a1 * b0),
    )
}

#[inline(always)]
fn ntt_multiply(
    left: &KyberPolynomialRingElement,
    right: &KyberPolynomialRingElement,
) -> KyberPolynomialRingElement {
    let mut out = KyberPolynomialRingElement::ZERO;

    for i in (0..COEFFICIENTS_IN_RING_ELEMENT).step_by(4) {
        let product = ntt_multiply_binomials(
            (left[i], left[i + 1]),
            (right[i], right[i + 1]),
            ZETAS_MONTGOMERY_DOMAIN[64 + (i / 4)],
        );
        out[i] = product.0;
        out[i + 1] = product.1;

        let product = ntt_multiply_binomials(
            (left[i + 2], left[i + 3]),
            (right[i + 2], right[i + 3]),
            -ZETAS_MONTGOMERY_DOMAIN[64 + (i / 4)],
        );
        out[i + 2] = product.0;
        out[i + 3] = product.1;
    }

    out
}

#[inline(always)]
pub fn multiply_row_by_column_montgomery<const K: usize>(
    row_vector: &[KyberPolynomialRingElement; K],
    column_vector: &[KyberPolynomialRingElement; K],
) -> KyberPolynomialRingElement {
    let mut result = KyberPolynomialRingElement::ZERO;

    for (row_element, column_element) in row_vector.iter().zip(column_vector.iter()) {
        result = result + ntt_multiply(row_element, column_element);
    }

    result.coefficients = result.coefficients.map(barrett_reduce);

    result
}

#[inline(always)]
pub fn multiply_matrix_by_column_montgomery<const K: usize>(
    matrix: &[[KyberPolynomialRingElement; K]; K],
    vector: &[KyberPolynomialRingElement; K],
) -> [KyberPolynomialRingElement; K] {
    let mut result = [KyberPolynomialRingElement::ZERO; K];

    for (i, row) in matrix.iter().enumerate() {
        for (j, matrix_element) in row.iter().enumerate() {
            let product = ntt_multiply(matrix_element, &vector[j]);
            result[i] = result[i] + product;
        }

        result[i].coefficients = result[i].coefficients.map(barrett_reduce);
    }

    result
}

// NOTE: This function performs matrix multiplication, then conversion from the
// montgomery domain, and last barrett reduction. It is only used in
// ind_cpa::generate_keypair(). (TODO: Verify this) Doing barrett reduction in
// this function after conversion from montgomery form lets us skip an extra
// barrett reduction step in generate_keypair itself.
#[inline(always)]
pub fn multiply_matrix_by_column<const K: usize>(
    matrix: &[[KyberPolynomialRingElement; K]; K],
    vector: &[KyberPolynomialRingElement; K],
) -> [KyberPolynomialRingElement; K] {
    let mut result = [KyberPolynomialRingElement::ZERO; K];

    for (i, row) in matrix.iter().enumerate() {
        for (j, matrix_element) in row.iter().enumerate() {
            let product = ntt_multiply(matrix_element, &vector[j]);
            result[i] = result[i] + product;
        }

        // The coefficients of the form aR^{-1} mod q, which means
        // calling to_montgomery_domain() on them should return a mod q.
        result[i].coefficients = result[i].coefficients.map(|coefficient| {
            let coefficient_montgomery = to_montgomery_domain(coefficient);
            barrett_reduce(coefficient_montgomery)
        });
    }

    result
}
