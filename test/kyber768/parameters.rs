/// Field modulus: 3329
pub(crate) const FIELD_MODULUS: i32 = 3329;

/// Each field element needs floor(log_2(FIELD_MODULUS)) + 1 = 12 bits to represent
pub(crate) const BITS_PER_COEFFICIENT: usize = 12;

/// Coefficients per ring element
pub(crate) const COEFFICIENTS_IN_RING_ELEMENT: usize = 256;

/// Bits required per (uncompressed) ring element
pub(crate) const BITS_PER_RING_ELEMENT: usize = COEFFICIENTS_IN_RING_ELEMENT * 12;

/// Bytes required per (uncompressed) ring element
pub(crate) const BYTES_PER_RING_ELEMENT: usize = BITS_PER_RING_ELEMENT / 8;

/// Seed size for rejection sampling.
///
/// See <https://eprint.iacr.org/2023/708> for some background regarding
/// this choice.
pub(crate) const REJECTION_SAMPLING_SEED_SIZE: usize = 168 * 5;

/// Rank
pub(crate) const RANK: usize = 3;

/// `T` NTT encoding size in bytes
pub(crate) const T_AS_NTT_ENCODED_SIZE: usize =
    (RANK * COEFFICIENTS_IN_RING_ELEMENT * BITS_PER_COEFFICIENT) / 8;

/// Compression factor for `U`
pub(crate) const VECTOR_U_COMPRESSION_FACTOR: usize = 10;

/// Compression factor for `V`
pub(crate) const VECTOR_V_COMPRESSION_FACTOR: usize = 4;

/// `U` encoding size in bytes
pub(crate) const BYTES_PER_ENCODED_ELEMENT_OF_U: usize =
    (COEFFICIENTS_IN_RING_ELEMENT * VECTOR_U_COMPRESSION_FACTOR) / 8;
pub(crate) const VECTOR_U_ENCODED_SIZE: usize = RANK * BYTES_PER_ENCODED_ELEMENT_OF_U;

/// `V` encoding size in bytes
pub(crate) const VECTOR_V_ENCODED_SIZE: usize =
    (COEFFICIENTS_IN_RING_ELEMENT * VECTOR_V_COMPRESSION_FACTOR) / 8;
