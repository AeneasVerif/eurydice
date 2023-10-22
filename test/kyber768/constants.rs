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

/// PKE message size
pub(crate) const SHARED_SECRET_SIZE: usize = 32;

pub(crate) const CPA_PKE_KEY_GENERATION_SEED_SIZE: usize = 32;

// [hax]: hacspec/hacspec-v2#27 stealing error
//        Using these functions causes stealing errors in hax.
// /// Compute serialized length for output size of ByteEncode
// pub(in crate::kem::kyber) const fn serialized_len<const K: usize, const OUT_LEN: usize>() -> usize {
//     OUT_LEN * K
// }

// /// Compute block length for output block size of ByteEncode u (c1)
// pub(in crate::kem::kyber) const fn block_len<const FACTOR: usize>() -> usize {
//     (COEFFICIENTS_IN_RING_ELEMENT * FACTOR) / 8
// }

pub(crate) const H_DIGEST_SIZE: usize = 32;
