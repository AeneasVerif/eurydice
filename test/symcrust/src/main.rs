#[allow(non_snake_case)]
#[no_mangle]
pub fn SymCrustMlKemPolyElementCompressAndEncode(
    coeffs:              &[u16; 256],
    nBitsPerCoefficient: u32,
    dst:                 &mut [u8] )
{
    let SYMCRYPT_MLKEM_COMPRESS_MULCONSTANT : u64 = 0x9d7dbb;
    let SYMCRYPT_MLKEM_COMPRESS_SHIFTCONSTANT : u32 = 35;
    let mut multiplication: u64;
    let mut coefficient: u32;
    let mut nBitsInCoefficient: u32;
    let mut bitsToEncode: u32;
    let mut nBitsToEncode: u32;
    let mut cbDstWritten: usize = 0;
    let mut accumulator: u32 = 0;
    let mut nBitsInAccumulator: u32 = 0;

    assert!(nBitsPerCoefficient > 0);
    assert!(nBitsPerCoefficient <= 12);
    assert!( dst.len() as u64 == (256*u64::from(nBitsPerCoefficient) / 8) );

    for i in 0..256
    {
        nBitsInCoefficient = nBitsPerCoefficient;
        coefficient = u32::from(coeffs[i]); // in range [0, Q-1]

        // first compress the coefficient
        // when nBitsPerCoefficient < 12 we compress per Compress_d in draft FIPS 203;
        if nBitsPerCoefficient < 12
        {
            // Multiply by 2^(nBitsPerCoefficient+1) / Q by multiplying by constant and shifting right
            multiplication = u64::from(coefficient) * SYMCRYPT_MLKEM_COMPRESS_MULCONSTANT;
            coefficient = (multiplication >> (SYMCRYPT_MLKEM_COMPRESS_SHIFTCONSTANT-(nBitsPerCoefficient+1))) as u32;

            // add "half" to round to nearest integer
            coefficient += 1;

            // final divide by two to get multiplication by 2^nBitsPerCoefficient / Q
            coefficient >>= 1;                              // in range [0, 2^nBitsPerCoefficient]

            // modular reduction by masking
            coefficient &= (1u32<<nBitsPerCoefficient)-1;    // in range [0, 2^nBitsPerCoefficient - 1]
        }

        // encode the coefficient
        // simple loop to add bits to accumulator and write accumulator to output
        while nBitsInCoefficient > 0
        {
            nBitsToEncode = nBitsInCoefficient.min(32-nBitsInAccumulator);

            bitsToEncode = coefficient & ((1u32<<nBitsToEncode)-1);
            coefficient >>= nBitsToEncode;
            nBitsInCoefficient -= nBitsToEncode;

            accumulator |= bitsToEncode << nBitsInAccumulator;
            nBitsInAccumulator += nBitsToEncode;
            if nBitsInAccumulator == 32
            {
                dst[cbDstWritten..cbDstWritten+4].copy_from_slice( &accumulator.to_le_bytes() );
                cbDstWritten += 4;
                accumulator = 0;
                nBitsInAccumulator = 0;
            }
        }
    }
}

fn main() {
    // let mut dst = [0; 256];
    // SymCrustMlKemPolyElementCompressAndEncode(&[0; 256], 1, &mut dst);
}
