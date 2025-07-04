/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "symcrust.h"

#include "Eurydice.h"
#include "internal/Eurydice.h"

void
symcrust_SymCrustMlKemPolyElementCompressAndEncode(
  uint16_t *coeffs,
  uint32_t nBitsPerCoefficient,
  Eurydice_slice dst
)
{
  uint64_t SYMCRYPT_MLKEM_COMPRESS_MULCONSTANT = 10321339ULL;
  uint32_t SYMCRYPT_MLKEM_COMPRESS_SHIFTCONSTANT = 35U;
  size_t cbDstWritten = (size_t)0U;
  uint32_t accumulator = 0U;
  uint32_t nBitsInAccumulator = 0U;
  EURYDICE_ASSERT(nBitsPerCoefficient > 0U, "panic!");
  EURYDICE_ASSERT(nBitsPerCoefficient <= 12U, "panic!");
  EURYDICE_ASSERT((uint64_t)Eurydice_slice_len(dst, uint8_t) ==
      256ULL * (uint64_t)nBitsPerCoefficient / 8ULL,
    "panic!");
  for (size_t i = (size_t)0U; i < (size_t)256U; i++)
  {
    size_t i0 = i;
    uint32_t nBitsInCoefficient = nBitsPerCoefficient;
    uint32_t coefficient = (uint32_t)coeffs[i0];
    if (nBitsPerCoefficient < 12U)
    {
      uint64_t multiplication = (uint64_t)coefficient * SYMCRYPT_MLKEM_COMPRESS_MULCONSTANT;
      coefficient =
        (uint32_t)(multiplication >>
          (uint32_t)(SYMCRYPT_MLKEM_COMPRESS_SHIFTCONSTANT - (nBitsPerCoefficient + 1U)));
      coefficient++;
      coefficient = coefficient >> 1U;
      coefficient = coefficient & ((1U << (uint32_t)nBitsPerCoefficient) - 1U);
    }
    while (nBitsInCoefficient > 0U)
    {
      uint32_t nBitsToEncode = Eurydice_min_u32(nBitsInCoefficient, 32U - nBitsInAccumulator);
      uint32_t bitsToEncode = coefficient & ((1U << (uint32_t)nBitsToEncode) - 1U);
      coefficient = coefficient >> (uint32_t)nBitsToEncode;
      nBitsInCoefficient = nBitsInCoefficient - nBitsToEncode;
      accumulator = accumulator | bitsToEncode << (uint32_t)nBitsInAccumulator;
      nBitsInAccumulator = nBitsInAccumulator + nBitsToEncode;
      if (nBitsInAccumulator == 32U)
      {
        Eurydice_slice
        uu____0 = Eurydice_slice_subslice3(dst, cbDstWritten, cbDstWritten + (size_t)4U, uint8_t *);
        uint8_t ret[4U];
        core_num__u32__to_le_bytes(accumulator, ret);
        Eurydice_slice_copy(uu____0, Eurydice_array_to_slice((size_t)4U, ret, uint8_t), uint8_t);
        cbDstWritten = cbDstWritten + (size_t)4U;
        accumulator = 0U;
        nBitsInAccumulator = 0U;
      }
    }
  }
}

void symcrust_main(void)
{

}

