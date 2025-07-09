#pragma once

// New types & definitions for 128-bit integers

#if defined(__SIZEOF_INT128__)
#define HAS_INT128 1
#endif

#ifdef HAS_INT128
#include <inttypes.h>

typedef __int128_t Eurydice_Int128_int128_t;
typedef __uint128_t Eurydice_Int128_uint128_t;

static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_from_bits(uint64_t hi, uint64_t lo) {
  return ((Eurydice_Int128_int128_t)hi << 64) | lo;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_from_bits(uint64_t hi, uint64_t lo) {
  return ((Eurydice_Int128_uint128_t)hi << 64) | lo;
}
static inline bool Eurydice_Int128_i128_eq(Eurydice_Int128_int128_t x, Eurydice_Int128_int128_t y) {
  return x == y;
}
static inline bool Eurydice_Int128_u128_eq(Eurydice_Int128_uint128_t x, Eurydice_Int128_uint128_t y) {
  return x == y;
}

static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_add(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs + rhs;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_add(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs + rhs;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_sub(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs - rhs;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_sub(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs - rhs;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_mul(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs * rhs;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_mul(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs * rhs;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_div(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs / rhs;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_div(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs / rhs;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_mod(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs % rhs;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_mod(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs % rhs;
}
static inline 
Eurydice_Int128_int128_t Eurydice_Int128_i128_bor(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs | rhs;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_bor(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs | rhs;
}
static inline 
Eurydice_Int128_int128_t Eurydice_Int128_i128_band(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs & rhs;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_band(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs & rhs;
}
static inline 
Eurydice_Int128_int128_t Eurydice_Int128_i128_bxor(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs ^ rhs;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_bxor(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs ^ rhs;
}
static inline 
Eurydice_Int128_int128_t Eurydice_Int128_i128_shl(Eurydice_Int128_int128_t lhs, uint32_t rhs) {
  return lhs << rhs;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_shl(Eurydice_Int128_uint128_t lhs, uint32_t rhs) {
  return lhs << rhs;
}
static inline 
Eurydice_Int128_int128_t Eurydice_Int128_i128_shr(Eurydice_Int128_int128_t lhs, uint32_t rhs) {
  return lhs >> rhs;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_shr(Eurydice_Int128_uint128_t lhs, uint32_t rhs) {
  return lhs >> rhs;
}
static inline 
Eurydice_Int128_int128_t Eurydice_Int128_i128_bnot(Eurydice_Int128_int128_t x) {
  return ~x;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_bnot(Eurydice_Int128_uint128_t x) {
  return ~x;
}
static inline 
bool Eurydice_Int128_i128_lt(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs < rhs;
}
static inline 
bool Eurydice_Int128_u128_lt(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs < rhs;
}
static inline 
bool Eurydice_Int128_i128_gt(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs > rhs;
}
static inline 
bool Eurydice_Int128_u128_gt(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs > rhs;
}
static inline 
bool Eurydice_Int128_i128_lte(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs <= rhs;
}
static inline 
bool Eurydice_Int128_u128_lte(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs <= rhs;
}
static inline 
bool Eurydice_Int128_i128_gte(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs >= rhs;
}
static inline 
bool Eurydice_Int128_u128_gte(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs >= rhs;
}
static inline 
bool Eurydice_Int128_i128_neq(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return lhs != rhs;
}
static inline 
bool Eurydice_Int128_u128_neq(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return lhs != rhs;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_neg(Eurydice_Int128_int128_t x) {
  return -x;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_neg(Eurydice_Int128_uint128_t x) {
  return -x;
}
#else
typedef struct {
  uint64_t hi;
  uint64_t lo;
} Eurydice_Int128_int128_t, Eurydice_Int128_uint128_t;
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_from_bits(uint64_t hi, uint64_t lo) {
  return (Eurydice_Int128_int128_t){.hi = hi, .lo = lo};
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_from_bits(uint64_t hi, uint64_t lo) {
  return (Eurydice_Int128_uint128_t){.hi = hi, .lo = lo};
}
static inline bool Eurydice_Int128_i128_eq(Eurydice_Int128_int128_t x, Eurydice_Int128_int128_t y) {
  return x.hi == y.hi && x.lo == y.lo;
}
static inline bool Eurydice_Int128_u128_eq(Eurydice_Int128_uint128_t x, Eurydice_Int128_uint128_t y) {
  return x.hi == y.hi && x.lo == y.lo;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_add(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  uint64_t lo = lhs.lo + rhs.lo;
  uint64_t carry = (lo < lhs.lo) ? 1 : 0;
  return (Eurydice_Int128_int128_t){.hi = lhs.hi + rhs.hi + carry, .lo = lo};
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_add(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  uint64_t lo = lhs.lo + rhs.lo;
  uint64_t carry = (lo < lhs.lo) ? 1 : 0;
  return (Eurydice_Int128_uint128_t){.hi = lhs.hi + rhs.hi + carry, .lo = lo};
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_sub(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  uint64_t lo = lhs.lo - rhs.lo;
  uint64_t borrow = (lhs.lo < rhs.lo) ? 1 : 0;
  return (Eurydice_Int128_int128_t){.hi = lhs.hi - rhs.hi - borrow, .lo = lo};
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_sub(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  uint64_t lo = lhs.lo - rhs.lo;
  uint64_t borrow = (lhs.lo < rhs.lo) ? 1 : 0;
  return (Eurydice_Int128_uint128_t){.hi = lhs.hi - rhs.hi - borrow, .lo = lo};
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_neg(Eurydice_Int128_uint128_t x) {
  Eurydice_Int128_uint128_t result;
  result.lo = ~x.lo + 1;
  result.hi = ~x.hi + (result.lo == 0 ? 1 : 0);
  return result;
}
static inline Eurydice_Int128_int128_t i128_of_u128(Eurydice_Int128_uint128_t x) {
  Eurydice_Int128_int128_t result;
  result.lo = x.lo;
  result.hi = x.hi;
  return result;
}
static inline Eurydice_Int128_uint128_t u128_of_i128(Eurydice_Int128_int128_t x) {
  Eurydice_Int128_uint128_t result;
  result.lo = x.lo;
  result.hi = x.hi;
  return result;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_neg(Eurydice_Int128_int128_t x) {
  return i128_of_u128(Eurydice_Int128_u128_neg(u128_of_i128(x)));
}
static inline void _mul64x64(uint64_t a, uint64_t b, uint64_t* lo, uint64_t* hi) {
  uint64_t a_lo = (uint32_t)a;
  uint64_t a_hi = a >> 32;
  uint64_t b_lo = (uint32_t)b;
  uint64_t b_hi = b >> 32;
  uint64_t p0 = a_lo * b_lo;
  uint64_t p1 = a_lo * b_hi;
  uint64_t p2 = a_hi * b_lo;
  uint64_t p3 = a_hi * b_hi;
  uint64_t carry = ((p0 >> 32) + (uint32_t)p1 + (uint32_t)p2) >> 32;
  *lo = p0 + ((p1 + p2) << 32);
  *hi = p3 + (p1 >> 32) + (p2 >> 32) + carry;
}
static inline uint64_t _add64x64_carry(uint64_t a, uint64_t b, uint64_t* sum) {
  *sum = a + b;
  return (*sum < a) ? 1 : 0;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_mul(Eurydice_Int128_uint128_t a, Eurydice_Int128_uint128_t b) {
  uint64_t a_lo = a.lo, a_hi = a.hi;
  uint64_t b_lo = b.lo, b_hi = b.hi;

  uint64_t carry_ll = 0;
  uint64_t product_ll_lo, product_ll_hi;
  _mul64x64(a_lo, b_lo, &product_ll_lo, &product_ll_hi);

  uint64_t product_lh_lo, product_lh_hi;
  _mul64x64(a_lo, b_hi, &product_lh_lo, &product_lh_hi);
  uint64_t carry_lh = _add64x64_carry(product_ll_hi, product_lh_lo, &product_ll_hi);

  uint64_t product_hl_lo, product_hl_hi;
  _mul64x64(a_hi, b_lo, &product_hl_lo, &product_hl_hi);
  uint64_t carry_hl = _add64x64_carry(product_ll_hi, product_hl_lo, &product_ll_hi);

  uint64_t result_hi = product_ll_hi + product_lh_hi + product_hl_hi + carry_lh + carry_hl;

  return (Eurydice_Int128_uint128_t){.hi = result_hi, .lo = product_ll_lo};
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_mul(Eurydice_Int128_int128_t a, Eurydice_Int128_int128_t b) {
  int sign = ((a.hi ^ b.hi) >> 63) & 1;
  Eurydice_Int128_uint128_t abs_a = (a.hi >> 63) ? Eurydice_Int128_u128_neg(*(Eurydice_Int128_uint128_t*)&a) 
                                         : *(Eurydice_Int128_uint128_t*)&a;
  Eurydice_Int128_uint128_t abs_b = (b.hi >> 63) ? Eurydice_Int128_u128_neg(*(Eurydice_Int128_uint128_t*)&b) 
                                         : *(Eurydice_Int128_uint128_t*)&b;
  Eurydice_Int128_uint128_t abs_result = Eurydice_Int128_u128_mul(abs_a, abs_b);
  return sign ? i128_of_u128(Eurydice_Int128_u128_neg(abs_result)) 
              : i128_of_u128(abs_result);
}
static inline int _u128_get_bit(Eurydice_Int128_uint128_t x, int pos) {
  return (pos < 64) ? (x.lo >> pos) & 1 : (x.hi >> (pos - 64)) & 1;
}
static inline 
Eurydice_Int128_uint128_t _u128_set_bit(Eurydice_Int128_uint128_t x, int pos) {
  if (pos < 64) x.lo |= (1ULL << pos);
  else x.hi |= (1ULL << (pos - 64));
  return x;
}
static inline 
Eurydice_Int128_uint128_t _u128_shl(Eurydice_Int128_uint128_t x, int shift) {
  if (shift >= 64) {
    x.hi = x.lo << (shift - 64);
    x.lo = 0;
  } else {
    x.hi = (x.hi << shift) | (x.lo >> (64 - shift));
    x.lo <<= shift;
  }
  return x;
}
static inline 
int _u128_compare(Eurydice_Int128_uint128_t a, Eurydice_Int128_uint128_t b) {
  if (a.hi > b.hi) return 1;
  if (a.hi < b.hi) return -1;
  return (a.lo > b.lo) ? 1 : (a.lo < b.lo) ? -1 : 0;
}
typedef struct { Eurydice_Int128_uint128_t quot; Eurydice_Int128_uint128_t rem; } u128_divmod_t;
static inline 
u128_divmod_t Eurydice_Int128_u128_divmod(Eurydice_Int128_uint128_t n, Eurydice_Int128_uint128_t d) {
  u128_divmod_t result = { {0, 0}, {0, 0} };
  if (d.hi == 0 && d.lo == 0) return result;

  for (int i = 127; i >= 0; i--) {
    result.rem = _u128_shl(result.rem, 1);
    if (_u128_get_bit(n, i)) 
      result.rem = _u128_set_bit(result.rem, 0);
    
    if (_u128_compare(result.rem, d) >= 0) {
      result.rem = Eurydice_Int128_u128_sub(result.rem, d);
      result.quot = _u128_set_bit(result.quot, i);
    }
  }
  return result;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_div(Eurydice_Int128_uint128_t dividend, Eurydice_Int128_uint128_t divisor) {
  return Eurydice_Int128_u128_divmod(dividend, divisor).quot;
}
static inline 
Eurydice_Int128_uint128_t Eurydice_Int128_u128_mod(Eurydice_Int128_uint128_t dividend, Eurydice_Int128_uint128_t divisor) {
  return Eurydice_Int128_u128_divmod(dividend, divisor).rem;
}
static inline 
Eurydice_Int128_int128_t Eurydice_Int128_i128_div(Eurydice_Int128_int128_t a, Eurydice_Int128_int128_t b) {
  int sign = ((a.hi ^ b.hi) >> 63) & 1;
  Eurydice_Int128_uint128_t abs_a = (a.hi >> 63) ? Eurydice_Int128_u128_neg(u128_of_i128(a)) 
                                        : u128_of_i128(a);
  Eurydice_Int128_uint128_t abs_b = (b.hi >> 63) ? Eurydice_Int128_u128_neg(u128_of_i128(b)) 
                                        : u128_of_i128(b);
  Eurydice_Int128_uint128_t abs_quot = Eurydice_Int128_u128_div(abs_a, abs_b);
  Eurydice_Int128_int128_t result = sign ? i128_of_u128(Eurydice_Int128_u128_neg(abs_quot))
                                : i128_of_u128(abs_quot);
  if (a.hi >> 63 && result.hi == 0 && result.lo == 0)
    result = Eurydice_Int128_i128_sub(result, (Eurydice_Int128_int128_t){ .hi = 0, .lo = 1 });
  return result;
}
static inline
Eurydice_Int128_uint128_t Eurydice_Int128_u128_bor(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return (Eurydice_Int128_uint128_t){.hi = lhs.hi | rhs.hi, .lo = lhs.lo | rhs.lo};
}
static inline
Eurydice_Int128_int128_t Eurydice_Int128_i128_bor(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return i128_of_u128(Eurydice_Int128_u128_bor(u128_of_i128(lhs), u128_of_i128(rhs)));
}
static inline
Eurydice_Int128_uint128_t Eurydice_Int128_u128_band(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return (Eurydice_Int128_uint128_t){.hi = lhs.hi & rhs.hi, .lo = lhs.lo & rhs.lo};
}
static inline
Eurydice_Int128_int128_t Eurydice_Int128_i128_band(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return i128_of_u128(Eurydice_Int128_u128_band(u128_of_i128(lhs), u128_of_i128(rhs)));
}
static inline
Eurydice_Int128_uint128_t Eurydice_Int128_u128_bxor(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return (Eurydice_Int128_uint128_t){.hi = lhs.hi ^ rhs.hi, .lo = lhs.lo ^ rhs.lo};
}
static inline
Eurydice_Int128_int128_t Eurydice_Int128_i128_bxor(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return i128_of_u128(Eurydice_Int128_u128_bxor(u128_of_i128(lhs), u128_of_i128(rhs)));
}
static inline
Eurydice_Int128_uint128_t Eurydice_Int128_u128_shl(Eurydice_Int128_uint128_t x, uint32_t shift) {
  if (shift >= 128) {
    return (Eurydice_Int128_uint128_t){.hi = 0, .lo = 0};
  } else if (shift >= 64) {
    return (Eurydice_Int128_uint128_t){.hi = x.lo << (shift - 64), .lo = 0};
  } else {
    return (Eurydice_Int128_uint128_t){.hi = (x.hi << shift) | (x.lo >> (64 - shift)),
                                .lo = x.lo << shift};
  }
}
static inline
Eurydice_Int128_int128_t Eurydice_Int128_i128_shl(Eurydice_Int128_int128_t x, uint32_t shift) {
  return i128_of_u128(Eurydice_Int128_u128_shl(u128_of_i128(x), shift));
}
static inline
Eurydice_Int128_uint128_t Eurydice_Int128_u128_shr(Eurydice_Int128_uint128_t x, uint32_t shift) {
  if (shift >= 128) {
    return (Eurydice_Int128_uint128_t){.hi = 0, .lo = 0};
  } else if (shift >= 64) {
    return (Eurydice_Int128_uint128_t){.hi = 0, .lo = x.hi >> (shift - 64)};
  } else {
    return (Eurydice_Int128_uint128_t){.hi = x.hi >> shift,
                                .lo = (x.lo >> shift) | (x.hi << (64 - shift))};
  }
}
static inline
Eurydice_Int128_int128_t Eurydice_Int128_i128_shr(Eurydice_Int128_int128_t x, uint32_t shift) {
  return i128_of_u128(Eurydice_Int128_u128_shr(u128_of_i128(x), shift));
}
static inline
Eurydice_Int128_uint128_t Eurydice_Int128_u128_bnot(Eurydice_Int128_uint128_t x) {
  return (Eurydice_Int128_uint128_t){.hi = ~x.hi, .lo = ~x.lo};
}
static inline
Eurydice_Int128_int128_t Eurydice_Int128_i128_bnot(Eurydice_Int128_int128_t x) {
  return i128_of_u128(Eurydice_Int128_u128_bnot(u128_of_i128(x)));
}
static inline
bool Eurydice_Int128_u128_lt(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return _u128_compare(lhs, rhs) < 0;
}
static inline
bool Eurydice_Int128_i128_lt(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return _u128_compare(u128_of_i128(lhs), u128_of_i128(rhs)) < 0;
}
static inline
bool Eurydice_Int128_u128_gt(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return _u128_compare(lhs, rhs) > 0;
}
static inline
bool Eurydice_Int128_i128_gt(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return _u128_compare(u128_of_i128(lhs), u128_of_i128(rhs)) > 0;
}
static inline
bool Eurydice_Int128_u128_lte(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return _u128_compare(lhs, rhs) <= 0;
}
static inline
bool Eurydice_Int128_i128_lte(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return _u128_compare(u128_of_i128(lhs), u128_of_i128(rhs)) <= 0;
}
static inline
bool Eurydice_Int128_u128_gte(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return _u128_compare(lhs, rhs) >= 0;
}
static inline
bool Eurydice_Int128_i128_gte(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return _u128_compare(u128_of_i128(lhs), u128_of_i128(rhs)) >= 0;
}
static inline
bool Eurydice_Int128_u128_neq(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return !Eurydice_Int128_u128_eq(lhs, rhs);
}
static inline
bool Eurydice_Int128_i128_neq(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return !Eurydice_Int128_i128_eq(lhs, rhs);
}

#endif // HAS_INT128
