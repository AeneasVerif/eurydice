#pragma once

// New types & definitions for 128-bit integers

// #if defined(__SIZEOF_INT128__)
// #define HAS_INT128 1
// #endif

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
// These macro reuse functions over u128 to implement fuctions over i128
#define i128_reuse_u128_impl1(f,x) i128_of_u128(f(u128_of_i128(x)))
#define i128_reuse_u128_impl2(f,x,y) i128_of_u128(f(u128_of_i128(x),u128_of_i128(y)))
static const Eurydice_Int128_int128_t neg_one = {
  .hi = 0xffffffff,
  .lo = 0xffffffff
};
static inline bool Eurydice_Int128_i128_eq(Eurydice_Int128_int128_t x, Eurydice_Int128_int128_t y) {
  return x.hi == y.hi && x.lo == y.lo;
}
static inline bool Eurydice_Int128_u128_eq(Eurydice_Int128_uint128_t x, Eurydice_Int128_uint128_t y) {
  return x.hi == y.hi && x.lo == y.lo;
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_add(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  uint64_t lo = lhs.lo + rhs.lo;
  uint64_t carry = (lo < lhs.lo) ? 1 : 0;
  return (Eurydice_Int128_uint128_t){.hi = lhs.hi + rhs.hi + carry, .lo = lo};
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_add(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return i128_reuse_u128_impl2(Eurydice_Int128_u128_add,lhs,rhs);
}
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_sub(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  uint64_t lo = lhs.lo - rhs.lo;
  uint64_t borrow = (lhs.lo < rhs.lo) ? 1 : 0;
  return (Eurydice_Int128_uint128_t){.hi = lhs.hi - rhs.hi - borrow, .lo = lo};
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_sub(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return i128_reuse_u128_impl2(Eurydice_Int128_u128_sub,lhs,rhs);
} 
static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_neg(Eurydice_Int128_uint128_t x) {
  Eurydice_Int128_uint128_t result;
  result.lo = ~x.lo + 1;
  result.hi = ~x.hi + (result.lo == 0 ? 1 : 0);
  return result;
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_neg(Eurydice_Int128_int128_t x) {
  return i128_reuse_u128_impl1(Eurydice_Int128_u128_neg,x);
}

static inline bool Eurydice_Int128_i128_is_neg(Eurydice_Int128_int128_t x) {
  return (x.hi >> 63 & 1);
}

static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_abs(Eurydice_Int128_int128_t x) {
  return (Eurydice_Int128_i128_is_neg(x) ? Eurydice_Int128_i128_neg(x) : x);
}

/* 
  a = a_hi * 2^64 + a_low
  b = b_hi * 2^64 + b_low
    a * b 
  = a_hi * b_hi * 2^128 + (a_hi * b_lo + b_hi * a_lo) * 2^64 + a_lo * b_lo
  res_hi = a_hi * b_lo + b_hi * a_lo + a_lo * b_lo >> 64
  res_lo = a_lo * b_lo
*/

static inline Eurydice_Int128_uint128_t Eurydice_Int128_u128_mul(Eurydice_Int128_uint128_t a, Eurydice_Int128_uint128_t b) {
  uint64_t a_lo = a.lo, a_hi = a.hi;
  uint64_t b_lo = b.lo, b_hi = b.hi;

  uint64_t result_lo = a_lo * b_lo;
  /*
    a_lo = a_lh * 2^32 + a_ll
    b_lo = b_lh * 2^32 + b_ll
    a_lo * b_lo = a_lh * b_lh * 2^64 + (a_ll * b_lh + b_ll * a_lh) * 2^32 + a_ll * b_ll
    carry_ll = a_lh * b_lh + (a_ll * b_lh + b_ll * a_lh) >> 32
  */
  uint64_t a_ll = a_lo & 0xffffffff, a_lh = a_lo >> 32;
  uint64_t b_ll = b_lo & 0xffffffff, b_lh = b_lo >> 32;
  uint64_t carry_ll = a_lh * b_lh + ((a_ll * b_lh + b_ll * a_lh) >> 32);
  uint64_t result_hi = a_hi * b_lo + b_hi * a_lo + carry_ll;
  return (Eurydice_Int128_uint128_t){.hi = result_hi, .lo = result_lo};
}
static inline Eurydice_Int128_int128_t Eurydice_Int128_i128_mul(Eurydice_Int128_int128_t a, Eurydice_Int128_int128_t b) {
  int sign = ((a.hi ^ b.hi) >> 63) & 1;
  Eurydice_Int128_uint128_t abs_a = u128_of_i128(Eurydice_Int128_i128_abs(a));
  Eurydice_Int128_uint128_t abs_b = u128_of_i128(Eurydice_Int128_i128_abs(b));
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
static inline 
int _i128_compare(Eurydice_Int128_int128_t a, Eurydice_Int128_int128_t b) {
  bool a_sign = Eurydice_Int128_i128_is_neg(a);
  bool b_sign = Eurydice_Int128_i128_is_neg(b);
  if (a_sign != b_sign) return (a_sign ? -1 : 1);
  return _u128_compare(u128_of_i128(a),u128_of_i128(b));
}
typedef struct { Eurydice_Int128_uint128_t quot; Eurydice_Int128_uint128_t rem; } u128_divmod_t;
static inline 
u128_divmod_t Eurydice_Int128_u128_divmod(Eurydice_Int128_uint128_t n, Eurydice_Int128_uint128_t d) {
  u128_divmod_t result = { {0, 0}, {0, 0} };
  if (d.hi == 0 && d.lo == 0) assert(false);

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
  Eurydice_Int128_uint128_t abs_a = u128_of_i128(Eurydice_Int128_i128_abs(a));
  Eurydice_Int128_uint128_t abs_b = u128_of_i128(Eurydice_Int128_i128_abs(b));
  Eurydice_Int128_uint128_t abs_quot = Eurydice_Int128_u128_div(abs_a, abs_b);
  Eurydice_Int128_int128_t result = sign ? i128_of_u128(Eurydice_Int128_u128_neg(abs_quot))
                                : i128_of_u128(abs_quot);
  return result;
}

Eurydice_Int128_int128_t Eurydice_Int128_i128_mod(Eurydice_Int128_int128_t a, Eurydice_Int128_int128_t b) {
  bool sign = Eurydice_Int128_i128_is_neg(a);
  Eurydice_Int128_uint128_t abs_a = u128_of_i128(Eurydice_Int128_i128_abs(a));
  Eurydice_Int128_uint128_t abs_b = u128_of_i128(Eurydice_Int128_i128_abs(b));
  Eurydice_Int128_uint128_t abs_rem = Eurydice_Int128_u128_mod(abs_a, abs_b);
  Eurydice_Int128_int128_t result = sign ? i128_of_u128(Eurydice_Int128_u128_neg(abs_rem))
                                : i128_of_u128(abs_rem);
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
  } else if (shift > 0){
    return (Eurydice_Int128_uint128_t){.hi = x.hi >> shift,
                                .lo = (x.lo >> shift) | (x.hi << (64 - shift))};
  } else return x;
}
static inline
Eurydice_Int128_int128_t Eurydice_Int128_i128_shr(Eurydice_Int128_int128_t x, uint32_t shift) {
  Eurydice_Int128_int128_t result = i128_of_u128(Eurydice_Int128_u128_shr(u128_of_i128(x), shift));
  if (Eurydice_Int128_i128_is_neg(x)){
    Eurydice_Int128_uint128_t mask = neg_one;
    if (shift <= 128){
      mask = Eurydice_Int128_i128_shl(mask, 128 - shift);
    }
    return Eurydice_Int128_i128_bor(result, mask);
  }
  return result;
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
  return _i128_compare(lhs, rhs) < 0;
}
static inline
bool Eurydice_Int128_u128_gt(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return _u128_compare(lhs, rhs) > 0;
}
static inline
bool Eurydice_Int128_i128_gt(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return _i128_compare(lhs, rhs) > 0;
}
static inline
bool Eurydice_Int128_u128_lte(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return _u128_compare(lhs, rhs) <= 0;
}
static inline
bool Eurydice_Int128_i128_lte(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return _i128_compare(lhs, rhs) <= 0;
}
static inline
bool Eurydice_Int128_u128_gte(Eurydice_Int128_uint128_t lhs, Eurydice_Int128_uint128_t rhs) {
  return _u128_compare(lhs, rhs) >= 0;
}
static inline
bool Eurydice_Int128_i128_gte(Eurydice_Int128_int128_t lhs, Eurydice_Int128_int128_t rhs) {
  return _i128_compare(lhs, rhs) >= 0;
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
