#include "__NAME__.h"
#include "eurydice_glue.h"

Eurydice_int128_t Eurydice_i128_from_bits(uint64_t hi, uint64_t lo) {
  return ((Eurydice_int128_t)hi << 64) | lo;
}
Eurydice_uint128_t Eurydice_u128_from_bits(uint64_t hi, uint64_t lo) {
  return ((Eurydice_uint128_t)hi << 64) | lo;
}
bool Eurydice_i128_eq(Eurydice_int128_t x, Eurydice_int128_t y) {
  return x == y;
}
bool Eurydice_u128_eq(Eurydice_uint128_t x, Eurydice_uint128_t y) {
  return x == y;
}

Eurydice_int128_t Eurydice_i128_add(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs + rhs;
}
Eurydice_uint128_t Eurydice_u128_add(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs + rhs;
}
Eurydice_int128_t Eurydice_i128_sub(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs - rhs;
}
Eurydice_uint128_t Eurydice_u128_sub(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs - rhs;
}
Eurydice_int128_t Eurydice_i128_mul(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs * rhs;
}
Eurydice_uint128_t Eurydice_u128_mul(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs * rhs;
}
Eurydice_int128_t Eurydice_i128_div(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs / rhs;
}
Eurydice_uint128_t Eurydice_u128_div(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs / rhs;
}
Eurydice_int128_t Eurydice_i128_mod(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs % rhs;
}
Eurydice_uint128_t Eurydice_u128_mod(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs % rhs;
}
Eurydice_int128_t Eurydice_i128_bor(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs | rhs;
}
Eurydice_uint128_t Eurydice_u128_bor(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs | rhs;
}
Eurydice_int128_t Eurydice_i128_band(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs & rhs;
}
Eurydice_uint128_t Eurydice_u128_band(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs & rhs;
}
Eurydice_int128_t Eurydice_i128_bxor(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs ^ rhs;
}
Eurydice_uint128_t Eurydice_u128_bxor(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs ^ rhs;
}
Eurydice_int128_t Eurydice_i128_shl(Eurydice_int128_t lhs, uint32_t rhs) {
  return lhs << rhs;
}
Eurydice_uint128_t Eurydice_u128_shl(Eurydice_uint128_t lhs, uint32_t rhs) {
  return lhs << rhs;
}
Eurydice_int128_t Eurydice_i128_shr(Eurydice_int128_t lhs, uint32_t rhs) {
  return lhs >> rhs;
}
Eurydice_uint128_t Eurydice_u128_shr(Eurydice_uint128_t lhs, uint32_t rhs) {
  return lhs >> rhs;
}
Eurydice_int128_t Eurydice_i128_bnot(Eurydice_int128_t x) {
  return ~x;
}
Eurydice_uint128_t Eurydice_u128_bnot(Eurydice_uint128_t x) {
  return ~x;
}
bool Eurydice_i128_lt(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs < rhs;
}
bool Eurydice_u128_lt(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs < rhs;
}
bool Eurydice_i128_gt(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs > rhs;
}
bool Eurydice_u128_gt(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs > rhs;
}
bool Eurydice_i128_lte(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs <= rhs;
}
bool Eurydice_u128_lte(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs <= rhs;
}
bool Eurydice_i128_gte(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs >= rhs;
}
bool Eurydice_u128_gte(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs >= rhs;
}
bool Eurydice_i128_neq(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs != rhs;
}
bool Eurydice_u128_neq(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs != rhs;
}

int main() {
  __NAME___main();
  return 0;
}
