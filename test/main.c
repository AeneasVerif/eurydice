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

int main() {
  __NAME___main();
  return 0;
}
