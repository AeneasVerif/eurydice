/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "where_clauses_fncg.h"

/**
This function found in impl {(where_clauses_fncg::Foo<K> for u64)}
*/
/**
A monomorphic instance of where_clauses_fncg.bar_ea
with const generics
- K= 8
- L= 4
*/
uint64_t where_clauses_fncg_bar_ea_7b(uint8_t x[8U][4U], uint8_t _[4U][8U])
{
  return (uint64_t)x[0U][0U];
}

/**
A monomorphic instance of where_clauses_fncg.f
with types uint64_t
with const generics
- K= 6
- L= 8
- M= 10
*/
uint64_t where_clauses_fncg_f_43(void)
{
  uint8_t buf0[8U][4U] = { { 0U } };
  uint8_t buf[4U][8U] = { { 0U } };
  return where_clauses_fncg_bar_ea_7b(buf0, buf);
}

typedef struct _uint64_t__x2_s
{
  uint64_t *fst;
  uint64_t *snd;
}
_uint64_t__x2;

void where_clauses_fncg_main(void)
{
  uint64_t r = where_clauses_fncg_f_43();
  uint64_t expected = 0ULL;
  _uint64_t__x2 uu____0 = { .fst = &r, .snd = &expected };
  uint64_t *left_val = uu____0.fst;
  uint64_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

