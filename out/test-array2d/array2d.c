/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "array2d.h"

bool array2d_f(uint32_t x[4U][2U])
{
  x[0U][0U] = 1U;
  x[0U][1U] = 2U;
  uint32_t y[4U][2U] = { { 1U, 2U }, { 3U, 4U }, { 1U, 2U }, { 3U, 4U } };
  return
    core_array_equality___core__cmp__PartialEq__Array_U__N___for__Array_T__N____eq((size_t)4U,
      x,
      y,
      uint32_t [2U],
      uint32_t [2U],
      bool);
}

typedef struct const_bool__x2_s
{
  const bool *fst;
  const bool *snd;
}
const_bool__x2;

void array2d_main(void)
{
  uint32_t y[4U][2U];
  KRML_MAYBE_FOR4(i,
    (size_t)0U,
    (size_t)4U,
    (size_t)1U,
    y[i][0U] = 1U;
    y[i][1U] = 2U;);
  y[1U][0U] = 3U;
  y[1U][1U] = 4U;
  y[3U][0U] = 3U;
  y[3U][1U] = 4U;
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_y[4U][2U];
  memcpy(copy_of_y, y, (size_t)4U * sizeof (uint32_t [2U]));
  bool actual = array2d_f(copy_of_y);
  bool expected = true;
  const_bool__x2 uu____1 = { CFIELD(.fst, &actual), CFIELD(.snd, &expected) };
  const bool *left_val = uu____1.fst;
  const bool *right_val = uu____1.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

