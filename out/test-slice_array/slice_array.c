/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "slice_array.h"

typedef struct _uint8_t__x2_s
{
  uint8_t *fst;
  uint8_t *snd;
}
_uint8_t__x2;

void slice_array_f1(void)
{
  uint8_t x[4U][4U] = { { 0U } };
  Eurydice_slice
  y0 =
    Eurydice_slice_split_at_mut(Eurydice_array_to_slice((size_t)4U, x, uint8_t [4U]),
      (size_t)2U,
      uint8_t [4U],
      Eurydice_slice_uint8_t_4size_t__x2).fst;
  Eurydice_slice_index(y0, (size_t)0U, uint8_t [4U], uint8_t (*)[4U])[0U] = 1U;
  uint8_t actual = x[0U][0U];
  uint8_t expected = 1U;
  _uint8_t__x2 uu____0 = { CFIELD(.fst, &actual), CFIELD(.snd, &expected) };
  uint8_t *left_val = uu____0.fst;
  uint8_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void slice_array_f2(void)
{
  uint8_t x[4U][4U] = { { 0U } };
  Eurydice_slice
  y0 =
    Eurydice_slice_split_at_mut(Eurydice_array_to_slice((size_t)4U, x, uint8_t [4U]),
      (size_t)2U,
      uint8_t [4U],
      Eurydice_slice_uint8_t_4size_t__x2).fst;
  uint8_t z[4U];
  memcpy(z,
    Eurydice_slice_index(y0, (size_t)0U, uint8_t [4U], uint8_t (*)[4U]),
    (size_t)4U * sizeof (uint8_t));
  z[0U] = 1U;
  uint8_t actual = x[0U][0U];
  uint8_t expected = 0U;
  _uint8_t__x2 uu____0 = { CFIELD(.fst, &actual), CFIELD(.snd, &expected) };
  uint8_t *left_val = uu____0.fst;
  uint8_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void slice_array_main(void)
{
  slice_array_f1();
  slice_array_f2();
}

