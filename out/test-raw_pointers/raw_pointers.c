/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "raw_pointers.h"

typedef struct _uint8_t__x2_s
{
  uint8_t *fst;
  uint8_t *snd;
}
_uint8_t__x2;

void raw_pointers_main(void)
{
  uint8_t x = 0U;
  uint8_t *px = (uint8_t *)&x;
  /* original Rust expression is not an lvalue in C */
  uint8_t lvalue = 0U;
  _uint8_t__x2 uu____0 = { .fst = &lvalue, .snd = px };
  uint8_t *left_val = uu____0.fst;
  uint8_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

