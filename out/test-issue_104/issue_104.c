/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "issue_104.h"

/**
A monomorphic instance of issue_104.sth
with types issue_104_S
with const generics

*/
uint8_t issue_104_sth_50(void)
{
  return ISSUE_104___ISSUE_104__FUN_FOR_ISSUE_104__S___VAL;
}

uint8_t issue_104_call(void)
{
  return issue_104_sth_50();
}

typedef struct _uint8_t__x2_s
{
  uint8_t *fst;
  uint8_t *snd;
}
_uint8_t__x2;

void issue_104_main(void)
{
  /* original Rust expression is not an lvalue in C */
  uint8_t lvalue0 = issue_104_call();
  /* original Rust expression is not an lvalue in C */
  uint8_t lvalue = 5U;
  _uint8_t__x2 uu____0 = { CFIELD(.fst, &lvalue0), CFIELD(.snd, &lvalue) };
  uint8_t *left_val = uu____0.fst;
  uint8_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

