/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "issue_123.h"

int32_t issue_123_fun(issue_123_E e)
{
  ptrdiff_t uu____0;
  if (e == issue_123_E_One)
  {
    uu____0 = (ptrdiff_t)1;
  }
  else
  {
    uu____0 = (ptrdiff_t)5;
  }
  uint8_t uu____1 = (uint8_t)uu____0;
  EURYDICE_ASSERT(!!(uu____1 >= 1U && uu____1 <= 5U), "assert failure");
  return (int32_t)uu____0;
}

typedef struct _ptrdiff_t__x2_s
{
  ptrdiff_t *fst;
  ptrdiff_t *snd;
}
_ptrdiff_t__x2;

typedef struct _int32_t__x2_s
{
  int32_t *fst;
  int32_t *snd;
}
_int32_t__x2;

void issue_123_main(void)
{
  /* original Rust expression is not an lvalue in C */
  ptrdiff_t lvalue0 = (ptrdiff_t)((ptrdiff_t)-1 + (ptrdiff_t)0);
  /* original Rust expression is not an lvalue in C */
  ptrdiff_t lvalue1 = (ptrdiff_t)-1;
  _ptrdiff_t__x2 uu____0 = { CFIELD(.fst, &lvalue0), CFIELD(.snd, &lvalue1) };
  ptrdiff_t *left_val0 = uu____0.fst;
  ptrdiff_t *right_val0 = uu____0.snd;
  EURYDICE_ASSERT(left_val0[0U] == right_val0[0U], "panic!");
  /* original Rust expression is not an lvalue in C */
  int32_t lvalue2 = issue_123_fun(issue_123_E_One);
  /* original Rust expression is not an lvalue in C */
  int32_t lvalue = (int32_t)1;
  _int32_t__x2 uu____1 = { CFIELD(.fst, &lvalue2), CFIELD(.snd, &lvalue) };
  int32_t *left_val = uu____1.fst;
  int32_t *right_val = uu____1.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

/**
This function found in impl {(core::cmp::PartialEq<issue_123::E2> for issue_123::E2)#1}
*/
inline bool issue_123_eq_87(issue_123_E2 *self, issue_123_E2 *other)
{
  ptrdiff_t __self_discr;
  if (self[0U] == issue_123_E2_C1)
  {
    __self_discr = (ptrdiff_t)255;
  }
  else
  {
    __self_discr = (ptrdiff_t)-1;
  }
  ptrdiff_t __arg1_discr;
  if (other[0U] == issue_123_E2_C1)
  {
    __arg1_discr = (ptrdiff_t)255;
  }
  else
  {
    __arg1_discr = (ptrdiff_t)-1;
  }
  return __self_discr == __arg1_discr;
}

