/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "array.h"

array_Foo array_mk_foo(void)
{
  uint32_t x[2U] = { 0U };
  uint32_t y[2U] = { 1U, 1U };
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_x[2U];
  memcpy(copy_of_x, x, (size_t)2U * sizeof (uint32_t));
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_y[2U];
  memcpy(copy_of_y, y, (size_t)2U * sizeof (uint32_t));
  array_Foo lit;
  memcpy(lit.x, copy_of_x, (size_t)2U * sizeof (uint32_t));
  memcpy(lit.y, copy_of_y, (size_t)2U * sizeof (uint32_t));
  return lit;
}

array_Foo array_mk_foo2(void)
{
  return array_mk_foo();
}

void array_mut_array(uint32_t x[2U])
{
  x[0U] = 1U;
}

void array_mut_foo(array_Foo f)
{
  f.x[0U] = 1U;
  uint32_t copy[2U];
  memcpy(copy, f.y, (size_t)2U * sizeof (uint32_t));
  copy[0U] = 0U;
}

/**
A monomorphic instance of array.mk_incr.closure
with const generics
- K= 10
*/
uint32_t array_mk_incr_closure_95(size_t i)
{
  return (uint32_t)i;
}

/**
A monomorphic instance of array.mk_incr
with const generics
- K= 10
*/
void array_mk_incr_95(uint32_t ret[10U])
{
  KRML_MAYBE_FOR10(i, (size_t)0U, (size_t)10U, (size_t)1U, ret[i] = (uint32_t)i;);
}

typedef struct _uint32_t__x2_s
{
  uint32_t *fst;
  uint32_t *snd;
}
_uint32_t__x2;

void array_main(void)
{
  /* XXX1 */
  array_Foo uu____0 = array_mk_foo2();
  uint32_t x[2U];
  memcpy(x, uu____0.x, (size_t)2U * sizeof (uint32_t));
  uint32_t y[2U];
  memcpy(y, uu____0.y, (size_t)2U * sizeof (uint32_t));
  uint32_t unsigned0 = 0U;
  uint32_t uu____1[2U];
  memcpy(uu____1, x, (size_t)2U * sizeof (uint32_t));
  array_mut_array(uu____1);
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_x[2U];
  /* XXX2 */
  memcpy(copy_of_x, x, (size_t)2U * sizeof (uint32_t));
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_y[2U];
  memcpy(copy_of_y, y, (size_t)2U * sizeof (uint32_t));
  array_Foo lit;
  memcpy(lit.x, copy_of_x, (size_t)2U * sizeof (uint32_t));
  memcpy(lit.y, copy_of_y, (size_t)2U * sizeof (uint32_t));
  array_mut_foo(lit);
  _uint32_t__x2 uu____4;
  uu____4.fst = x;
  uu____4.snd = &unsigned0;
  /* XXX3 */
  uint32_t *left_val = uu____4.fst;
  uint32_t *right_val0 = uu____4.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val0[0U], "panic!");
  uint32_t a[10U];
  array_mk_incr_95(a);
  _uint32_t__x2 uu____5;
  uu____5.fst = &a[9U];
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 9U;
  uu____5.snd = &lvalue;
  uint32_t *left_val0 = uu____5.fst;
  uint32_t *right_val = uu____5.snd;
  EURYDICE_ASSERT(left_val0[0U] == right_val[0U], "panic!");
}

