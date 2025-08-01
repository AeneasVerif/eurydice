/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "array.h"

array_Foo array_mk_foo(void)
{
  uint32_t x[2U] = { 0U };
  uint32_t y[2U];
  for (uint32_t _i = 0U; _i < (size_t)2U; ++_i)
    y[_i] = 1U;
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
This function found in impl {core::ops::function::FnMut<(usize), u32> for array::mk_incr::closure<K>}
*/
/**
A monomorphic instance of array.mk_incr.call_mut_e2
with const generics
- K= 10
*/
uint32_t array_mk_incr_call_mut_e2_95(void **_, size_t tupled_args)
{
  size_t i = tupled_args;
  return (uint32_t)i;
}

/**
This function found in impl {core::ops::function::FnOnce<(usize), u32> for array::mk_incr::closure<K>}
*/
/**
A monomorphic instance of array.mk_incr.call_once_b7
with const generics
- K= 10
*/
uint32_t array_mk_incr_call_once_b7_95(size_t _)
{
  /* original Rust expression is not an lvalue in C */
  void *lvalue = (void *)0U;
  return array_mk_incr_call_mut_e2_95(&lvalue, _);
}

/**
A monomorphic instance of array.mk_incr
with const generics
- K= 10
*/
void array_mk_incr_95(uint32_t ret[10U])
{
  KRML_MAYBE_FOR10(i,
    (size_t)0U,
    (size_t)10U,
    (size_t)1U,
    /* original Rust expression is not an lvalue in C */
    void *lvalue = (void *)0U;
    ret[i] = array_mk_incr_call_mut_e2_95(&lvalue, i););
}

/**
This function found in impl {core::ops::function::FnMut<(usize), u32> for array::mk_incr2::closure<0, K>}
*/
/**
A monomorphic instance of array.mk_incr2.call_mut_eb
with const generics
- K= 10
*/
uint32_t array_mk_incr2_call_mut_eb_95(uint32_t **_, size_t tupled_args)
{
  size_t i = tupled_args;
  return (uint32_t)i + _[0U][0U];
}

/**
This function found in impl {core::ops::function::FnOnce<(usize), u32> for array::mk_incr2::closure<0, K>}
*/
/**
A monomorphic instance of array.mk_incr2.call_once_ad
with const generics
- K= 10
*/
uint32_t array_mk_incr2_call_once_ad_95(uint32_t *_, size_t _0)
{
  return array_mk_incr2_call_mut_eb_95(&_, _0);
}

/**
A monomorphic instance of array.mk_incr2
with const generics
- K= 10
*/
void array_mk_incr2_95(uint32_t ret[10U])
{
  uint32_t j = 1U;
  uint32_t ret0[10U];
  KRML_MAYBE_FOR10(i,
    (size_t)0U,
    (size_t)10U,
    (size_t)1U,
    /* original Rust expression is not an lvalue in C */
    uint32_t *lvalue = &j;
    ret0[i] = array_mk_incr2_call_mut_eb_95(&lvalue, i););
  memcpy(ret, ret0, (size_t)10U * sizeof (uint32_t));
}

/**
This function found in impl {core::ops::function::FnMut<(u32), u16> for array::plus_one::closure<K>}
*/
/**
A monomorphic instance of array.plus_one.call_mut_8d
with const generics
- K= 1
*/
uint16_t array_plus_one_call_mut_8d_74(void **_, uint32_t tupled_args)
{
  uint32_t x = tupled_args;
  return (uint16_t)(x + 1U);
}

/**
This function found in impl {core::ops::function::FnOnce<(u32), u16> for array::plus_one::closure<K>}
*/
/**
A monomorphic instance of array.plus_one.call_once_36
with const generics
- K= 1
*/
uint16_t array_plus_one_call_once_36_74(uint32_t _)
{
  /* original Rust expression is not an lvalue in C */
  void *lvalue = (void *)0U;
  return array_plus_one_call_mut_8d_74(&lvalue, _);
}

/**
A monomorphic instance of array.plus_one
with const generics
- K= 1
*/
void array_plus_one_74(uint32_t x[1U], uint16_t ret[1U])
{
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_x[1U];
  memcpy(copy_of_x, x, (size_t)1U * sizeof (uint32_t));
  uint16_t ret0[1U];
  {
    /* original Rust expression is not an lvalue in C */
    void *lvalue = (void *)0U;
    ret0[0U] = array_plus_one_call_mut_8d_74(&lvalue, copy_of_x[0U]);
  }
  memcpy(ret, ret0, (size_t)1U * sizeof (uint16_t));
}

/**
This function found in impl {core::ops::function::FnMut<(usize), usize> for array::nested_from_fn::closure::closure<0, K>}
*/
/**
A monomorphic instance of array.nested_from_fn.closure.call_mut_74
with const generics
- K= 4
*/
size_t array_nested_from_fn_closure_call_mut_74_ac(size_t **_, size_t tupled_args)
{
  size_t i = tupled_args;
  return i + _[0U][0U];
}

/**
This function found in impl {core::ops::function::FnOnce<(usize), usize> for array::nested_from_fn::closure::closure<0, K>}
*/
/**
A monomorphic instance of array.nested_from_fn.closure.call_once_4d
with const generics
- K= 4
*/
size_t array_nested_from_fn_closure_call_once_4d_ac(size_t *_, size_t _0)
{
  return array_nested_from_fn_closure_call_mut_74_ac(&_, _0);
}

/**
This function found in impl {core::ops::function::FnMut<(usize), @Array<usize, K>> for array::nested_from_fn::closure<K>}
*/
/**
A monomorphic instance of array.nested_from_fn.call_mut_af
with const generics
- K= 4
*/
void array_nested_from_fn_call_mut_af_ac(void **_, size_t tupled_args, size_t ret[4U])
{
  size_t j = tupled_args;
  size_t ret0[4U];
  KRML_MAYBE_FOR4(i,
    (size_t)0U,
    (size_t)4U,
    (size_t)1U,
    /* original Rust expression is not an lvalue in C */
    size_t *lvalue = &j;
    ret0[i] = array_nested_from_fn_closure_call_mut_74_ac(&lvalue, i););
  memcpy(ret, ret0, (size_t)4U * sizeof (size_t));
}

/**
This function found in impl {core::ops::function::FnOnce<(usize), @Array<usize, K>> for array::nested_from_fn::closure<K>}
*/
/**
A monomorphic instance of array.nested_from_fn.call_once_f6
with const generics
- K= 4
*/
void array_nested_from_fn_call_once_f6_ac(size_t _, size_t ret[4U])
{
  /* original Rust expression is not an lvalue in C */
  void *lvalue = (void *)0U;
  array_nested_from_fn_call_mut_af_ac(&lvalue, _, ret);
}

/**
A monomorphic instance of array.nested_from_fn
with const generics
- K= 4
*/
void array_nested_from_fn_ac(size_t ret[4U][4U])
{
  KRML_MAYBE_FOR4(i,
    (size_t)0U,
    (size_t)4U,
    (size_t)1U,
    /* original Rust expression is not an lvalue in C */
    void *lvalue = (void *)0U;
    array_nested_from_fn_call_mut_af_ac(&lvalue, i, ret[i]););
}

typedef struct _uint32_t__x2_s
{
  uint32_t *fst;
  uint32_t *snd;
}
_uint32_t__x2;

typedef struct _uint16_t__x2_s
{
  uint16_t *fst;
  uint16_t *snd;
}
_uint16_t__x2;

typedef struct _size_t__x2_s
{
  size_t *fst;
  size_t *snd;
}
_size_t__x2;

void array_main(void)
{
  /* XXX1 */
  array_Foo uu____0 = array_mk_foo2();
  uint32_t x[2U];
  memcpy(x, uu____0.x, (size_t)2U * sizeof (uint32_t));
  /* XXX2 */
  uint32_t y[2U];
  memcpy(y, uu____0.y, (size_t)2U * sizeof (uint32_t));
  uint32_t unsigned0 = 0U;
  uint32_t uu____1[2U];
  memcpy(uu____1, x, (size_t)2U * sizeof (uint32_t));
  array_mut_array(uu____1);
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_x[2U];
  memcpy(copy_of_x, x, (size_t)2U * sizeof (uint32_t));
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_y[2U];
  memcpy(copy_of_y, y, (size_t)2U * sizeof (uint32_t));
  array_Foo lit;
  memcpy(lit.x, copy_of_x, (size_t)2U * sizeof (uint32_t));
  memcpy(lit.y, copy_of_y, (size_t)2U * sizeof (uint32_t));
  array_mut_foo(lit);
  _uint32_t__x2 uu____4 = { .fst = x, .snd = &unsigned0 };
  EURYDICE_ASSERT(uu____4.fst[0U] == uu____4.snd[0U], "panic!");
  uint32_t a[10U];
  array_mk_incr_95(a);
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue0 = 9U;
  _uint32_t__x2 uu____5 = { .fst = &a[9U], .snd = &lvalue0 };
  EURYDICE_ASSERT(uu____5.fst[0U] == uu____5.snd[0U], "panic!");
  uint32_t a0[10U];
  array_mk_incr2_95(a0);
  uint32_t expected = 10U;
  _uint32_t__x2 uu____6 = { .fst = &a0[9U], .snd = &expected };
  EURYDICE_ASSERT(uu____6.fst[0U] == uu____6.snd[0U], "panic!");
  uint16_t a1[1U];
  uint32_t buf[1U] = { 0U };
  array_plus_one_74(buf, a1);
  /* original Rust expression is not an lvalue in C */
  uint16_t lvalue1 = 1U;
  _uint16_t__x2 uu____7 = { .fst = a1, .snd = &lvalue1 };
  EURYDICE_ASSERT(uu____7.fst[0U] == uu____7.snd[0U], "panic!");
  /* XXX5 */
  size_t a2[4U][4U];
  array_nested_from_fn_ac(a2);
  /* original Rust expression is not an lvalue in C */
  size_t lvalue = (size_t)6U;
  _size_t__x2 uu____8 = { .fst = &a2[3U][3U], .snd = &lvalue };
  EURYDICE_ASSERT(uu____8.fst[0U] == uu____8.snd[0U], "panic!");
}

