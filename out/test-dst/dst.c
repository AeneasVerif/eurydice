/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "dst.h"

Eurydice_dst_31 dst_alloc(void)
{
  Eurydice_dst_31 lit0;
  dst_S_dd lit;
  lit.foo = 0U;
  uint32_t repeat_expression[4U] = { 0U };
  memcpy(lit.my_data, repeat_expression, (size_t)4U * sizeof (uint32_t));
  lit0.ptr = (dst_T *)Eurydice_box_new(lit, dst_S_dd, dst_S_dd *);
  lit0.len = (size_t)4U;
  return lit0;
}

Eurydice_dst_f1 dst_alloc3(void)
{
  Eurydice_dst_f1 lit0;
  dst_S_a4 lit;
  lit.foo = 0U;
  uint32_t repeat_expression[4U][3U] = { { 0U } };
  memcpy(lit.my_data, repeat_expression, (size_t)4U * sizeof (uint32_t [3U]));
  lit0.ptr = (dst_T3 *)Eurydice_box_new(lit, dst_S_a4, dst_S_a4 *);
  lit0.len = (size_t)4U;
  return lit0;
}

typedef struct _uint32_t__x2_s
{
  uint32_t *fst;
  uint32_t *snd;
}
_uint32_t__x2;

void dst_check_regular_field(Eurydice_dst_31 x)
{
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 0U;
  _uint32_t__x2 uu____0 = { .fst = &x.ptr->foo, .snd = &lvalue };
  uint32_t *left_val = uu____0.fst;
  uint32_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void dst_check_regular_field_ref(Eurydice_dst_31 x)
{
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 0U;
  _uint32_t__x2 uu____0 = { .fst = &x.ptr->foo, .snd = &lvalue };
  uint32_t *left_val = uu____0.fst;
  uint32_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void dst_check_var_field(Eurydice_dst_31 x)
{
  size_t uu____0 = (size_t)0U;
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 0U;
  _uint32_t__x2
  uu____1 =
    {
      .fst = &Eurydice_slice_index(Eurydice_slice_of_dst(&x.ptr->my_data,
          x.len,
          uint32_t,
          Eurydice_slice),
        uu____0,
        uint32_t,
        uint32_t *),
      .snd = &lvalue
    };
  uint32_t *left_val = uu____1.fst;
  uint32_t *right_val = uu____1.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void dst_check_var_field_ref(Eurydice_dst_31 x)
{
  size_t uu____0 = (size_t)0U;
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 0U;
  _uint32_t__x2
  uu____1 =
    {
      .fst = &Eurydice_slice_index(Eurydice_slice_of_dst(&x.ptr->my_data,
          x.len,
          uint32_t,
          Eurydice_slice),
        uu____0,
        uint32_t,
        uint32_t *),
      .snd = &lvalue
    };
  uint32_t *left_val = uu____1.fst;
  uint32_t *right_val = uu____1.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void dst_check_var_field_ref3(Eurydice_dst_f1 x)
{
  size_t uu____0 = (size_t)0U;
  size_t uu____1 = (size_t)0U;
  _uint32_t__x2 uu____2;
  uu____2.fst =
    &Eurydice_slice_index(Eurydice_slice_of_dst(&x.ptr->my_data,
        x.len,
        uint32_t [3U],
        Eurydice_slice),
      uu____0,
      uint32_t [3U],
      uint32_t (*)[3U])[uu____1];
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 0U;
  uu____2.snd = &lvalue;
  uint32_t *left_val = uu____2.fst;
  uint32_t *right_val = uu____2.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void dst_main3(void)
{
  Eurydice_dst_f1 x = dst_alloc3();
  dst_check_var_field_ref3(x);
}

Eurydice_dst_7a dst_mk(void)
{
  dst_T2_dd x;
  x.header = (size_t)0U;
  uint32_t repeat_expression[4U] = { 0U };
  memcpy(x.my_data, repeat_expression, (size_t)4U * sizeof (uint32_t));
  x.my_data[1U] = 2U;
  Eurydice_dst_7a
  y = { .ptr = (dst_T2_be *)Eurydice_box_new(x, dst_T2_dd, dst_T2_dd *), .len = (size_t)4U };
  return y;
}

void dst_main4(void)
{
  uint32_t repeat_expression[4U] = { 0U };
  Eurydice_slice
  x =
    Eurydice_slice_of_boxed_array(Eurydice_box_new_array((size_t)4U,
        repeat_expression,
        uint32_t,
        uint32_t *),
      (size_t)4U,
      uint32_t,
      Eurydice_slice);
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 0U;
  _uint32_t__x2
  uu____0 = { .fst = &Eurydice_slice_index(x, (size_t)3U, uint32_t, uint32_t *), .snd = &lvalue };
  uint32_t *left_val = uu____0.fst;
  uint32_t *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

void dst_main(void)
{
  dst_check_regular_field(dst_alloc());
  dst_check_var_field(dst_alloc());
  dst_S_dd x;
  x.foo = 0U;
  uint32_t repeat_expression[4U] = { 0U };
  memcpy(x.my_data, repeat_expression, (size_t)4U * sizeof (uint32_t));
  Eurydice_dst_31 x0 = { .ptr = (dst_T *)&x, .len = (size_t)4U };
  dst_check_regular_field_ref(x0);
  dst_check_var_field_ref(x0);
  dst_main3();
  Eurydice_dst_7a uu____0 = dst_mk();
  size_t uu____1 = (size_t)0U;
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue0 = 0U;
  _uint32_t__x2
  uu____2 =
    {
      .fst = &Eurydice_slice_index(Eurydice_slice_of_dst(&uu____0.ptr->my_data,
          uu____0.len,
          uint32_t,
          Eurydice_slice),
        uu____1,
        uint32_t,
        uint32_t *),
      .snd = &lvalue0
    };
  uint32_t *left_val0 = uu____2.fst;
  uint32_t *right_val0 = uu____2.snd;
  EURYDICE_ASSERT(left_val0[0U] == right_val0[0U], "panic!");
  Eurydice_dst_7a uu____3 = dst_mk();
  size_t uu____4 = (size_t)1U;
  /* original Rust expression is not an lvalue in C */
  uint32_t lvalue = 2U;
  _uint32_t__x2
  uu____5 =
    {
      .fst = &Eurydice_slice_index(Eurydice_slice_of_dst(&uu____3.ptr->my_data,
          uu____3.len,
          uint32_t,
          Eurydice_slice),
        uu____4,
        uint32_t,
        uint32_t *),
      .snd = &lvalue
    };
  uint32_t *left_val = uu____5.fst;
  uint32_t *right_val = uu____5.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
  dst_main4();
}

