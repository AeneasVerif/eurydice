/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "const_generics.h"

#include "Eurydice.h"

/**
A monomorphic instance of const_generics.serialize
with const generics
- OUT_LEN= 8
*/
void const_generics_serialize_3b(Eurydice_slice re, uint8_t ret[8U])
{
  uint8_t out[8U] = { 0U };
  Eurydice_slice
  uu____0 =
    Eurydice_array_to_subslice_to((size_t)8U,
      out,
      (size_t)4U,
      uint8_t,
      size_t,
      __builtin_slice_t);
  uint8_t ret0[4U];
  core_num__u32_8__to_be_bytes(Eurydice_slice_index(re, (size_t)0U, uint32_t, uint32_t *), ret0);
  Eurydice_slice_copy(uu____0, Eurydice_array_to_slice((size_t)4U, ret0, uint8_t), uint8_t);
  Eurydice_slice
  uu____1 =
    Eurydice_array_to_subslice_from((size_t)8U,
      out,
      (size_t)4U,
      uint8_t,
      size_t,
      __builtin_slice_t);
  uint8_t ret1[4U];
  core_num__u32_8__to_be_bytes(Eurydice_slice_index(re, (size_t)1U, uint32_t, uint32_t *), ret1);
  Eurydice_slice_copy(uu____1, Eurydice_array_to_slice((size_t)4U, ret1, uint8_t), uint8_t);
  memcpy(ret, out, (size_t)8U * sizeof (uint8_t));
}

void const_generics_main(void)
{
  uint8_t s[8U];
  uint32_t buf[2U] = { 1U, 2U };
  const_generics_serialize_3b(Eurydice_array_to_slice((size_t)2U, buf, uint32_t), s);
  EURYDICE_ASSERT(s[3U] == 1U, "panic!");
  EURYDICE_ASSERT(s[7U] == 2U, "panic!");
}

/**
A monomorphic instance of const_generics.mk_pairs
with types uint32_t, uint64_t
with const generics
- N= 2
- M= 4
*/
const_generics_Pair_4e const_generics_mk_pairs_e0(uint32_t x, uint64_t y)
{
  uint32_t a1[2U];
  KRML_MAYBE_FOR2(i, (size_t)0U, (size_t)2U, (size_t)1U, a1[i] = x;);
  uint64_t a2[4U];
  KRML_MAYBE_FOR4(i, (size_t)0U, (size_t)4U, (size_t)1U, a2[i] = y;);
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_a10[2U];
  memcpy(copy_of_a10, a1, (size_t)2U * sizeof (uint32_t));
  /* Passing arrays by value in Rust generates a copy in C */
  uint64_t copy_of_a20[4U];
  memcpy(copy_of_a20, a2, (size_t)4U * sizeof (uint64_t));
  const_generics_Pair_a5 p1;
  memcpy(p1.left, copy_of_a10, (size_t)2U * sizeof (uint32_t));
  memcpy(p1.right, copy_of_a20, (size_t)4U * sizeof (uint64_t));
  /* Passing arrays by value in Rust generates a copy in C */
  uint64_t copy_of_a2[4U];
  memcpy(copy_of_a2, a2, (size_t)4U * sizeof (uint64_t));
  /* Passing arrays by value in Rust generates a copy in C */
  uint32_t copy_of_a1[2U];
  memcpy(copy_of_a1, a1, (size_t)2U * sizeof (uint32_t));
  const_generics_Pair_87 p2;
  memcpy(p2.left, copy_of_a2, (size_t)4U * sizeof (uint64_t));
  memcpy(p2.right, copy_of_a1, (size_t)2U * sizeof (uint32_t));
  uint32_t uu____4[2U];
  memcpy(uu____4, p1.left, (size_t)2U * sizeof (uint32_t));
  uint32_t uu____5[2U];
  memcpy(uu____5, p2.right, (size_t)2U * sizeof (uint32_t));
  const_generics_Pair_4e lit;
  memcpy(lit.left, uu____4, (size_t)2U * sizeof (uint32_t));
  memcpy(lit.right, uu____5, (size_t)2U * sizeof (uint32_t));
  return lit;
}

typedef struct _uint32_t__x2_s
{
  uint32_t *fst;
  uint32_t *snd;
}
_uint32_t__x2;

void const_generics_main1(void)
{
  const_generics_Pair_4e uu____0 = const_generics_mk_pairs_e0(0U, 0ULL);
  uint32_t left[2U];
  memcpy(left, uu____0.left, (size_t)2U * sizeof (uint32_t));
  uint32_t right[2U];
  memcpy(right, uu____0.right, (size_t)2U * sizeof (uint32_t));
  uint32_t expected = 0U;
  _uint32_t__x2 uu____1;
  uu____1.fst = left;
  uu____1.snd = &expected;
  uint32_t *left_val0 = uu____1.fst;
  uint32_t *right_val0 = uu____1.snd;
  EURYDICE_ASSERT(left_val0[0U] == right_val0[0U], "panic!");
  _uint32_t__x2 uu____2;
  uu____2.fst = &left[1U];
  uu____2.snd = &expected;
  uint32_t *left_val1 = uu____2.fst;
  uint32_t *right_val1 = uu____2.snd;
  EURYDICE_ASSERT(left_val1[0U] == right_val1[0U], "panic!");
  _uint32_t__x2 uu____3;
  uu____3.fst = right;
  uu____3.snd = &expected;
  uint32_t *left_val2 = uu____3.fst;
  uint32_t *right_val2 = uu____3.snd;
  EURYDICE_ASSERT(left_val2[0U] == right_val2[0U], "panic!");
  _uint32_t__x2 uu____4;
  uu____4.fst = &right[1U];
  uu____4.snd = &expected;
  uint32_t *left_val = uu____4.fst;
  uint32_t *right_val = uu____4.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

/**
A monomorphic instance of const_generics.f
with const generics
- FOO= 1
- BAR= 2
*/
bool const_generics_f_e5(uint32_t x, size_t y)
{
  uint32_t arr1[1U];
  {
    arr1[0U] = x;
  }
  size_t arr2[1U];
  {
    arr2[0U] = y;
  }
  bool uu____0;
  if (arr1[0U] == 2U)
  {
    uu____0 = arr2[0U] == (size_t)1U;
  }
  else
  {
    uu____0 = false;
  }
  return uu____0;
}

/**
A monomorphic instance of const_generics.f
with const generics
- FOO= 3
- BAR= 4
*/
bool const_generics_f_70(uint32_t x, size_t y)
{
  uint32_t arr1[3U];
  KRML_MAYBE_FOR3(i, (size_t)0U, (size_t)3U, (size_t)1U, arr1[i] = x;);
  size_t arr2[3U];
  KRML_MAYBE_FOR3(i, (size_t)0U, (size_t)3U, (size_t)1U, arr2[i] = y;);
  bool uu____0;
  if (arr1[0U] == 4U)
  {
    uu____0 = arr2[0U] == (size_t)3U;
  }
  else
  {
    uu____0 = false;
  }
  return uu____0;
}

/**
A monomorphic instance of const_generics.g
with const generics
- BAR= 3
- FOO= 4
*/
bool const_generics_g_70(uint32_t x, size_t y)
{
  bool uu____0;
  if (const_generics_f_70(x, y))
  {
    if (x == 4U)
    {
      uu____0 = y == (size_t)3U;
    }
    else
    {
      uu____0 = false;
    }
  }
  else
  {
    uu____0 = false;
  }
  return uu____0;
}

typedef struct _bool__x2_s
{
  bool *fst;
  bool *snd;
}
_bool__x2;

void const_generics_main3(void)
{
  bool x;
  if (const_generics_f_e5(0U, (size_t)0U))
  {
    x = const_generics_g_70(0U, (size_t)0U);
  }
  else
  {
    x = false;
  }
  bool expected = false;
  _bool__x2 uu____0 = { CFIELD(.fst, &x), CFIELD(.snd, &expected) };
  bool *left_val = uu____0.fst;
  bool *right_val = uu____0.snd;
  EURYDICE_ASSERT(left_val[0U] == right_val[0U], "panic!");
}

