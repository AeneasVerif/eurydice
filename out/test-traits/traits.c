/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "traits.h"

/**
This function found in impl {(traits::ToInt for traits::Foo)}
*/
uint32_t traits_to_int_7d(traits_Foo *self)
{
  if (!(self[0U] == traits_Foo_Foo1))
  {
    return 2U;
  }
  return 1U;
}

/**
This function found in impl {(traits::ToInt for &0 (@Slice<traits::Foo>))#1}
*/
uint32_t traits_to_int_dd(Eurydice_slice *self)
{
  uint32_t
  uu____0 =
    traits_to_int_7d(&Eurydice_slice_index(self[0U], (size_t)0U, traits_Foo, traits_Foo *));
  return
    uu____0
    * traits_to_int_7d(&Eurydice_slice_index(self[0U], (size_t)1U, traits_Foo, traits_Foo *));
}

void traits_main(void)
{
  traits_Foo foos[2U] = { traits_Foo_Foo1, traits_Foo_Foo2 };
  /* original Rust expression is not an lvalue in C */
  Eurydice_slice lvalue = Eurydice_array_to_subslice2(foos, (size_t)0U, (size_t)2U, traits_Foo);
  if (!(traits_to_int_dd(&lvalue) != 2U))
  {
    return;
  }
  KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n", __FILE__, __LINE__, "panic!");
  KRML_HOST_EXIT(255U);
}

