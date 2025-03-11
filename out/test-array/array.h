/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#ifndef __array_H
#define __array_H
#include "eurydice_glue.h"


#if defined(__cplusplus)
extern "C" {
#endif

typedef struct array_Foo_s
{
  uint32_t x[2U];
  uint32_t y[2U];
}
array_Foo;

#define core_panicking_AssertKind_Eq 0
#define core_panicking_AssertKind_Ne 1
#define core_panicking_AssertKind_Match 2

typedef uint8_t core_panicking_AssertKind;

array_Foo array_mk_foo(void);

array_Foo array_mk_foo2(void);

void array_mut_array(uint32_t x[2U]);

void array_mut_foo(array_Foo f);

/**
A monomorphic instance of array.mk_incr.closure
with const generics
- K= 10
*/
uint32_t array_mk_incr_closure_95(size_t i);

/**
A monomorphic instance of array.mk_incr
with const generics
- K= 10
*/
void array_mk_incr_95(uint32_t ret[10U]);

void array_main(void);

#if defined(__cplusplus)
}
#endif

#define __array_H_DEFINED
#endif
