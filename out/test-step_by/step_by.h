/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#ifndef __step_by_H
#define __step_by_H

#include "eurydice_glue.h"


#if defined(__cplusplus)
extern "C" {
#endif

extern int32_t core_clone_impls___core__clone__Clone_for_i32__14__clone(int32_t *x0);

#define core_cmp_Ordering_Less -1
#define core_cmp_Ordering_Equal 0
#define core_cmp_Ordering_Greater 1

typedef int8_t core_cmp_Ordering;

extern bool
core_cmp_impls___core__cmp__PartialEq_i32__for_i32__30__eq(int32_t *x0, int32_t *x1);

#define core_option_None 0
#define core_option_Some 1

typedef uint8_t core_option_Option_77_tags;

/**
A monomorphic instance of core.option.Option
with types core_cmp_Ordering

*/
typedef struct core_option_Option_77_s
{
  core_option_Option_77_tags tag;
  core_cmp_Ordering f0;
}
core_option_Option_77;

extern core_option_Option_77
core_cmp_impls___core__cmp__PartialOrd_i32__for_i32__76__partial_cmp(int32_t *x0, int32_t *x1);

/**
A monomorphic instance of core.option.Option
with types int32_t

*/
typedef struct core_option_Option_9e_s
{
  core_option_Option_77_tags tag;
  int32_t f0;
}
core_option_Option_9e;

extern core_option_Option_9e
core_iter_range___core__iter__range__Step_for_i32__40__backward_checked(int32_t x0, size_t x1);

extern core_option_Option_9e
core_iter_range___core__iter__range__Step_for_i32__40__forward_checked(int32_t x0, size_t x1);

/**
A monomorphic instance of core.option.Option
with types size_t

*/
typedef struct core_option_Option_08_s
{
  core_option_Option_77_tags tag;
  size_t f0;
}
core_option_Option_08;

/**
A monomorphic instance of K.
with types size_t, core_option_Option size_t

*/
typedef struct tuple_04_s
{
  size_t fst;
  core_option_Option_08 snd;
}
tuple_04;

extern tuple_04
core_iter_range___core__iter__range__Step_for_i32__40__steps_between(int32_t *x0, int32_t *x1);

#define core_panicking_AssertKind_Eq 0
#define core_panicking_AssertKind_Ne 1
#define core_panicking_AssertKind_Match 2

typedef uint8_t core_panicking_AssertKind;

void step_by_bar(void);

void step_by_main1(void);

void step_by_main2(void);

void step_by_main(void);

#if defined(__cplusplus)
}
#endif

#define __step_by_H_DEFINED
#endif
