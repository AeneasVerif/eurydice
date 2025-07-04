/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#ifndef __symcrust_H
#define __symcrust_H

#include "eurydice_glue.h"


#if defined(__cplusplus)
extern "C" {
#endif

#include "Eurydice.h"

extern uint8_t core_clone_impls__core__clone__Clone_for_u8__clone(uint8_t *x0);

extern size_t core_clone_impls__core__clone__Clone_for_usize__clone(size_t *x0);

#define core_cmp_Ordering_Less -1
#define core_cmp_Ordering_Equal 0
#define core_cmp_Ordering_Greater 1

typedef int8_t core_cmp_Ordering;

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

extern core_cmp_Ordering
core_cmp_impls__core__cmp__Ord_for_u32__cmp(uint32_t *x0, uint32_t *x1);

extern uint32_t core_cmp_impls__core__cmp__Ord_for_u32__min(uint32_t x0, uint32_t x1);

extern bool core_cmp_impls__core__cmp__PartialEq_u32__for_u32__eq(uint32_t *x0, uint32_t *x1);

extern bool core_cmp_impls__core__cmp__PartialEq_usize__for_usize__eq(size_t *x0, size_t *x1);

extern core_option_Option_77
core_cmp_impls__core__cmp__PartialOrd_u32__for_u32__partial_cmp(uint32_t *x0, uint32_t *x1);

extern core_option_Option_77
core_cmp_impls__core__cmp__PartialOrd_usize__for_usize__partial_cmp(size_t *x0, size_t *x1);

static inline uint32_t core_convert_num__core__convert__From_u16__for_u32__from(uint16_t x0);

static inline uint64_t core_convert_num__core__convert__From_u32__for_u64__from(uint32_t x0);

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

extern core_option_Option_08
core_iter_range__core__iter__range__Step_for_usize__backward_checked(size_t x0, size_t x1);

extern core_option_Option_08
core_iter_range__core__iter__range__Step_for_usize__forward_checked(size_t x0, size_t x1);

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
core_iter_range__core__iter__range__Step_for_usize__steps_between(size_t *x0, size_t *x1);

static inline void core_num__u32__to_le_bytes(uint32_t x0, uint8_t x1[4U]);

void
symcrust_SymCrustMlKemPolyElementCompressAndEncode(
  uint16_t *coeffs,
  uint32_t nBitsPerCoefficient,
  Eurydice_slice dst
);

void symcrust_main(void);

#if defined(__cplusplus)
}
#endif

#define __symcrust_H_DEFINED
#endif
