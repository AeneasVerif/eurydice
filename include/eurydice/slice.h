#pragma once

#include <inttypes.h>
#include <stdlib.h>

// SLICES, ARRAYS, ETC.

// For convenience, we give these common slice types, below, a distinguished
// status and rather than emit them in the client code, we skip their
// code-generation in Cleanup3.ml and write them by hand here. This makes it
// easy to write interop code that brings those definitions in scope.

// &[u8]
typedef struct Eurydice_borrow_slice_u8_s {
  const uint8_t *ptr;
  size_t meta;
} Eurydice_borrow_slice_u8;

// &[u16]
typedef struct Eurydice_borrow_slice_i16_s {
  const int16_t *ptr;
  size_t meta;
} Eurydice_borrow_slice_i16;

// &mut [u8]
typedef struct Eurydice_mut_borrow_slice_u8_s {
  uint8_t *ptr;
  size_t meta;
} Eurydice_mut_borrow_slice_u8;

// &mut [u16]
typedef struct Eurydice_mut_borrow_slice_i16_s {
  int16_t *ptr;
  size_t meta;
} Eurydice_mut_borrow_slice_i16;

// Copy a slice with memcopy
#define Eurydice_slice_copy(dst, src, t)                                       \
  memcpy(dst.ptr, src.ptr, dst.meta * sizeof(t))

#define core_array___Array_T__N___as_slice(len_, ptr_, t, ret_t)               \
  (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.ptr =)(ptr_)->data,                   \
                        EURYDICE_CFIELD(.meta =) len_})

#define core_array___Array_T__N___as_mut_slice(len_, ptr_, t, ret_t) \
  core_array___Array_T__N___as_slice(len_, ptr_, t, ret_t)

#define core_array__core__clone__Clone_for__Array_T__N___clone(                \
    len, src, elem_type, _ret_t)                                               \
  (*(src))
#define TryFromSliceError uint8_t
#define core_array_TryFromSliceError uint8_t

// Distinguished support for some PartialEq trait implementations
//
// core::cmp::PartialEq<@Array<U, N>> for @Array<T, N>
#define Eurydice_array_eq(sz, a1, a2, t)                                       \
  (memcmp((a1)->data, (a2)->data, sz * sizeof(t)) == 0)
// core::cmp::PartialEq<&0 (@Slice<U>)> for @Array<T, N>
#define Eurydice_array_eq_slice_shared(sz, a1, s2, t, _)                       \
  (memcmp((a1)->data, (s2)->ptr, sz * sizeof(t)) == 0)
#define Eurydice_array_eq_slice_mut(sz, a1, s2, t, _)                          \
  Eurydice_array_eq_slice_shared(sz, a1, s2, t, _)

#define Eurydice_slice_eq_shared(s1, s2, t, _) \
  ((s1)->meta == (s2)->meta &&                 \
   memcmp((s1)->ptr, (s2)->ptr, (s1)->meta * sizeof(t)) == 0)

// DEPRECATED -- should no longer be generated
#define core_array_equality__core__cmp__PartialEq__Array_U__N___for__Array_T__N___eq( \
    sz, a1, a2, t, _, _ret_t)                                                         \
  Eurydice_array_eq(sz, a1, a2, t)
#define core_array_equality__core__cmp__PartialEq__0___Slice_U____for__Array_T__N___eq( \
    sz, a1, a2, t, _, _ret_t)                                                           \
  Eurydice_array_eq(sz, a1, ((a2)->ptr), t)
#define core_cmp_impls__core__cmp__PartialEq__0_mut__B___for__1_mut__A___eq(   \
    _m0, _m1, src1, src2, _0, _1, T)                                           \
  Eurydice_slice_eq(src1, src2, _, _, T, _)

#define Eurydice_slice_split_at(slice, mid, element_type, ret_t)               \
  KRML_CLITERAL(ret_t) {                                                       \
    EURYDICE_CFIELD(.fst =){EURYDICE_CFIELD(.ptr =)((slice).ptr),              \
                            EURYDICE_CFIELD(.meta =) mid},                     \
        EURYDICE_CFIELD(.snd =) {                                              \
      EURYDICE_CFIELD(.ptr =)                                                  \
      ((slice).ptr + mid), EURYDICE_CFIELD(.meta =)((slice).meta - mid)        \
    }                                                                          \
  }

#define Eurydice_slice_split_at_mut(slice, mid, element_type, ret_t)           \
  KRML_CLITERAL(ret_t) {                                                       \
    EURYDICE_CFIELD(.fst =){EURYDICE_CFIELD(.ptr =)((slice).ptr),              \
                            EURYDICE_CFIELD(.meta =) mid},                     \
        EURYDICE_CFIELD(.snd =) {                                              \
      EURYDICE_CFIELD(.ptr =)                                                  \
      ((slice).ptr + mid), EURYDICE_CFIELD(.meta =)((slice).meta - mid)        \
    }                                                                          \
  }

// Conversion of slice to an array, rewritten (by Eurydice) to name the
// destination array, since arrays are not values in C.
// N.B.: see note in karamel/lib/Inlining.ml if you change this.

#define Eurydice_slice_to_ref_array2(len_, src, arr_ptr, t_ptr, t_arr, t_err,  \
                                     t_res)                                    \
  (src.meta >= len_                                                            \
       ? ((t_res){.tag = core_result_Ok, .val = {.case_Ok = arr_ptr}})         \
       : ((t_res){.tag = core_result_Err, .val = {.case_Err = 0}}))

