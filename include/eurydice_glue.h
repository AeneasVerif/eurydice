#pragma once

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
// For __popcnt
#include <intrin.h>
#endif

#include "krml/internal/target.h"
#include "krml/lowstar_endianness.h"

// C++ HELPERS

#if defined(__cplusplus)

#ifndef KRML_HOST_EPRINTF
#define KRML_HOST_EPRINTF(...) fprintf(stderr, __VA_ARGS__)
#endif

#include <utility>

#ifndef __cpp_lib_type_identity
template <class T> struct type_identity {
  using type = T;
};

template <class T> using type_identity_t = typename type_identity<T>::type;
#else
using std::type_identity_t;
#endif

#define KRML_UNION_CONSTRUCTOR(T)                                              \
  template <typename V>                                                        \
  constexpr T(int t, V U::*m, type_identity_t<V> v) : tag(t) {                 \
    val.*m = std::move(v);                                                     \
  }                                                                            \
  T() = default;

#endif

// GENERAL-PURPOSE STUFF

#define LowStar_Ignore_ignore(e, t, _ret_t) ((void)e)

#define EURYDICE_ASSERT(test, msg)                                             \
  do {                                                                         \
    if (!(test)) {                                                             \
      fprintf(stderr, "assertion \"%s\" failed: file \"%s\", line %d\n", msg,  \
              __FILE__, __LINE__);                                             \
      exit(255);                                                               \
    }                                                                          \
  } while (0)

// SIZEOF, ALIGNOF

#define Eurydice_sizeof(t, _) sizeof(t)

#define Eurydice_alignof(t, _) _Alignof(t)

#define Eurydice_opaque(reason, t, _) KRML_EABORT(t, reason)

// SLICES, ARRAYS, ETC.

// We define several slice types in order to define builtin definitions for
// libcrux

typedef struct Eurydice_dst_ref_87_s {
  uint8_t *ptr;
  size_t meta;
} Eurydice_dst_ref_87;

typedef struct Eurydice_dst_ref_9a_s {
  int16_t *ptr;
  size_t meta;
} Eurydice_dst_ref_9a;

#if defined(__cplusplus)
#define KRML_CLITERAL(type) type
#else
#define KRML_CLITERAL(type) (type)
#endif

#if defined(__cplusplus) && defined(__cpp_designated_initializers) ||          \
    !(defined(__cplusplus))
#define EURYDICE_CFIELD(X) X
#else
#define EURYDICE_CFIELD(X)
#endif

// Slice length
#define EURYDICE_SLICE_LEN(s, _) (s).meta
#define Eurydice_slice_len(s, _) (s).meta

#define Eurydice_slice_index(s, i, t) ((s).ptr[i])

#define Eurydice_array_repeat(dst, len, init, t)                               \
  ERROR "should've been desugared"

// Copy a slice with memcopy
#define Eurydice_slice_copy(dst, src, t)                                       \
  memcpy(dst.ptr, src.ptr, dst.meta * sizeof(t))

#define core_array___Array_T__N___as_slice(len_, ptr_, t, ret_t)               \
  (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.ptr =)(ptr_)->data,                   \
                        EURYDICE_CFIELD(.meta =) len_})

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
#define Eurydice_array_eq_slice(sz, a1, s2, t, _)                              \
  (memcmp((a1)->data, (s2)->ptr, sz * sizeof(t)) == 0)

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

// CORE STUFF (conversions, endianness, ...)

// We slap extern "C" on declarations that intend to implement a prototype
// generated by Eurydice, because Eurydice prototypes are always emitted within
// an extern "C" block, UNLESS you use -fcxx17-compat, in which case, you must
// pass -DKRML_CXX17_COMPAT="" to your C++ compiler.
#if defined(__cplusplus) && !defined(KRML_CXX17_COMPAT)
extern "C" {
#endif

#define core_hint_black_box(X, _0, _1) (X)

typedef struct Eurydice_arr_8b_s {
  uint8_t data[2];
} Eurydice_arr_8b;
typedef struct Eurydice_arr_e9_s {
  uint8_t data[4];
} Eurydice_arr_e9;
typedef struct Eurydice_arr_c4_s {
  uint8_t data[8];
} Eurydice_arr_c4;

static inline uint16_t core_num__u16__from_le_bytes(Eurydice_arr_8b buf) {
  return load16_le(buf.data);
}

static inline Eurydice_arr_e9 core_num__u32__to_be_bytes(uint32_t src) {
  // TODO: why not store32_be?
  Eurydice_arr_e9 a;
  uint32_t x = htobe32(src);
  memcpy(a.data, &x, 4);
  return a;
}

static inline Eurydice_arr_e9 core_num__u32__to_le_bytes(uint32_t src) {
  Eurydice_arr_e9 a;
  store32_le(a.data, src);
  return a;
}

static inline uint32_t core_num__u32__from_le_bytes(Eurydice_arr_e9 buf) {
  return load32_le(buf.data);
}

static inline Eurydice_arr_c4 core_num__u64__to_le_bytes(uint64_t v) {
  Eurydice_arr_c4 a;
  store64_le(a.data, v);
  return a;
}

static inline uint64_t core_num__u64__from_le_bytes(Eurydice_arr_c4 buf) {
  return load64_le(buf.data);
}

static inline int64_t
core_convert_num__core__convert__From_i32__for_i64__from(int32_t x) {
  return x;
}

static inline uint64_t
core_convert_num__core__convert__From_u8__for_u64__from(uint8_t x) {
  return x;
}

static inline uint64_t
core_convert_num__core__convert__From_u16__for_u64__from(uint16_t x) {
  return x;
}

static inline size_t
core_convert_num__core__convert__From_u16__for_usize__from(uint16_t x) {
  return x;
}

static inline uint32_t core_num__u8__count_ones(uint8_t x0) {
#ifdef _MSC_VER
  return __popcnt(x0);
#else
  return __builtin_popcount(x0);
#endif
}

static inline uint32_t core_num__u32__count_ones(uint32_t x0) {
#ifdef _MSC_VER
  return __popcnt(x0);
#else
  return __builtin_popcount(x0);
#endif
}

static inline uint32_t core_num__i32__count_ones(int32_t x0) {
#ifdef _MSC_VER
  return __popcnt(x0);
#else
  return __builtin_popcount(x0);
#endif
}

static inline size_t core_cmp_impls__core__cmp__Ord_for_usize__min(size_t a,
                                                                   size_t b) {
  if (a <= b)
    return a;
  else
    return b;
}

// unsigned overflow wraparound semantics in C
static inline uint8_t core_num__u8__wrapping_sub(uint8_t x, uint8_t y) {
  return x - y;
}
static inline uint8_t core_num__u8__wrapping_add(uint8_t x, uint8_t y) {
  return x + y;
}
static inline uint8_t core_num__u8__wrapping_mul(uint8_t x, uint8_t y) {
  return x * y;
}
static inline uint16_t core_num__u16__wrapping_sub(uint16_t x, uint16_t y) {
  return x - y;
}
static inline uint16_t core_num__u16__wrapping_add(uint16_t x, uint16_t y) {
  return x + y;
}
static inline uint16_t core_num__u16__wrapping_mul(uint16_t x, uint16_t y) {
  return x * y;
}
static inline uint32_t core_num__u32__wrapping_sub(uint32_t x, uint32_t y) {
  return x - y;
}
static inline uint32_t core_num__u32__wrapping_add(uint32_t x, uint32_t y) {
  return x + y;
}
static inline uint32_t core_num__u32__wrapping_mul(uint32_t x, uint32_t y) {
  return x * y;
}
static inline uint64_t core_num__u64__wrapping_sub(uint64_t x, uint64_t y) {
  return x - y;
}
static inline uint64_t core_num__u64__wrapping_add(uint64_t x, uint64_t y) {
  return x + y;
}
static inline uint64_t core_num__u64__wrapping_mul(uint64_t x, uint64_t y) {
  return x * y;
}
static inline size_t core_num__usize__wrapping_sub(size_t x, size_t y) {
  return x - y;
}
static inline size_t core_num__usize__wrapping_add(size_t x, size_t y) {
  return x + y;
}
static inline size_t core_num__usize__wrapping_mul(size_t x, size_t y) {
  return x * y;
}

static inline uint64_t core_num__u64__rotate_left(uint64_t x0, uint32_t x1) {
  return (x0 << x1) | (x0 >> ((-x1) & 63));
}

static inline void core_ops_arith__i32__add_assign(int32_t *x0, int32_t *x1) {
  *x0 = *x0 + *x1;
}

static inline uint8_t Eurydice_bitand_pv_u8(uint8_t *p, uint8_t v) {
  return (*p) & v;
}
static inline uint8_t Eurydice_shr_pv_u8(uint8_t *p, int32_t v) {
  return (*p) >> v;
}
static inline uint32_t Eurydice_min_u32(uint32_t x, uint32_t y) {
  return x < y ? x : y;
}

static inline uint8_t
core_ops_bit__core__ops__bit__BitAnd_u8__u8__for___a__u8___bitand(uint8_t *x0,
                                                                  uint8_t x1) {
  return Eurydice_bitand_pv_u8(x0, x1);
}

static inline uint8_t
core_ops_bit__core__ops__bit__Shr_i32__u8__for___a__u8___shr(uint8_t *x0,
                                                             int32_t x1) {
  return Eurydice_shr_pv_u8(x0, x1);
}

#define core_num_nonzero_private_NonZeroUsizeInner size_t
static inline core_num_nonzero_private_NonZeroUsizeInner
core_num_nonzero_private___core__clone__Clone_for_core__num__nonzero__private__NonZeroUsizeInner___clone(
    core_num_nonzero_private_NonZeroUsizeInner *x0) {
  return *x0;
}

#if defined(__cplusplus) && !defined(KRML_CXX17_COMPAT)
}
#endif

// ITERATORS

#define Eurydice_range_iter_next(iter_ptr, t, ret_t)                           \
  (((iter_ptr)->start >= (iter_ptr)->end)                                      \
       ? (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.tag =) 0,                      \
                               EURYDICE_CFIELD(.f0 =) 0})                      \
       : (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.tag =) 1,                      \
                               EURYDICE_CFIELD(.f0 =)(iter_ptr)->start++}))

#define core_iter_range__core__iter__traits__iterator__Iterator_A__for_core__ops__range__Range_A__TraitClause_0___next \
  Eurydice_range_iter_next

// See note in karamel/lib/Inlining.ml if you change this
#define Eurydice_into_iter(x, t, _ret_t, _) (x)
#define core_iter_traits_collect__core__iter__traits__collect__IntoIterator_Clause1_Item__I__for_I__into_iter \
  Eurydice_into_iter

// STRINGS

typedef char Eurydice_c_char_t;
typedef const Eurydice_c_char_t *Prims_string;
typedef void Eurydice_c_void_t;

// UNSAFE CODE

#define core_slice___Slice_T___as_mut_ptr(x, t, _) (x.ptr)
#define core_mem_size_of(t, _) (sizeof(t))
#define core_slice_raw_from_raw_parts_mut(ptr, len, _0, _1)                    \
  (KRML_CLITERAL(Eurydice_slice){(void *)(ptr), len})
#define core_slice_raw_from_raw_parts(ptr, len, _0, _1)                        \
  (KRML_CLITERAL(Eurydice_slice){(void *)(ptr), len})

// FIXME: add dedicated extraction to extract NonNull<T> as T*
#define core_ptr_non_null_NonNull void *

// PRINTING
//
// This is temporary. Ultimately we want to be able to extract all of this.

typedef void *core_fmt_Formatter;
#define core_fmt_rt__core__fmt__rt__Argument__a___new_display(x1, x2, x3, x4)  \
  NULL

// BOXES

#ifndef EURYDICE_MALLOC
#define EURYDICE_MALLOC malloc
#endif

#ifndef EURYDICE_REALLOC
#define EURYDICE_REALLOC realloc
#endif

static inline char *malloc_and_init(size_t sz, char *init) {
  char *ptr = (char *)EURYDICE_MALLOC(sz);
  if (ptr != NULL)
    memcpy(ptr, init, sz);
  return ptr;
}

#define Eurydice_box_new(init, t, t_dst)                                       \
  ((t_dst)(malloc_and_init(sizeof(t), (char *)(&init))))

// Initializer for array of size zero
#define Eurydice_empty_array(dummy, t, t_dst) ((t_dst){.data = {}})

#define Eurydice_box_new_array(len, ptr, t, t_dst)                             \
  ((t_dst)(malloc_and_init(len * sizeof(t), (char *)(ptr))))

// FIXME this needs to handle allocation failure errors, but this seems hard to
// do without evaluating malloc_and_init twice...
#define alloc_boxed__alloc__boxed__Box_T___try_new(init, t, t_ret)             \
  ((t_ret){.tag = core_result_Ok,                                              \
           .f0 = (t *)malloc_and_init(sizeof(t), (char *)(&init))})

// VECTORS

// We adapt the layout of https://doc.rust-lang.org/std/vec/struct.Vec.html,
// dispensing with the nested RawVec -- basically, we follow what the
// documentation says. Just like Eurydice_slice, we keep sizes in number of
// elements. This means we pass three words by value whenever we carry a vector
// around. Things that modify the vector take &mut's in Rust, or a Eurydice_vec*
// in C.
//
// Another design choice: just like Eurydice_slice, we treat Eurydice_vec as an
// opaque type, and rely on macros receiving their type arguments at call-site
// to perform necessary casts. A downside is that anything that looks into the
// definition of Eurydice_vec must be exposed (from the eurydice point of view)
// as an external -- see, for instance, Eurydice_vec_failed, below.
typedef struct {
  char *ptr;
  size_t len;      /* current length, in elements */
  size_t capacity; /* the size of the allocation, in number of elements */
} Eurydice_vec, alloc_vec_Vec;

// This is a helper that Eurydice has special knowledge about. Essentially,
// allocation functions return a result type that has been monomorphized, say,
// Result_XY; this means we need to do something like:
//   Eurydice_vec v = try_with_capacity(len, sz);
//   Result_XY r = v.ptr == NULL ? (Result_XY) { .tag = core_result_Ok, .case_Ok
//   = v }
//     : (Result_XY) { .tag = core_result_Error, .case_Error = ... };
// but with a macro (since we don't have templates).
// However, unless we allow statement-expressions (GCC extension), we cannot do
// the above with an expression, since we need to name the result of
// try_with_capacity to avoid evaluating it twice.
static inline Eurydice_vec Eurydice_vec_alloc2(size_t len, size_t element_sz) {
  return ((Eurydice_vec){.ptr = (char *)EURYDICE_MALLOC(len * element_sz),
                         .len = len,
                         .capacity = len});
}

#define Eurydice_vec_alloc(len, t, _) (Eurydice_vec_alloc2((len), sizeof(t)))
#define Eurydice_vec_overflows(len, t, _) (!((len) <= SIZE_MAX / (sizeof(t))))
#define Eurydice_vec_failed(v, _, _1) ((v).ptr == NULL)
#define Eurydice_layout(t, _)                                                  \
  ((core_alloc_layout_Layout){.size = sizeof(t), .align = _Alignof(t)})

#define alloc_vec__alloc__vec__Vec_T___resize(                                 \
    /* Eurydice_vec * */ v, /* size_t */ new_len, /* T */ elt, T, _0, _1)      \
  do {                                                                         \
    if (new_len <= (v)->capacity)                                              \
      (v)->len = new_len;                                                      \
    else {                                                                     \
      (v)->ptr = EURYDICE_REALLOC((v)->ptr, new_len * sizeof(T));              \
      /* TODO: check success? Rust function is infallible */                   \
      for (size_t i = (v)->len; i < new_len; i++)                              \
        ((T *)(v)->ptr)[i] = elt;                                              \
      (v)->len = new_len;                                                      \
      (v)->capacity = new_len;                                                 \
    }                                                                          \
  } while (0)

#define alloc_vec__alloc__vec__Vec_T___into_boxed_slice(/* Eurydice_vec */ v,  \
                                                        T, _0, _1)             \
  ((Eurydice_slice){.ptr = (v).ptr, .len = (v).len})

#define alloc_boxed__alloc__boxed__Box_T___from_raw(x, _0, _1) (x)
#define alloc_boxed__alloc__boxed__Box_T___into_raw(x, _0, _1) (x)
