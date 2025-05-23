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

// SLICES, ARRAYS, ETC.

// We represent a slice as a pair of an (untyped) pointer, along with the length
// of the slice, i.e. the number of elements in the slice (this is NOT the
// number of bytes). This design choice has two important consequences.
// - if you need to use `ptr`, you MUST cast it to a proper type *before*
//   performing pointer arithmetic on it (remember that C desugars pointer
//   arithmetic based on the type of the address)
// - if you need to use `len` for a C style function (e.g. memcpy, memcmp), you
//   need to multiply it by sizeof t, where t is the type of the elements.
//
// Empty slices have `len == 0` and `ptr` always needs to be a valid pointer
// that is not NULL (otherwise the construction in EURYDICE_SLICE computes `NULL
// + start`).
typedef struct {
  void *ptr;
  size_t len;
} Eurydice_slice;

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

// Helper macro to create a slice out of a pointer x, a start index in x
// (included), and an end index in x (excluded). The argument x must be suitably
// cast to something that can decay (see remark above about how pointer
// arithmetic works in C), meaning either pointer or array type.
#define EURYDICE_SLICE(x, start, end)                                          \
  (KRML_CLITERAL(Eurydice_slice){(void *)(x + start), end - start})

// Slice length
#define EURYDICE_SLICE_LEN(s, _) (s).len
#define Eurydice_slice_len(s, _) (s).len

// This macro is a pain because in case the dereferenced element type is an
// array, you cannot simply write `t x` as it would yield `int[4] x` instead,
// which is NOT correct C syntax, so we add a dedicated phase in Eurydice that
// adds an extra argument to this macro at the last minute so that we have the
// correct type of *pointers* to elements.
#define Eurydice_slice_index(s, i, t, t_ptr_t) (((t_ptr_t)s.ptr)[i])

// The following functions get sub slices from a slice.

#define Eurydice_slice_subslice(s, r, t, _0, _1)                               \
  EURYDICE_SLICE((t *)s.ptr, r.start, r.end)

// Variant for when the start and end indices are statically known (i.e., the
// range argument `r` is a literal).
#define Eurydice_slice_subslice2(s, start, end, t)                             \
  EURYDICE_SLICE((t *)s.ptr, start, end)

#define Eurydice_slice_subslice_to(s, subslice_end_pos, t, _0, _1)             \
  EURYDICE_SLICE((t *)s.ptr, 0, subslice_end_pos)

#define Eurydice_slice_subslice_from(s, subslice_start_pos, t, _0, _1)         \
  EURYDICE_SLICE((t *)s.ptr, subslice_start_pos, s.len)

#define Eurydice_array_to_slice(end, x, t)                                     \
  EURYDICE_SLICE(x, 0,                                                         \
                 end) /* x is already at an array type, no need for cast */
#define Eurydice_array_to_subslice(_arraylen, x, r, t, _0, _1)                 \
  EURYDICE_SLICE((t *)x, r.start, r.end)

// Same as above, variant for when start and end are statically known
#define Eurydice_array_to_subslice2(x, start, end, t)                          \
  EURYDICE_SLICE((t *)x, start, end)

#define Eurydice_array_repeat(dst, len, init, t)                               \
  ERROR "should've been desugared"

// The following functions convert an array into a slice.

#define Eurydice_array_to_subslice_to(_size, x, r, t, _range_t, _0)            \
  EURYDICE_SLICE((t *)x, 0, r)
#define Eurydice_array_to_subslice_from(size, x, r, t, _range_t, _0)           \
  EURYDICE_SLICE((t *)x, r, size)

// Copy a slice with memcopy
#define Eurydice_slice_copy(dst, src, t)                                       \
  memcpy(dst.ptr, src.ptr, dst.len * sizeof(t))

#define core_array___Array_T__N__23__as_slice(len_, ptr_, t, _ret_t)           \
  KRML_CLITERAL(Eurydice_slice) { ptr_, len_ }

#define core_array___core__clone__Clone_for__Array_T__N___20__clone(           \
    len, src, dst, elem_type, _ret_t)                                          \
  (memcpy(dst, src, len * sizeof(elem_type)))
#define TryFromSliceError uint8_t
#define core_array_TryFromSliceError uint8_t

#define Eurydice_array_eq(sz, a1, a2, t, _)                                    \
  (memcmp(a1, a2, sz * sizeof(t)) == 0)
#define core_array_equality___core__cmp__PartialEq__Array_U__N___for__Array_T__N____eq( \
    sz, a1, a2, t, _, _ret_t)                                                           \
  Eurydice_array_eq(sz, a1, a2, t, _)
#define core_array_equality___core__cmp__PartialEq__0___Slice_U____for__Array_T__N___3__eq( \
    sz, a1, a2, t, _, _ret_t)                                                               \
  Eurydice_array_eq(sz, a1, ((a2)->ptr), t, _)

#define Eurydice_slice_split_at(slice, mid, element_type, ret_t)               \
  KRML_CLITERAL(ret_t) {                                                       \
    EURYDICE_CFIELD(.fst =)                                                    \
    EURYDICE_SLICE((element_type *)(slice).ptr, 0, mid),                       \
        EURYDICE_CFIELD(.snd =)                                                \
            EURYDICE_SLICE((element_type *)(slice).ptr, mid, (slice).len)      \
  }

#define Eurydice_slice_split_at_mut(slice, mid, element_type, ret_t)           \
  KRML_CLITERAL(ret_t) {                                                       \
    EURYDICE_CFIELD(.fst =)                                                    \
    KRML_CLITERAL(Eurydice_slice){EURYDICE_CFIELD(.ptr =)(slice.ptr),          \
                                  EURYDICE_CFIELD(.len =) mid},                \
        EURYDICE_CFIELD(.snd =) KRML_CLITERAL(Eurydice_slice) {                \
      EURYDICE_CFIELD(.ptr =)                                                  \
      ((char *)slice.ptr + mid * sizeof(element_type)),                        \
          EURYDICE_CFIELD(.len =)(slice.len - mid)                             \
    }                                                                          \
  }

// Conversion of slice to an array, rewritten (by Eurydice) to name the
// destination array, since arrays are not values in C.
// N.B.: see note in karamel/lib/Inlining.ml if you change this.
#define Eurydice_slice_to_array2(dst, src, _0, t_arr, _1)                      \
  Eurydice_slice_to_array3(&(dst)->tag, (char *)&(dst)->val.case_Ok, src,      \
                           sizeof(t_arr))

static inline void Eurydice_slice_to_array3(uint8_t *dst_tag, char *dst_ok,
                                            Eurydice_slice src, size_t sz) {
  *dst_tag = 0;
  memcpy(dst_ok, src.ptr, sz);
}

// SUPPORT FOR DSTs (Dynamically-Sized Types)

// A DST is a fat pointer that keeps tracks of the size of it flexible array
// member. Slices are a specific case of DSTs, where [T; N] implements
// Unsize<[T]>, meaning an array of statically known size can be converted to a
// fat pointer, i.e. a slice.
//
// Unlike slices, DSTs have a built-in definition that gets monomorphized, of
// the form:
//
// typedef struct {
//   T *ptr;
//   size_t len; // number of elements
// } Eurydice_dst;
//
// Furthermore, T = T0<[U0]> where `struct T0<U: ?Sized>`, where the `U` is the
// last field. This means that there are two monomorphizations of T0 in the
// program. One is `T0<[V; N]>`
// -- this is directly converted to a Eurydice_dst via suitable codegen (no
// macro). The other is `T = T0<[U]>`, where `[U]` gets emitted to
// `Eurydice_derefed_slice`, a type that only appears in that precise situation
// and is thus defined to give rise to a flexible array member.

typedef char Eurydice_derefed_slice[];

#define Eurydice_slice_of_dst(fam_ptr, len_, t, _)                             \
  ((Eurydice_slice){.ptr = (void *)(fam_ptr), .len = len_})

#define Eurydice_slice_of_boxed_array(ptr_, len_, t, _)                        \
  ((Eurydice_slice){.ptr = (void *)(ptr_), .len = len_})

// CORE STUFF (conversions, endianness, ...)

// We slap extern "C" on declarations that intend to implement a prototype
// generated by Eurydice, because Eurydice prototypes are always emitted within
// an extern "C" block, UNLESS you use -fcxx17-compat, in which case, you must
// pass -DKRML_CXX17_COMPAT="" to your C++ compiler.
#if defined(__cplusplus) && !defined(KRML_CXX17_COMPAT)
extern "C" {
#endif

static inline void core_num__u32_8__to_be_bytes(uint32_t src, uint8_t dst[4]) {
  // TODO: why not store32_be?
  uint32_t x = htobe32(src);
  memcpy(dst, &x, 4);
}

static inline void core_num__u32_8__to_le_bytes(uint32_t src, uint8_t dst[4]) {
  store32_le(dst, src);
}

static inline uint32_t core_num__u32_8__from_le_bytes(uint8_t buf[4]) {
  return load32_le(buf);
}

static inline void core_num__u64_9__to_le_bytes(uint64_t v, uint8_t buf[8]) {
  store64_le(buf, v);
}

static inline uint64_t core_num__u64_9__from_le_bytes(uint8_t buf[8]) {
  return load64_le(buf);
}

static inline int64_t
core_convert_num___core__convert__From_i32__for_i64__59__from(int32_t x) {
  return x;
}

static inline uint64_t
core_convert_num___core__convert__From_u8__for_u64__66__from(uint8_t x) {
  return x;
}

static inline uint64_t
core_convert_num___core__convert__From_u16__for_u64__70__from(uint16_t x) {
  return x;
}

static inline size_t
core_convert_num___core__convert__From_u16__for_usize__96__from(uint16_t x) {
  return x;
}

static inline uint32_t core_num__u8_6__count_ones(uint8_t x0) {
#ifdef _MSC_VER
  return __popcnt(x0);
#else
  return __builtin_popcount(x0);
#endif
}

static inline uint32_t core_num__i32_2__count_ones(int32_t x0) {
#ifdef _MSC_VER
  return __popcnt(x0);
#else
  return __builtin_popcount(x0);
#endif
}

static inline size_t
core_cmp_impls___core__cmp__Ord_for_usize__59__min(size_t a, size_t b) {
  if (a <= b)
    return a;
  else
    return b;
}

// unsigned overflow wraparound semantics in C
static inline uint16_t core_num__u16_7__wrapping_add(uint16_t x, uint16_t y) {
  return x + y;
}
static inline uint8_t core_num__u8_6__wrapping_sub(uint8_t x, uint8_t y) {
  return x - y;
}
static inline uint64_t core_num__u64_9__rotate_left(uint64_t x0, uint32_t x1) {
  return (x0 << x1 | x0 >> (64 - x1));
}

static inline void core_ops_arith__i32_319__add_assign(int32_t *x0,
                                                       int32_t *x1) {
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
core_ops_bit___core__ops__bit__BitAnd_u8__u8__for___a__u8___46__bitand(
    uint8_t *x0, uint8_t x1) {
  return Eurydice_bitand_pv_u8(x0, x1);
}

static inline uint8_t
core_ops_bit___core__ops__bit__Shr_i32__u8__for___a__u8___792__shr(uint8_t *x0,
                                                                   int32_t x1) {
  return Eurydice_shr_pv_u8(x0, x1);
}

#define core_num_nonzero_private_NonZeroUsizeInner size_t
static inline core_num_nonzero_private_NonZeroUsizeInner
core_num_nonzero_private___core__clone__Clone_for_core__num__nonzero__private__NonZeroUsizeInner__26__clone(
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

#define core_iter_range___core__iter__traits__iterator__Iterator_A__for_core__ops__range__Range_A__TraitClause_0___6__next \
  Eurydice_range_iter_next

// See note in karamel/lib/Inlining.ml if you change this
#define Eurydice_into_iter(x, t, _ret_t, _) (x)
#define core_iter_traits_collect___core__iter__traits__collect__IntoIterator_Clause1_Item__I__for_I__1__into_iter \
  Eurydice_into_iter

typedef struct {
  Eurydice_slice slice;
  size_t chunk_size;
} Eurydice_chunks;

// Can't use macros Eurydice_slice_subslice_{to,from} because they require a
// type, and this static inline function cannot receive a type as an argument.
// Instead, we receive the element size and use it to peform manual offset
// computations rather than going through the macros.
static inline Eurydice_slice chunk_next(Eurydice_chunks *chunks,
                                        size_t element_size) {
  size_t chunk_size = chunks->slice.len >= chunks->chunk_size
                          ? chunks->chunk_size
                          : chunks->slice.len;
  Eurydice_slice curr_chunk;
  curr_chunk.ptr = chunks->slice.ptr;
  curr_chunk.len = chunk_size;
  chunks->slice.ptr = (char *)(chunks->slice.ptr) + chunk_size * element_size;
  chunks->slice.len = chunks->slice.len - chunk_size;
  return curr_chunk;
}

#define core_slice___Slice_T___chunks(slice_, sz_, t, _ret_t)                  \
  ((Eurydice_chunks){.slice = slice_, .chunk_size = sz_})
#define core_slice___Slice_T___chunks_exact(slice_, sz_, t, _ret_t)            \
  ((Eurydice_chunks){                                                          \
      .slice = {.ptr = slice_.ptr, .len = slice_.len - (slice_.len % sz_)},    \
      .chunk_size = sz_})
#define core_slice_iter_Chunks Eurydice_chunks
#define core_slice_iter_ChunksExact Eurydice_chunks
#define Eurydice_chunks_next(iter, t, ret_t)                                   \
  (((iter)->slice.len == 0) ? ((ret_t){.tag = core_option_None})               \
                            : ((ret_t){.tag = core_option_Some,                \
                                       .f0 = chunk_next(iter, sizeof(t))}))
#define core_slice_iter___core__iter__traits__iterator__Iterator_for_core__slice__iter__Chunks__a__T___70__next \
  Eurydice_chunks_next
// This name changed on 20240627
#define core_slice_iter___core__iter__traits__iterator__Iterator_for_core__slice__iter__Chunks__a__T___71__next \
  Eurydice_chunks_next
#define core_slice_iter__core__slice__iter__ChunksExact__a__T__89__next(       \
    iter, t, _ret_t)                                                           \
  core_slice_iter__core__slice__iter__Chunks__a__T__70__next(iter, t)

typedef struct {
  Eurydice_slice s;
  size_t index;
} Eurydice_slice_iterator;

#define core_slice___Slice_T___iter(x, t, _ret_t)                              \
  ((Eurydice_slice_iterator){.s = x, .index = 0})
#define core_slice_iter_Iter Eurydice_slice_iterator
#define core_slice_iter__core__slice__iter__Iter__a__T__181__next(iter, t,     \
                                                                  ret_t)       \
  (((iter)->index == (iter)->s.len)                                            \
       ? (KRML_CLITERAL(ret_t){.tag = core_option_None})                       \
       : (KRML_CLITERAL(ret_t){                                                \
             .tag = core_option_Some,                                          \
             .f0 = ((iter)->index++,                                           \
                    &((t *)((iter)->s.ptr))[(iter)->index - 1])}))
#define core_option__core__option__Option_T__TraitClause_0___is_some(X, _0,    \
                                                                     _1)       \
  ((X)->tag == 1)
// STRINGS

typedef const char *Prims_string;

// MISC (UNTESTED)

typedef void *core_fmt_Formatter;
typedef void *core_fmt_Arguments;
typedef void *core_fmt_rt_Argument;
#define core_fmt_rt__core__fmt__rt__Argument__a__1__new_display(x1, x2, x3,    \
                                                                x4)            \
  NULL

// BOXES

// Crimes.
static inline char *malloc_and_init(size_t sz, char *init) {
  char *ptr = (char *)malloc(sz);
  memcpy(ptr, init, sz);
  return ptr;
}

#define Eurydice_box_new(init, t, t_dst)                                       \
  ((t_dst)(malloc_and_init(sizeof(t), (char *)(&init))))

#define Eurydice_box_new_array(len, ptr, t, t_dst)                             \
  ((t_dst)(malloc_and_init(len * sizeof(t), (char *)(ptr))))

// VECTORS (ANCIENT, POSSIBLY UNTESTED)

/* For now these are passed by value -- three words. We could conceivably change
 * the representation to heap-allocate this struct and only pass around the
 * pointer (one word). */
typedef struct {
  void *ptr;
  size_t len;        /* the number of elements */
  size_t alloc_size; /* the size of the allocation, in number of BYTES */
} Eurydice_vec_s, *Eurydice_vec;

/* Here, we set everything to zero rather than use a non-standard GCC
 * statement-expression -- this suitably initializes ptr to NULL and len and
 * size to 0. */
#define EURYDICE_VEC_NEW(_) calloc(1, sizeof(Eurydice_vec_s))
#define EURYDICE_VEC_PUSH(v, x, t)                                             \
  do {                                                                         \
    /* Grow the vector if capacity has been reached. */                        \
    if (v->len == v->alloc_size / sizeof(t)) {                                 \
      /* Assuming that this does not exceed SIZE_MAX, because code proven      \
       * correct by Aeneas. Would this even happen in practice? */             \
      size_t new_size;                                                         \
      if (v->alloc_size == 0)                                                  \
        new_size = 8 * sizeof(t);                                              \
      else if (v->alloc_size <= SIZE_MAX / 2)                                  \
        /* TODO: discuss growth policy */                                      \
        new_size = 2 * v->alloc_size;                                          \
      else                                                                     \
        new_size = (SIZE_MAX / sizeof(t)) * sizeof(t);                         \
      v->ptr = realloc(v->ptr, new_size);                                      \
      v->alloc_size = new_size;                                                \
    }                                                                          \
    ((t *)v->ptr)[v->len] = x;                                                 \
    v->len++;                                                                  \
  } while (0)

#define EURYDICE_VEC_DROP(v, t)                                                \
  do {                                                                         \
    free(v->ptr);                                                              \
    free(v);                                                                   \
  } while (0)

#define EURYDICE_VEC_INDEX(v, i, t) &((t *)v->ptr)[i]
#define EURYDICE_VEC_LEN(v, t) (v)->len

/* TODO: remove GCC-isms */

#define EURYDICE_REPLACE(ptr, new_v, t)                                        \
  ({                                                                           \
    t old_v = *ptr;                                                            \
    *ptr = new_v;                                                              \
    old_v;                                                                     \
  })
