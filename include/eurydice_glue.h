#pragma once

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

// SYSTEM DEFINITIONS (taken from krml...)

#define KRML_HOST_EXIT exit
#define KRML_HOST_EPRINTF(...) fprintf(stderr, __VA_ARGS__)

#define LowStar_Ignore_ignore(e, t) ((void)e)

#if defined(__linux__) || defined(__CYGWIN__) || defined (__USE_SYSTEM_ENDIAN_H__) || defined(__GLIBC__)
#  include <endian.h>

/* ... for OSX */
#elif defined(__APPLE__)
#  include <libkern/OSByteOrder.h>
#  define htole64(x) OSSwapHostToLittleInt64(x)
#  define le64toh(x) OSSwapLittleToHostInt64(x)
#  define htobe64(x) OSSwapHostToBigInt64(x)
#  define be64toh(x) OSSwapBigToHostInt64(x)

#  define htole16(x) OSSwapHostToLittleInt16(x)
#  define le16toh(x) OSSwapLittleToHostInt16(x)
#  define htobe16(x) OSSwapHostToBigInt16(x)
#  define be16toh(x) OSSwapBigToHostInt16(x)

#  define htole32(x) OSSwapHostToLittleInt32(x)
#  define le32toh(x) OSSwapLittleToHostInt32(x)
#  define htobe32(x) OSSwapHostToBigInt32(x)
#  define be32toh(x) OSSwapBigToHostInt32(x)
#endif


// SLICES, ARRAYS, ETC.

// We represent a slice as a pair of an (untyped) pointer, along with the length of the slice, i.e.
// the number of elements in the slice (this is NOT the number of bytes). This design choice has two
// important consequences.
// - if you need to use `ptr`, you MUST cast it to a proper type *before* performing pointer
//   arithmetic on it (remember that C desugars pointer arithmetic based on the type of the address)
// - if you need to use `len` for a C style function (e.g. memcpy, memcmp), you need to multiply it
//   by sizeof t, where t is the type of the elements.
typedef struct {
  void *ptr;
  size_t len;
} Eurydice_slice;

// Helper macro to create a slice out of a pointer x, a start index in x (included), and an end
// index in x (excluded). The argument x must be suitably cast to something that can decay (see
// remark above about how pointer arithmetic works in C), meaning either pointer or array type.
#define EURYDICE_SLICE(x, start, end) ((Eurydice_slice){ .ptr = (void*)(x + start), .len = end - start })
#define EURYDICE_SLICE_LEN(s, _) s.len
#define Eurydice_slice_index(s, i, t) (((t*) s.ptr)[i])
#define Eurydice_slice_subslice(s, r, t, _) EURYDICE_SLICE((t*)s.ptr, r.start, r.end)
#define Eurydice_slice_subslice_to(s, subslice_end_pos, t, _) EURYDICE_SLICE((t*)s.ptr, 0, subslice_end_pos)
#define Eurydice_slice_subslice_from(s, subslice_start_pos, t, _) EURYDICE_SLICE((t*)s.ptr, subslice_start_pos, s.len)
#define Eurydice_array_to_slice(end, x, t) EURYDICE_SLICE(x, 0, end) /* x is already at an array type, no need for cast */
#define Eurydice_array_to_subslice(_arraylen, x, r, t, _) EURYDICE_SLICE((t*)x, r.start, r.end)
#define Eurydice_array_to_subslice_to(_size, x, r, t, _range_t) EURYDICE_SLICE((t*)x, 0, r)
#define Eurydice_array_to_subslice_from(size, x, r, t, _range_t) EURYDICE_SLICE((t*)x, r, size)
#define Eurydice_array_repeat(dst, len, init, t) ERROR "should've been desugared"
#define core_slice___Slice_T___len(s, t) EURYDICE_SLICE_LEN(s, t)
#define core_slice___Slice_T___copy_from_slice(dst, src, t) memcpy(dst.ptr, src.ptr, dst.len * sizeof(t))
#define core_array___Array_T__N__23__as_slice(len_, ptr_, t) ((Eurydice_slice){ .ptr = ptr_, .len = len_ })

#define core_array_TryFromSliceError uint8_t

#define core_array_equality___Array_A__N___eq(sz, a1, a2, t, _) (memcmp(a1, a2, sz * sizeof(t)) == 0)

#define core_slice___Slice_T___split_at(slice, mid, element_type) \
  ((K___Eurydice_slice_##element_type##_Eurydice_slice_##element_type){ \
    .fst = EURYDICE_SLICE((element_type*)slice.ptr, 0, mid), \
    .snd = EURYDICE_SLICE((element_type*)slice.ptr, mid, slice.len)})

// Can't have a flexible array as a member of a union -- this violates strict aliasing rules.
typedef struct
{
  uint8_t tag;
  uint8_t case_Ok[];
}
result_tryfromslice_flexible;

#define Eurydice_slice_to_array2(dst, src, _, t_arr) Eurydice_slice_to_array3((result_tryfromslice_flexible *)dst, src, sizeof(t_arr))

static inline void Eurydice_slice_to_array3(result_tryfromslice_flexible *dst, Eurydice_slice src, size_t sz) {
  dst->tag = 0;
  memcpy(dst->case_Ok, src.ptr, sz);
}

// CORE STUFF (conversions, endianness, ...)

static inline void core_num__u32_8__to_be_bytes(uint32_t src, uint8_t dst[4]) {
  uint32_t x = htobe32(src);
  memcpy(dst, &x, 4);
}

static inline int64_t core_convert_num__i64_59__from(int32_t x) { return x; }
static inline int32_t core_convert_num__i32_56__from(int16_t x) { return x; }
// unsigned overflow wraparound semantics in C
static inline uint16_t core_num__u16_7__wrapping_add(uint16_t x, uint16_t y) { return x + y; }
static inline uint8_t core_num__u8_6__wrapping_sub(uint8_t x, uint8_t y) { return x - y; }

static inline void core_ops_arith__i32_319__add_assign(int32_t *x0, int32_t *x1) {
  *x0 = *x0 + *x1;
}

static inline uint8_t Eurydice_bitand_pv_u8(uint8_t *p, uint8_t v) { return (*p) & v; }
static inline uint8_t Eurydice_shr_pv_u8(uint8_t *p, int32_t v) { return (*p) >> v; }

static uint32_t core_num__u32_8__BITS = 32;

// ITERATORS

#define core_num_nonzero_NonZeroUsize size_t
#define core_iter_range__core__ops__range__Range_A__3__next(iter_ptr, t) ( \
  ((iter_ptr)->start == (iter_ptr)->end) ? \
    ((core_option_Option__##t) { .tag = core_option_None }) : \
    ((core_option_Option__##t) { .tag = core_option_Some, .f0 = (iter_ptr)->start++ }) \
  )

#define core_iter_traits_collect__I__into_iter(x, t) (x)

typedef struct {
  Eurydice_slice slice;
  size_t chunk_size;
} Eurydice_chunks;


// Can't use macros Eurydice_slice_subslice_{to,from} because they require a type, and this static
// inline function cannot receive a type as an argument. Instead, we receive the element size and
// use it to peform manual offset computations rather than going through the macros.
static inline Eurydice_slice chunk_next(Eurydice_chunks *chunks, size_t element_size) {
  size_t chunk_size = chunks->slice.len >= chunks->chunk_size ? chunks->chunk_size : chunks->slice.len;
  Eurydice_slice curr_chunk = ((Eurydice_slice) { .ptr = chunks->slice.ptr, .len = chunk_size });
  chunks->slice = ((Eurydice_slice) {
    .ptr = (char *)(chunks->slice.ptr) + chunk_size * element_size,
    .len = chunks->slice.len - chunk_size
  });
  return curr_chunk;
}

#define core_slice___Slice_T___chunks(slice_, sz_, t) ((Eurydice_chunks){ .slice = slice_, .chunk_size = sz_ })
#define core_slice___Slice_T___chunks_exact(slice_, sz_, t) ((Eurydice_chunks){ \
    .slice = { .ptr = slice_.ptr, .len = slice_.len - (slice_.len % sz_) }, \
    .chunk_size = sz_ })
#define core_slice_iter_Chunks Eurydice_chunks
#define core_slice_iter_ChunksExact Eurydice_chunks
#define core_slice_iter__core__slice__iter__Chunks__a__T__70__next(iter, t) \
  (((iter)->slice.len == 0) ? \
    ((core_option_Option__Eurydice_slice_##t) { .tag = core_option_None }) : \
    ((core_option_Option__Eurydice_slice_##t){ \
       .tag = core_option_Some, \
       .f0 = chunk_next(iter, sizeof(t)) }))
#define core_slice_iter__core__slice__iter__ChunksExact__a__T__89__next(iter, t) \
  core_slice_iter__core__slice__iter__Chunks__a__T__70__next(iter, t)

typedef struct {
  Eurydice_slice s;
  size_t index;
} Eurydice_slice_iterator;

#define core_slice___Slice_T___iter(x, t) ((Eurydice_slice_iterator){ .s = x, .index = 0 })
#define core_slice_iter_Iter Eurydice_slice_iterator
#define core_slice_iter__core__slice__iter__Iter__a__T__181__next(iter, t) \
  (((iter)->index == (iter)->s.len) ? \
    ((core_option_Option___##t##_) { .tag = core_option_None }) : \
    ((core_option_Option___##t##_){ \
       .tag = core_option_Some, \
       .f0 = ((iter)->index++, &((t*)((iter)->s.ptr))[(iter)->index-1]) }))

// MISC

#define core_fmt_Formatter void


// VECTORS

/* For now these are passed by value -- three words. We could conceivably change
 * the representation to heap-allocate this struct and only pass around the
 * pointer (one word). */
typedef struct {
  void *ptr;
  size_t len; /* the number of elements */
  size_t alloc_size; /* the size of the allocation, in number of BYTES */
} Eurydice_vec_s, *Eurydice_vec;

/* Here, we set everything to zero rather than use a non-standard GCC
 * statement-expression -- this suitably initializes ptr to NULL and len and
 * size to 0. */
#define EURYDICE_VEC_NEW(_) calloc(1, sizeof(Eurydice_vec_s))
#define EURYDICE_VEC_PUSH(v, x, t) \
  do { \
    /* Grow the vector if capacity has been reached. */ \
    if (v->len == v->alloc_size/sizeof(t)) { \
      /* Assuming that this does not exceed SIZE_MAX, because code proven \
       * correct by Aeneas. Would this even happen in practice? */ \
      size_t new_size; \
      if (v->alloc_size == 0) \
        new_size = 8 * sizeof(t); \
      else if (v->alloc_size <= SIZE_MAX/2) \
        /* TODO: discuss growth policy */ \
        new_size = 2 * v->alloc_size; \
      else \
        new_size = (SIZE_MAX/sizeof(t))*sizeof(t); \
      v->ptr = realloc(v->ptr, new_size); \
      v->alloc_size = new_size; \
    } \
    ((t*)v->ptr)[v->len] = x; \
    v->len++; \
  } while (0)

#define EURYDICE_VEC_DROP(v, t) \
  do { \
    free(v->ptr); \
    free(v); \
  } while (0)

#define EURYDICE_VEC_INDEX(v, i, t) &((t*) v->ptr)[i]
#define EURYDICE_VEC_LEN(v, t) (v)->len

/* TODO: remove GCC-isms */
#define EURYDICE_BOX_NEW(x, t) ({ t *p = malloc(sizeof(t)); *p = x; p; })

#define EURYDICE_REPLACE(ptr, new_v, t) ({ t old_v = *ptr; *ptr = new_v; old_v; })

