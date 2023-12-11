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
// index in x (excluded). The macro also needs the type of the *elements* in order to perform
// suitable pointer casts (see remark above about how pointer arithmetic works in C).
#define EURYDICE_SLICE(x, start, end, t) ((Eurydice_slice){ .ptr = (void*)((t*)x + start), .len = end - start })
#define EURYDICE_SLICE_LEN(s, _) s.len
#define Eurydice_slice_index(s, i, t) (((t*) s.ptr)[i])
#define Eurydice_slice_subslice(s, r, t, _) EURYDICE_SLICE(s.ptr, r.start, r.end, t)
#define Eurydice_slice_subslice_from(s, subslice_start_pos, t, _) EURYDICE_SLICE(s.ptr, subslice_start_pos, s.len, t)
#define Eurydice_array_to_slice(end, x, t) EURYDICE_SLICE(x, 0, end, t)
#define Eurydice_array_to_subslice(_arraylen, x, r, t, _) EURYDICE_SLICE(x, r.start, r.end, t)
#define Eurydice_array_to_subslice_to(_size, x, r, t, _range_t) EURYDICE_SLICE(x, 0, r, t)
#define Eurydice_array_to_subslice_from(size, x, r, t, _range_t) EURYDICE_SLICE(x, r, size, t)
#define Eurydice_array_repeat(dst, len, init, t) ERROR "should've been desugared"
#define core_slice___Slice_T___len(s, t) EURYDICE_SLICE_LEN(s, t)
#define core_slice___Slice_T___copy_from_slice(dst, src, t) memcpy(dst.ptr, src.ptr, dst.len * sizeof(t))
#define core_array___Array_T__N__23__as_slice(len, ptr, t) ((Eurydice_slice){ .ptr = ptr, .len = len })

#define core_array_TryFromSliceError uint8_t

#define core_array_equality___Array_A__N___eq(sz, a1, a2, t, _) (memcmp(a1, a2, sz * sizeof(t)) == 0)

#define core_slice___Slice_T___split_at(slice, mid, element_type) \
  ((K___Eurydice_slice_##element_type##_Eurydice_slice_##element_type){ \
    .fst = EURYDICE_SLICE(slice.ptr, 0, mid, element_type), \
    .snd = EURYDICE_SLICE(slice.ptr, mid, slice.len, element_type)})


// CORE STUFF (conversions, endianness, ...)

static inline void core_num__u32_8__to_be_bytes(uint32_t src, uint8_t dst[4]) {
  uint32_t x = htobe32(src);
  memcpy(dst, &x, 4);
}

static inline int32_t core_convert_num__i32_56__from(int16_t x) { return x; }


// ITERATORS

#define core_num_nonzero_NonZeroUsize size_t
#define core_iter_range__core__ops__range__Range_A__3__next(iter_ptr, t) ( \
  ((iter_ptr)->start == (iter_ptr)->end) ? \
    ((core_option_Option__##t) { .tag = core_option_None }) : \
    ((core_option_Option__##t) { .tag = core_option_Some, .f0 = (iter_ptr)->start++ }) \
  )

#define core_iter_traits_collect__I__into_iter(x, t) (x)


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

