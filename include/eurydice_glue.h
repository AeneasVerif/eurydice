#pragma once

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define KRML_HOST_EXIT exit
#define KRML_HOST_EPRINTF(...) fprintf(stderr, __VA_ARGS__)

typedef struct {
  void *ptr;
  size_t len;
} Eurydice_slice;

#define EURYDICE_SLICE(x, start, end, t) ((Eurydice_slice){ .ptr = (void*)(x + start), .len = end - start })
#define EURYDICE_SLICE_LEN(s, _) s.len
#define Eurydice_slice_index(s, i, t) (((t*) s.ptr)[i])
#define Eurydice_slice_subslice(s, r, t, _) EURYDICE_SLICE(((t*)s.ptr), r.start, r.end, t)
#define Eurydice_array_to_slice(x, end, t) EURYDICE_SLICE(x, 0, end, t)
#define Eurydice_array_to_subslice(x, r, t, _) EURYDICE_SLICE(x, r.start, r.end, t)
#define core_slice___Slice_T___len(s, t) EURYDICE_SLICE_LEN(s, t)

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

#define LowStar_Ignore_ignore(e, t) ((void)e)

/* Support for iterators */

#define core_num_nonzero_NonZeroUsize size_t
#define core_iter_range__core__ops__range__Range_A__3__next(iter_ptr, t) ( \
  ((iter_ptr)->start == (iter_ptr)->end) ? \
    ((core_option_Option__##t) { .tag = core_option_None }) : \
    ((core_option_Option__##t) { .tag = core_option_Some, .f0 = (iter_ptr)->start++ }) \
  )

#define core_iter_traits_collect__I__into_iter(x, t) (x)
