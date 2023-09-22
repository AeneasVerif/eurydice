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
#define EURYDICE_SLICE_INDEX(s, i, t) (((t*) s.ptr)[i])
#define EURYDICE_SLICE_SUBSLICE(s, r, t) EURYDICE_SLICE(((t*)s.ptr), r.start, r.end, t)
#define EURYDICE_ARRAY_TO_SLICE(x, end, t) EURYDICE_SLICE(x, 0, end, t)
#define EURYDICE_ARRAY_TO_SUBSLICE(x, r, t) EURYDICE_SLICE(x, r.start, r.end, t)
#define CORE_SLICE__T__0_LEN(s, t) EURYDICE_SLICE_LEN(s, t)

/* For now these are passed by value -- three words. We could conceivably change
 * the representation to heap-allocate this struct and only pass around the
 * pointer (one word). */
typedef struct {
  void *ptr;
  size_t len;
  size_t size;
} *Eurydice_vec;

/* Here, we set everything to zero rather than use a non-standard GCC
 * statement-expression -- this suitably initializes ptr to NULL and len and
 * size to 0. */
#define EURYDICE_VEC_NEW(_) calloc(1, sizeof(Eurydice_vec))
#define EURYDICE_VEC_PUSH(v, x, t) \
  do { \
    /* Grow the vector if capacity has been reached. */ \
    if (v->len == v->size) { \
      /* Assuming that this does not exceed SIZE_MAX, because code proven \
       * correct by Aeneas. Would this even happen in practice? */ \
      size_t new_size; \
      if (v->size == 0) \
        new_size = 8 * sizeof(t); \
      else if (v->size <= SIZE_MAX/2) \
        /* TODO: discuss growth policy */ \
        new_size = 2 * v->size; \
      else \
        new_size = SIZE_MAX; \
      v->ptr = realloc(v->ptr, new_size); \
      v->size = new_size / sizeof(t); \
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
