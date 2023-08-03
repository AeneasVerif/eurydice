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

#define EURYDICE_SLICE_NEW(x, start, end, t) ((Eurydice_slice){ .ptr = (void*)(x + start), .len = end - start })
#define EURYDICE_SLICE_LEN(s, _) s.len
#define EURYDICE_SLICE_INDEX(s, i, t) (((t*) s.ptr)[i])
#define EURYDICE_SLICE_NEW_WITH_RANGE(x, r, tarr, t) \
  ((Eurydice_slice){ .ptr = (void*)(x + r.start), .len = r.end - r.start })
