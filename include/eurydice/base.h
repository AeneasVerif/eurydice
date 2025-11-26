#pragma once

// Base macros emitted by the code-gen -- this file should always be included,
// unless you want to reimplement some stuff by hand.

// Eurydice emits type `bool`
#include <stdbool.h>
// Eurydice may emit the KRML_FOR_* series of macros
#include <krml/internal/target.h>

#include <stdio.h>

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

#define Eurydice_sizeof(t) sizeof(t)

#define Eurydice_alignof(t) alignof(t)

// MACROS EXPECTED BY CODE-GEN

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
