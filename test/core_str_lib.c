// This file provides the C implementation of the `core::str` Rust library.
// Notably, we assume `&str` to be represented by `Eurydice_str`.

#include <stddef.h>

// types

typedef struct Eurydice_str_s {
    char* data;
    size_t len;
} Eurydice_str;

// functions

size_t core_str__str__len(Eurydice_str s) {
    return s.len;
}
