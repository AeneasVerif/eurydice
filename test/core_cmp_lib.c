// this file provides the comparison implementations for various built-in types

#include "eurydice_glue.h"

// the implementation for `()` comparison -- the inputs are `&()`
// as the function interface is `&self`
bool core_cmp_impls__core__cmp__PartialEq_____for_____eq(void **x0, void **x1) {
  return true;
}

// same implementation for the monomorphized LLBC
bool core_cmp_impls__core__cmp__PartialEq____________eq(void **x0, void **x1) {
  return true;
}
