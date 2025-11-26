#pragma once

// Compatibility helpers for C++. This header can be safely omitted if you don't
// intend on generating code that ought to work with C++17 via the
// -fcxx17-compat flag.

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
