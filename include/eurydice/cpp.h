/*
 * SPDX-FileCopyrightText: 2024-2025 Eurydice Contributors
 *
 * SPDX-License-Identifier: MIT or Apache-2.0
 * 
 * Include this header when compiling Eurydice compiled code with
 * C++.
 * 
 * In order to be compatible with C++17 we need to work around the fact that we
 * can not use designated initializers. This is an issue for unions. The
 * folowing defines `KRML_UNION_CONSTRUCTOR` for this purpose.
 * This is used in union definions such as
 * ```c
 * typedef struct Result_s {
 *  uint8_t tag;
 *  union U {
 *    int16_t case_Ok[16U];
 *    uint8_t case_Err;
 *  } val;
 *  KRML_UNION_CONSTRUCTOR(Result_s)
 * } Result_0a;
 * ```
 */

#ifndef EURYDICE_HEADER_CPP_H
#define EURYDICE_HEADER_CPP_H

#include <utility>

#ifndef KRML_HOST_EPRINTF
#define KRML_HOST_EPRINTF(...) fprintf(stderr, __VA_ARGS__)
#endif

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

#endif /* EURYDICE_HEADER_CPP_H */
