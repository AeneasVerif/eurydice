/*
 * SPDX-FileCopyrightText: 2024-2025 Eurydice Contributors
 *
 * SPDX-License-Identifier: MIT or Apache-2.0
 *
 * Vectors.
 */

#ifndef EURYDICE_HEADER_VEC_H
#define EURYDICE_HEADER_VEC_H

#include "box.h"

// We adapt the layout of https://doc.rust-lang.org/std/vec/struct.Vec.html,
// dispensing with the nested RawVec -- basically, we follow what the
// documentation says. Just like Eurydice_slice, we keep sizes in number of
// elements. This means we pass three words by value whenever we carry a vector
// around. Things that modify the vector take &mut's in Rust, or a Eurydice_vec*
// in C.
//
// Another design choice: just like Eurydice_slice, we treat Eurydice_vec as an
// opaque type, and rely on macros receiving their type arguments at call-site
// to perform necessary casts. A downside is that anything that looks into the
// definition of Eurydice_vec must be exposed (from the eurydice point of view)
// as an external -- see, for instance, Eurydice_vec_failed, below.
typedef struct
{
    char *ptr;
    size_t len;      /* current length, in elements */
    size_t capacity; /* the size of the allocation, in number of elements */
} Eurydice_vec, alloc_vec_Vec;

// This is a helper that Eurydice has special knowledge about. Essentially,
// allocation functions return a result type that has been monomorphized, say,
// Result_XY; this means we need to do something like:
//   Eurydice_vec v = try_with_capacity(len, sz);
//   Result_XY r = v.ptr == NULL ? (Result_XY) { .tag = core_result_Ok, .case_Ok
//   = v }
//     : (Result_XY) { .tag = core_result_Error, .case_Error = ... };
// but with a macro (since we don't have templates).
// However, unless we allow statement-expressions (GCC extension), we cannot do
// the above with an expression, since we need to name the result of
// try_with_capacity to avoid evaluating it twice.
static inline Eurydice_vec Eurydice_vec_alloc2(size_t len, size_t element_sz)
{
    return ((Eurydice_vec){.ptr = (char *)EURYDICE_MALLOC(len * element_sz),
                           .len = len,
                           .capacity = len});
}

#define Eurydice_vec_alloc(len, t, _) (Eurydice_vec_alloc2((len), sizeof(t)))
#define Eurydice_vec_overflows(len, t, _) (!((len) <= SIZE_MAX / (sizeof(t))))
#define Eurydice_vec_failed(v, _, _1) ((v).ptr == NULL)
#define Eurydice_layout(t, _) \
    ((core_alloc_layout_Layout){.size = sizeof(t), .align = _Alignof(t)})

#define alloc_vec__alloc__vec__Vec_T___resize(                            \
    /* Eurydice_vec * */ v, /* size_t */ new_len, /* T */ elt, T, _0, _1) \
    do                                                                    \
    {                                                                     \
        if (new_len <= (v)->capacity)                                     \
            (v)->len = new_len;                                           \
        else                                                              \
        {                                                                 \
            (v)->ptr = EURYDICE_REALLOC((v)->ptr, new_len * sizeof(T));   \
            /* TODO: check success? Rust function is infallible */        \
            for (size_t i = (v)->len; i < new_len; i++)                   \
                ((T *)(v)->ptr)[i] = elt;                                 \
            (v)->len = new_len;                                           \
            (v)->capacity = new_len;                                      \
        }                                                                 \
    } while (0)

#define alloc_vec__alloc__vec__Vec_T___into_boxed_slice(/* Eurydice_vec */ v, \
                                                        T, _0, _1)            \
    ((Eurydice_slice){.ptr = (v).ptr, .len = (v).len})

#endif /* EURYDICE_HEADER_VEC_H */
