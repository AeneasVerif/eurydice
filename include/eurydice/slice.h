/*
 * SPDX-FileCopyrightText: 2024-2025 Eurydice Contributors
 *
 * SPDX-License-Identifier: MIT or Apache-2.0
 *
 * This header implements `Eurydice_slice` and functions on it to reflect
 * Rust slices in C.
 */

#ifndef EURYDICE_HEADER_SLICE_H
#define EURYDICE_HEADER_SLICE_H

#include <stddef.h>
#include <inttypes.h>
#include <string.h>

// We represent a slice as a pair of an (untyped) pointer, along with the length
// of the slice, i.e. the number of elements in the slice (this is NOT the
// number of bytes). This design choice has two important consequences.
// - if you need to use `ptr`, you MUST cast it to a proper type *before*
//   performing pointer arithmetic on it (remember that C desugars pointer
//   arithmetic based on the type of the address)
// - if you need to use `len` for a C style function (e.g. memcpy, memcmp), you
//   need to multiply it by sizeof t, where t is the type of the elements.
//
// Empty slices have `len == 0` and `ptr` always needs to be a valid pointer
// that is not NULL (otherwise the construction in EURYDICE_SLICE computes `NULL
// + start`). 20250714: this is fine since C23.
typedef struct
{
    void *ptr;
    size_t len;
} Eurydice_slice;

#if defined(__cplusplus)
#define KRML_CLITERAL(type) type
#else
#define KRML_CLITERAL(type) (type)
#endif

#if defined(__cplusplus) && defined(__cpp_designated_initializers) || \
    !(defined(__cplusplus))
#define EURYDICE_CFIELD(X) X
#else
#define EURYDICE_CFIELD(X)
#endif

// Helper macro to create a slice out of a pointer x, a start index in x
// (included), and an end index in x (excluded). The argument x must be suitably
// cast to something that can decay (see remark above about how pointer
// arithmetic works in C), meaning either pointer or array type.
#define EURYDICE_SLICE(x, start, end) \
    (KRML_CLITERAL(Eurydice_slice){(void *)(x + start), end - (start)})

// Slice length
#define Eurydice_slice_len(s, _) (s).len

// This macro is a pain because in case the dereferenced element type is an
// array, you cannot simply write `t x` as it would yield `int[4] x` instead,
// which is NOT correct C syntax, so we add a dedicated phase in Eurydice that
// adds an extra argument to this macro at the last minute so that we have the
// correct type of *pointers* to elements.
#define Eurydice_slice_index(s, i, t, t_ptr_t) (((t_ptr_t)s.ptr)[i])

// The following functions get sub slices from a slice.

#define Eurydice_slice_subslice(s, r, t, _0, _1) \
    EURYDICE_SLICE((t *)s.ptr, r.start, r.end)

// Variant for when the start and end indices are statically known (i.e., the
// range argument `r` is a literal).
#define Eurydice_slice_subslice2(s, start, end, t) \
    EURYDICE_SLICE((t *)s.ptr, (start), (end))

// Previous version above does not work when t is an array type (as usual). Will
// be deprecated soon.
#define Eurydice_slice_subslice3(s, start, end, t_ptr) \
    EURYDICE_SLICE((t_ptr)s.ptr, start, end)

#define Eurydice_slice_subslice_to(s, subslice_end_pos, t, _0, _1) \
    EURYDICE_SLICE((t *)s.ptr, 0, subslice_end_pos)

#define Eurydice_slice_subslice_from(s, subslice_start_pos, t, _0, _1) \
    EURYDICE_SLICE((t *)s.ptr, subslice_start_pos, s.len)

#define Eurydice_array_to_slice(end, x, t) \
    EURYDICE_SLICE(x, 0,                   \
                   end) /* x is already at an array type, no need for cast */
#define Eurydice_array_to_subslice(_arraylen, x, r, t, _0, _1) \
    EURYDICE_SLICE((t *)x, r.start, r.end)

// Same as above, variant for when start and end are statically known
#define Eurydice_array_to_subslice3(x, start, end, t_ptr) \
    EURYDICE_SLICE((t_ptr)x, start, end)

#define Eurydice_array_repeat(dst, len, init, t) \
    ERROR "should've been desugared"

// The following functions convert an array into a slice.

#define Eurydice_array_to_subslice_to(_size, x, r, t, _range_t, _0) \
    EURYDICE_SLICE((t *)x, 0, r)
#define Eurydice_array_to_subslice_from(size, x, r, t, _range_t, _0) \
    EURYDICE_SLICE((t *)x, r, size)

// Copy a slice with memcopy
#define Eurydice_slice_copy(dst, src, t) \
    memcpy(dst.ptr, src.ptr, dst.len * sizeof(t))

// Distinguished support for some PartialEq trait implementations
//
#define Eurydice_slice_eq(src1, src2, t, _) \
    ((src1)->len == (src2)->len &&          \
     !memcmp((src1)->ptr, (src2)->ptr, (src1)->len * sizeof(t)))

#define core_array___Array_T__N___as_slice(len_, ptr_, t, _ret_t) \
    KRML_CLITERAL(Eurydice_slice) { ptr_, len_ }

#define core_array__core__clone__Clone_for__Array_T__N___clone( \
    len, src, dst, elem_type, _ret_t)                           \
    (memcpy(dst, src, len * sizeof(elem_type)))
#define TryFromSliceError uint8_t
#define core_array_TryFromSliceError uint8_t

// Distinguished support for some PartialEq trait implementations
//
// core::cmp::PartialEq<@Array<U, N>> for @Array<T, N>
#define Eurydice_array_eq(sz, a1, a2, t) \
    (memcmp(a1, a2, sz * sizeof(t)) == 0)
// core::cmp::PartialEq<&0 (@Slice<U>)> for @Array<T, N>
#define Eurydice_array_eq_slice(sz, a1, s2, t, _) \
    (memcmp(a1, (s2)->ptr, sz * sizeof(t)) == 0)

// DEPRECATED -- should no longer be generated
#define core_array_equality__core__cmp__PartialEq__Array_U__N___for__Array_T__N___eq( \
    sz, a1, a2, t, _, _ret_t)                                                         \
    Eurydice_array_eq(sz, a1, a2, t)
#define core_array_equality__core__cmp__PartialEq__0___Slice_U____for__Array_T__N___eq( \
    sz, a1, a2, t, _, _ret_t)                                                           \
    Eurydice_array_eq(sz, a1, ((a2)->ptr), t)
#define core_cmp_impls__core__cmp__PartialEq__0_mut__B___for__1_mut__A___eq( \
    _m0, _m1,                                                                \
    src1, src2, _0, _1, T)                                                   \
    Eurydice_slice_eq(src1, src2, _, _, T, _)

#define Eurydice_slice_split_at(slice, mid, element_type, ret_t)              \
    KRML_CLITERAL(ret_t)                                                      \
    {                                                                         \
        EURYDICE_CFIELD(.fst =)                                               \
        EURYDICE_SLICE((element_type *)(slice).ptr, 0, mid),                  \
            EURYDICE_CFIELD(.snd =)                                           \
                EURYDICE_SLICE((element_type *)(slice).ptr, mid, (slice).len) \
    }

#define Eurydice_slice_split_at_mut(slice, mid, element_type, ret_t)      \
    KRML_CLITERAL(ret_t)                                                  \
    {                                                                     \
        EURYDICE_CFIELD(.fst =)                                           \
        KRML_CLITERAL(Eurydice_slice){EURYDICE_CFIELD(.ptr =)(slice.ptr), \
                                      EURYDICE_CFIELD(.len =) mid},       \
            EURYDICE_CFIELD(.snd =) KRML_CLITERAL(Eurydice_slice)         \
        {                                                                 \
            EURYDICE_CFIELD(.ptr =)                                       \
            ((char *)slice.ptr + mid * sizeof(element_type)),             \
                EURYDICE_CFIELD(.len =)(slice.len - (mid))                \
        }                                                                 \
    }

// Conversion of slice to an array, rewritten (by Eurydice) to name the
// destination array, since arrays are not values in C.
// N.B.: see note in karamel/lib/Inlining.ml if you change this.
#define Eurydice_slice_to_array2(dst, src, _0, t_arr, _1)                   \
    Eurydice_slice_to_array3(&(dst)->tag, (char *)&(dst)->val.case_Ok, src, \
                             sizeof(t_arr))

#define Eurydice_slice_to_ref_array(len_, src, t_ptr, t_arr, t_err, t_res) \
    (src.len >= len_                                                       \
         ? ((t_res){.tag = core_result_Ok, .val = {.case_Ok = src.ptr}})   \
         : ((t_res){.tag = core_result_Err, .val = {.case_Err = 0}}))

static inline void Eurydice_slice_to_array3(uint8_t *dst_tag, char *dst_ok,
                                            Eurydice_slice src, size_t sz)
{
    *dst_tag = 0;
    memcpy(dst_ok, src.ptr, sz);
}

#endif /* EURYDICE_HEADER_SLICE_H */
