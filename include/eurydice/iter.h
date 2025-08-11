/*
 * SPDX-FileCopyrightText: 2024-2025 Eurydice Contributors
 *
 * SPDX-License-Identifier: MIT or Apache-2.0
 *
 * This header implements iterator patterns from Rust.
 */

#ifndef EURYDICE_HEADER_ITER_H
#define EURYDICE_HEADER_ITER_H

#include "slice.h"

#define Eurydice_range_iter_next(iter_ptr, t, ret_t)        \
    (((iter_ptr)->start >= (iter_ptr)->end)                 \
         ? (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.tag =) 0, \
                                 EURYDICE_CFIELD(.f0 =) 0}) \
         : (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.tag =) 1, \
                                 EURYDICE_CFIELD(.f0 =)(iter_ptr)->start++}))

#define core_iter_range__core__iter__traits__iterator__Iterator_A__for_core__ops__range__Range_A__TraitClause_0___next \
    Eurydice_range_iter_next

// See note in karamel/lib/Inlining.ml if you change this
#define Eurydice_into_iter(x, t, _ret_t, _) (x)
#define core_iter_traits_collect__core__iter__traits__collect__IntoIterator_Clause1_Item__I__for_I__into_iter \
    Eurydice_into_iter

typedef struct
{
    Eurydice_slice slice;
    size_t chunk_size;
} Eurydice_chunks;

// Can't use macros Eurydice_slice_subslice_{to,from} because they require a
// type, and this static inline function cannot receive a type as an argument.
// Instead, we receive the element size and use it to peform manual offset
// computations rather than going through the macros.
static inline Eurydice_slice chunk_next(Eurydice_chunks *chunks,
                                        size_t element_size)
{
    size_t chunk_size = chunks->slice.len >= chunks->chunk_size
                            ? chunks->chunk_size
                            : chunks->slice.len;
    Eurydice_slice curr_chunk;
    curr_chunk.ptr = chunks->slice.ptr;
    curr_chunk.len = chunk_size;
    chunks->slice.ptr = (char *)(chunks->slice.ptr) + chunk_size * element_size;
    chunks->slice.len = chunks->slice.len - chunk_size;
    return curr_chunk;
}

#define core_slice___Slice_T___chunks(slice_, sz_, t, _ret_t) \
    ((Eurydice_chunks){.slice = slice_, .chunk_size = sz_})
#define core_slice___Slice_T___chunks_exact(slice_, sz_, t, _ret_t)           \
    ((Eurydice_chunks){                                                       \
        .slice = {.ptr = slice_.ptr, .len = slice_.len - (slice_.len % sz_)}, \
        .chunk_size = sz_})
#define core_slice_iter_Chunks Eurydice_chunks
#define core_slice_iter_ChunksExact Eurydice_chunks
#define Eurydice_chunks_next(iter, t, ret_t)                       \
    (((iter)->slice.len == 0) ? ((ret_t){.tag = core_option_None}) \
                              : ((ret_t){.tag = core_option_Some,  \
                                         .f0 = chunk_next(iter, sizeof(t))}))
#define core_slice_iter__core__iter__traits__iterator__Iterator_for_core__slice__iter__Chunks__a__T___next \
    Eurydice_chunks_next
// This name changed on 20240627
#define core_slice_iter__core__iter__traits__iterator__Iterator_for_core__slice__iter__Chunks__a__T___next \
    Eurydice_chunks_next
#define core_slice_iter__core__slice__iter__ChunksExact__a__T__89__next( \
    iter, t, _ret_t)                                                     \
    core_slice_iter__core__slice__iter__Chunks__a__T__70__next(iter, t)

typedef struct
{
    Eurydice_slice s;
    size_t index;
} Eurydice_slice_iterator;

#define core_slice___Slice_T___iter(x, t, _ret_t) \
    ((Eurydice_slice_iterator){.s = x, .index = 0})
#define core_slice_iter_Iter Eurydice_slice_iterator
#define core_slice_iter__core__slice__iter__Iter__a__T__next(iter, t, ret_t) \
    (((iter)->index == (iter)->s.len)                                        \
         ? (KRML_CLITERAL(ret_t){.tag = core_option_None})                   \
         : (KRML_CLITERAL(ret_t){                                            \
               .tag = core_option_Some,                                      \
               .f0 = ((iter)->index++,                                       \
                      &((t *)((iter)->s.ptr))[(iter)->index - 1])}))
#define core_option__core__option__Option_T__TraitClause_0___is_some(X, _0, \
                                                                     _1)    \
    ((X)->tag == 1)

#endif /* EURYDICE_HEADER_ITER_H */
