/*
 * SPDX-FileCopyrightText: 2024-2025 Eurydice Contributors
 *
 * SPDX-License-Identifier: MIT or Apache-2.0
 *
 * This header implements functionality from Rust core, mapping these functions
 * to C.
 */

#ifndef EURYDICE_HEADER_CORE_H
#define EURYDICE_HEADER_CORE_H

#include <inttypes.h>
#include <string.h>

// Functions like `load16_le` are defined in this Karamel header.
#include "endianness.h"

#ifdef _MSC_VER
// For __popcnt
#include <intrin.h>
#endif

static inline uint16_t core_num__u16__from_le_bytes(uint8_t buf[2])
{
    return load16_le(buf);
}

static inline void core_num__u32__to_be_bytes(uint32_t src, uint8_t dst[4])
{
    // TODO: why not store32_be?
    uint32_t x = htobe32(src);
    memcpy(dst, &x, 4);
}

static inline void core_num__u32__to_le_bytes(uint32_t src, uint8_t dst[4])
{
    store32_le(dst, src);
}

static inline uint32_t core_num__u32__from_le_bytes(uint8_t buf[4])
{
    return load32_le(buf);
}

static inline void core_num__u64__to_le_bytes(uint64_t v, uint8_t buf[8])
{
    store64_le(buf, v);
}

static inline uint64_t core_num__u64__from_le_bytes(uint8_t buf[8])
{
    return load64_le(buf);
}

static inline int64_t
core_convert_num__core__convert__From_i32__for_i64__from(int32_t x)
{
    return x;
}

static inline uint64_t
core_convert_num__core__convert__From_u8__for_u64__from(uint8_t x)
{
    return x;
}

static inline uint64_t
core_convert_num__core__convert__From_u16__for_u64__from(uint16_t x)
{
    return x;
}

static inline size_t
core_convert_num__core__convert__From_u16__for_usize__from(uint16_t x)
{
    return x;
}

static inline uint32_t core_num__u8__count_ones(uint8_t x0)
{
#ifdef _MSC_VER
    return __popcnt(x0);
#else
    return __builtin_popcount(x0);
#endif
}

static inline uint32_t core_num__u32__count_ones(uint32_t x0)
{
#ifdef _MSC_VER
    return __popcnt(x0);
#else
    return __builtin_popcount(x0);
#endif
}

static inline uint32_t core_num__i32__count_ones(int32_t x0)
{
#ifdef _MSC_VER
    return __popcnt(x0);
#else
    return __builtin_popcount(x0);
#endif
}

static inline size_t core_cmp_impls__core__cmp__Ord_for_usize__min(size_t a,
                                                                   size_t b)
{
    if (a <= b)
        return a;
    else
        return b;
}

// unsigned overflow wraparound semantics in C
static inline uint8_t core_num__u8__wrapping_sub(uint8_t x, uint8_t y)
{
    return x - y;
}
static inline uint8_t core_num__u8__wrapping_add(uint8_t x, uint8_t y)
{
    return x + y;
}
static inline uint8_t core_num__u8__wrapping_mul(uint8_t x, uint8_t y)
{
    return x * y;
}
static inline uint16_t core_num__u16__wrapping_sub(uint16_t x, uint16_t y)
{
    return x - y;
}
static inline uint16_t core_num__u16__wrapping_add(uint16_t x, uint16_t y)
{
    return x + y;
}
static inline uint16_t core_num__u16__wrapping_mul(uint16_t x, uint16_t y)
{
    return x * y;
}
static inline uint32_t core_num__u32__wrapping_sub(uint32_t x, uint32_t y)
{
    return x - y;
}
static inline uint32_t core_num__u32__wrapping_add(uint32_t x, uint32_t y)
{
    return x + y;
}
static inline uint32_t core_num__u32__wrapping_mul(uint32_t x, uint32_t y)
{
    return x * y;
}
static inline uint64_t core_num__u64__wrapping_sub(uint64_t x, uint64_t y)
{
    return x - y;
}
static inline uint64_t core_num__u64__wrapping_add(uint64_t x, uint64_t y)
{
    return x + y;
}
static inline uint64_t core_num__u64__wrapping_mul(uint64_t x, uint64_t y)
{
    return x * y;
}
static inline size_t core_num__usize__wrapping_sub(size_t x, size_t y)
{
    return x - y;
}
static inline size_t core_num__usize__wrapping_add(size_t x, size_t y)
{
    return x + y;
}
static inline size_t core_num__usize__wrapping_mul(size_t x, size_t y)
{
    return x * y;
}

static inline uint64_t core_num__u64__rotate_left(uint64_t x0, uint32_t x1)
{
    return (x0 << x1) | (x0 >> ((-x1) & 63));
}

static inline void core_ops_arith__i32__add_assign(int32_t *x0, int32_t *x1)
{
    *x0 = *x0 + *x1;
}

static inline uint8_t Eurydice_bitand_pv_u8(uint8_t *p, uint8_t v)
{
    return (*p) & v;
}
static inline uint8_t Eurydice_shr_pv_u8(uint8_t *p, int32_t v)
{
    return (*p) >> v;
}
static inline uint32_t Eurydice_min_u32(uint32_t x, uint32_t y)
{
    return x < y ? x : y;
}

static inline uint8_t
core_ops_bit__core__ops__bit__BitAnd_u8__u8__for___a__u8___bitand(uint8_t *x0,
                                                                  uint8_t x1)
{
    return Eurydice_bitand_pv_u8(x0, x1);
}

static inline uint8_t
core_ops_bit__core__ops__bit__Shr_i32__u8__for___a__u8___shr(uint8_t *x0,
                                                             int32_t x1)
{
    return Eurydice_shr_pv_u8(x0, x1);
}

#define core_num_nonzero_private_NonZeroUsizeInner size_t
static inline core_num_nonzero_private_NonZeroUsizeInner
core_num_nonzero_private___core__clone__Clone_for_core__num__nonzero__private__NonZeroUsizeInner___clone(
    core_num_nonzero_private_NonZeroUsizeInner *x0)
{
    return *x0;
}

#endif /* EURYDICE_HEADER_CORE_H */
