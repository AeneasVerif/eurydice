/*
 * SPDX-FileCopyrightText: 2024-2025 Eurydice Contributors
 *
 * SPDX-License-Identifier: MIT or Apache-2.0
 *
 * Things required when writing unsafe Rust code.
 */

#ifndef EURYDICE_HEADER_UNSAFE_H
#define EURYDICE_HEADER_UNSAFE_H

#define core_slice___Slice_T___as_mut_ptr(x, t, _) (x.ptr)
#define core_mem_size_of(t, _) (sizeof(t))
#define core_slice_raw_from_raw_parts_mut(ptr, len, _0, _1) \
    (KRML_CLITERAL(Eurydice_slice){(void *)(ptr), len})
#define core_slice_raw_from_raw_parts(ptr, len, _0, _1) \
    (KRML_CLITERAL(Eurydice_slice){(void *)(ptr), len})

#endif /* EURYDICE_HEADER_UNSAFE_H */
