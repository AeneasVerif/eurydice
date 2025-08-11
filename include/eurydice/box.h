/*
 * SPDX-FileCopyrightText: 2024-2025 Eurydice Contributors
 *
 * SPDX-License-Identifier: MIT or Apache-2.0
 *
 * Rust boxes.
 */

#ifndef EURYDICE_HEADER_BOX_H
#define EURYDICE_HEADER_BOX_H

#include <stdint.h>
#include <string.h>

#ifndef EURYDICE_MALLOC
#define EURYDICE_MALLOC malloc
#endif

#ifndef EURYDICE_REALLOC
#define EURYDICE_REALLOC realloc
#endif

static inline char *malloc_and_init(size_t sz, char *init)
{
    char *ptr = (char *)EURYDICE_MALLOC(sz);
    if (ptr != NULL)
        memcpy(ptr, init, sz);
    return ptr;
}

#define Eurydice_box_new(init, t, t_dst) \
    ((t_dst)(malloc_and_init(sizeof(t), (char *)(&init))))

#define Eurydice_box_new_array(len, ptr, t, t_dst) \
    ((t_dst)(malloc_and_init(len * sizeof(t), (char *)(ptr))))

// FIXME this needs to handle allocation failure errors, but this seems hard to
// do without evaluating malloc_and_init twice...
#define alloc_boxed__alloc__boxed__Box_T___try_new(init, t, t_ret) \
    ((t_ret){.tag = core_result_Ok,                                \
             .f0 = (t *)malloc_and_init(sizeof(t), (char *)(&init))})

#define alloc_boxed__alloc__boxed__Box_T___from_raw(x, _0, _1) (x)
#define alloc_boxed__alloc__boxed__Box_T___into_raw(x, _0, _1) (x)

#endif /* EURYDICE_HEADER_BOX_H */
