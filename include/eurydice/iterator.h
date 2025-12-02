#pragma once

// ITERATORS

// FIXME: these definitions should eventually be extracted instead of being hand
// written

#define Eurydice_range_iter_next(iter_ptr, t, ret_t)                           \
  (((iter_ptr)->start >= (iter_ptr)->end)                                      \
       ? (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.tag =) 0,                      \
                               EURYDICE_CFIELD(.f0 =) 0})                      \
       : (KRML_CLITERAL(ret_t){EURYDICE_CFIELD(.tag =) 1,                      \
                               EURYDICE_CFIELD(.f0 =)(iter_ptr)->start++}))

#define core_iter_range__core__iter__traits__iterator__Iterator_A__for_core__ops__range__Range_A__TraitClause_0___next \
  Eurydice_range_iter_next
