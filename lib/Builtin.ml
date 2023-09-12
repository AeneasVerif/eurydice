module K = struct
  include Krml.Ast
end

let slice: K.lident = ["Eurydice"], "slice"

let mk_slice (t: K.typ): K.typ =
  K.TApp (slice, [ t ])

let slice_len: K.lident = ["Eurydice"], "slice_len"
let slice_len_t = K.(TArrow (mk_slice (TBound 0), TInt SizeT))

let array_to_slice: K.lident = ["Eurydice"], "array_to_slice"
let array_to_slice_t = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    TInt SizeT
  ] (mk_slice (TBound 0))

let range: K.lident = ["core"; "ops"; "range"], "Range"
let mk_range (t: K.typ): K.typ =
  K.TApp (range, [ t ])

let array_to_subslice: K.lident = ["Eurydice"], "array_to_subslice"
let array_to_subslice_t = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    mk_range (TInt SizeT)
  ] (mk_slice (TBound 0))

let slice_index: K.lident = ["Eurydice"], "slice_index"
let slice_index_t = Krml.Helpers.fold_arrow [
    mk_slice (TBound 0);
    TInt SizeT
  ] (TBound 0)

let array_copy = ["Eurydice"], "array_copy"

let files = [
  "Eurydice", [
    K.DExternal (None, [], 1, slice_len, slice_len_t, [ "s" ]);
    K.DExternal (None, [], 1, array_to_slice, array_to_slice_t, [ "a" ]);
    K.DExternal (None, [], 1, array_to_subslice, array_to_subslice_t, [ "a"; "r" ]);
    K.DExternal (None, [], 1, slice_index, slice_index_t, [ "s"; "i" ]);
    K.DType (range, [], 1, Flat [
      Some "start", (TBound 0, true);
      Some "end", (TBound 0, true);
    ]);
  ]
]

let macros = Krml.Idents.LidSet.of_list [
  [ "core"; "slice"; "[T]"; "{0}" ], "len"
]
