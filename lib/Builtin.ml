module K = struct
  include Krml.Ast
end

let slice: K.lident = ["Eurydice"], "slice"

let mk_slice (t: K.typ): K.typ =
  K.TApp (slice, [ t ])

let slice_len: K.lident = ["Eurydice"], "slice_len"
let slice_len_t = K.(TArrow (mk_slice (TBound 0), TInt SizeT))

let slice_new: K.lident = ["Eurydice"], "slice_new"
let slice_new_t = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    TInt SizeT;
    TInt SizeT
  ] (mk_slice (TBound 0))

let range: K.lident = ["core"; "ops"; "range"], "Range"
let mk_range (t: K.typ): K.typ =
  K.TApp (range, [ t ])

let slice_new_with_range: K.lident = ["Eurydice"], "slice_new_with_range"
let slice_new_with_range_t = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    mk_range (TInt SizeT)
  ] (TBound 0)

let array_copy = ["Eurydice"], "array_copy"

let files = [
  "Eurydice", [
    K.DExternal (None, [], 1, slice_len, slice_len_t, [ "s" ]);
    K.DExternal (None, [], 1, slice_new, slice_new_t, [ "a" ]);
    K.DExternal (None, [], 1, slice_new_with_range, slice_new_with_range_t, [ "a"; "r" ]);
    K.DType (range, [], 1, Flat [
      Some "start", (TBound 0, true);
      Some "end", (TBound 0, true);
    ]);
  ]
]
