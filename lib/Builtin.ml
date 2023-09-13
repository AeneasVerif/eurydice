module K = struct
  include Krml.Ast
end

(* Helpers to build krml types *)

let slice: K.lident = ["Eurydice"], "slice"

let mk_slice (t: K.typ): K.typ =
  K.TApp (slice, [ t ])

let range: K.lident = ["core"; "ops"; "range"], "Range"
let mk_range (t: K.typ): K.typ =
  K.TApp (range, [ t ])

let array_copy = ["Eurydice"], "array_copy"

let macros = Krml.Idents.LidSet.of_list [
  [ "core"; "slice"; "[T]"; "{0}" ], "len"
]

(* A record to hold a builtin function with all relevant information for both
   krml and the transpilation phase in AstOfLlbc *)

type builtin = {
  name: K.lident;
  typ: K.typ;
  n_type_args: int;
  arg_names: string list;
}

let slice_len = {
  name = ["Eurydice"], "slice_len";
  typ = K.(TArrow (mk_slice (TBound 0), TInt SizeT));
  n_type_args = 1;
  arg_names = [ "s" ]
}

let array_to_slice = {
  name = ["Eurydice"], "array_to_slice";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    TInt SizeT
  ] (mk_slice (TBound 0));
  n_type_args = 1;
  arg_names = [ "a" ]
}

let array_to_subslice = {
  name = ["Eurydice"], "array_to_subslice";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    mk_range (TInt SizeT)
  ] (mk_slice (TBound 0));
  n_type_args = 1;
  arg_names = [ "a"; "r" ]
}

let slice_index = {
  name = ["Eurydice"], "slice_index";
  typ = Krml.Helpers.fold_arrow [
    mk_slice (TBound 0);
    TInt SizeT
  ] (TBuf (TBound 0, false));
  n_type_args = 1;
  arg_names = ["s"; "i"]
}

let slice_subslice = {
  name = ["Eurydice"], "slice_subslice";
  typ = Krml.Helpers.fold_arrow [
    mk_slice (TBound 0);
    mk_range (TInt SizeT)
  ] (mk_slice (TBound 0));
  n_type_args = 1;
  arg_names = ["s"; "r"]
}

let files = [
  let externals = List.map (fun { name; typ; n_type_args; arg_names } ->
      K.DExternal (None, [], n_type_args, name, typ, arg_names)
    ) [
      array_to_slice;
      array_to_subslice;
      slice_len;
      slice_index;
      slice_subslice;
  ] in
  "Eurydice", externals @ [
    K.DType (range, [], 1, Flat [
      Some "start", (TBound 0, true);
      Some "end", (TBound 0, true);
    ]);
  ]
]

