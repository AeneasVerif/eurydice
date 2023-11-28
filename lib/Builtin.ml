module K = struct
  include Krml.Ast
end

(* Helpers to build krml types *)

let vec: K.lident = ["Eurydice"], "vec"

let mk_vec (t: K.typ): K.typ =
  K.TApp (vec, [ t ])

let slice: K.lident = ["Eurydice"], "slice"

let mk_slice (t: K.typ): K.typ =
  K.TApp (slice, [ t ])

let range: K.lident = ["core"; "ops"; "range"], "Range"
let mk_range (t: K.typ): K.typ =
  K.TApp (range, [ t ])

let option: K.lident = ["core"; "ops"; "option"], "Option"
let mk_option (t: K.typ): K.typ =
  K.TApp (option, [ t ])

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

let array_to_slice = {
  name = ["Eurydice"], "array_to_slice";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    TInt SizeT
  ] (mk_slice (TBound 0));
  n_type_args = 1;
  arg_names = [ "a" ]
}

let slice_index = {
  name = ["Eurydice"], "slice_index";
  typ = Krml.Helpers.fold_arrow [
    mk_slice (TBound 0);
    TInt SizeT
  ] (TBound 0);
  n_type_args = 1;
  arg_names = ["s"; "i"]
}

let box_new = {
  name = ["Eurydice"], "box_new";
  typ = Krml.Helpers.fold_arrow [
    TBound 0;
  ] (TBuf (TBound 0, false));
  n_type_args = 1;
  arg_names = ["v"]
}

let files = [
  Krml.Builtin.lowstar_ignore;
  let externals = List.map (fun { name; typ; n_type_args; arg_names } ->
      K.DExternal (None, [], n_type_args, name, typ, arg_names)
    ) [
      array_to_slice;
      slice_index;
      box_new;
  ] in
  "Eurydice", externals @ [
    K.DType (range, [], 1, Flat [
      Some "start", (TBound 0, true);
      Some "end", (TBound 0, true);
    ]);
    K.DType (option, [], 1, Variant [
      "None", [];
      "Some", [ "x", (TBound 0, true) ]
    ]);
  ]
]

