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

let option: K.lident = ["core"; "option"], "Option"
let mk_option (t: K.typ): K.typ =
  K.TApp (option, [ t ])

let array_copy = ["Eurydice"], "array_copy"

let macros = Krml.Idents.LidSet.of_list [
  ["core"; "slice"; "{@Slice<T>}"], "len"
]

(* A record to hold a builtin function with all relevant information for both
   krml and the transpilation phase in AstOfLlbc *)

type builtin = {
  name: K.lident;
  typ: K.typ;
  n_type_args: int;
  cg_args: K.typ list;
  arg_names: string list;
}

let array_to_slice = {
  name = ["Eurydice"], "array_to_slice";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
  ] (mk_slice (TBound 0));
  n_type_args = 1;
  cg_args = [
    TInt SizeT
  ];
  arg_names = [ "a" ]
}

let array_to_subslice = {
  name = ["Eurydice"], "array_to_subslice";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 1, false);
    mk_range (TInt SizeT)
  ] (mk_slice (TBound 1));
  n_type_args = 2;
  cg_args = [ TInt SizeT ];
  arg_names = [ "a"; "r" ]
}

let array_repeat = {
  name = ["Eurydice"], "array_repeat";
  typ = Krml.Helpers.fold_arrow [
    TBound 0
  ] (TCgArray (TBound 0, 0));
  n_type_args = 1;
  cg_args = [ TInt SizeT ];
  arg_names = [ "init" ]
}

let slice_index = {
  name = ["Eurydice"], "slice_index";
  typ = Krml.Helpers.fold_arrow [
    mk_slice (TBound 0);
    TInt SizeT
  ] (TBound 0);
  n_type_args = 1;
  cg_args = [];
  arg_names = ["s"; "i"]
}

let slice_subslice = {
  name = ["Eurydice"], "slice_subslice";
  typ = Krml.Helpers.fold_arrow [
    mk_slice (TBound 1);
    mk_range (TInt SizeT)
  ] (mk_slice (TBound 1));
  n_type_args = 2;
  cg_args = [];
  arg_names = ["s"; "r"]
}

let vec_new = {
  name = ["Eurydice"], "vec_new";
  typ = Krml.Helpers.fold_arrow [
    TUnit
  ] (mk_vec (TBound 0));
  n_type_args = 1;
  cg_args = [];
  arg_names = []
}

let vec_push = {
  name = ["Eurydice"], "vec_push";
  typ = Krml.Helpers.fold_arrow [
    mk_vec (TBound 0);
    TBound 0;
  ] TUnit;
  n_type_args = 1;
  cg_args = [];
  arg_names = ["v"; "x"]
}

let vec_len = {
  name = ["Eurydice"], "vec_len";
  typ = Krml.Helpers.fold_arrow [
    mk_vec (TBound 0);
  ] (TInt SizeT);
  n_type_args = 1;
  cg_args = [];
  arg_names = ["v"]
}

let vec_drop = {
  name = ["Eurydice"], "vec_drop";
  typ = Krml.Helpers.fold_arrow [
    mk_vec (TBound 0);
  ] TUnit;
  n_type_args = 1;
  cg_args = [];
  arg_names = ["v"]
}

let vec_index = {
  name = ["Eurydice"], "vec_index";
  typ = Krml.Helpers.fold_arrow [
    mk_vec (TBound 0);
    TInt SizeT
  ] (TBuf (TBound 0, false));
  n_type_args = 1;
  cg_args = [];
  arg_names = ["v"; "i"]
}

let box_new = {
  name = ["Eurydice"], "box_new";
  typ = Krml.Helpers.fold_arrow [
    TBound 0;
  ] (TBuf (TBound 0, false));
  n_type_args = 1;
  cg_args = [];
  arg_names = ["v"]
}

let replace = {
  name = ["Eurydice"], "replace";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 0, false);
    TBound 0;
  ] (TBound 0);
  n_type_args = 1;
  cg_args = [];
  arg_names = ["v"; "x"]
}

let files = [
  Krml.Builtin.lowstar_ignore;
  let externals = List.map (fun { name; typ; cg_args; n_type_args; arg_names } ->
      let typ = Krml.Helpers.fold_arrow cg_args typ in
      K.DExternal (None, [], List.length cg_args, n_type_args, name, typ, arg_names)
    ) [
      array_to_slice;
      array_to_subslice;
      array_repeat;
      slice_index;
      slice_subslice;
      vec_push;
      vec_new;
      vec_len;
      vec_drop;
      vec_index;
      box_new;
      replace;
  ] in
  "Eurydice", externals
]

