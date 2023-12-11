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

let range_to: K.lident = ["core"; "ops"; "range"], "RangeTo"
let mk_range_to (t: K.typ): K.typ =
  K.TApp (range_to, [ t ])

let range_from: K.lident = ["core"; "ops"; "range"], "RangeFrom"
let mk_range_from (t: K.typ): K.typ =
  K.TApp (range_from, [ t ])

let option: K.lident = ["core"; "option"], "Option"
let mk_option (t: K.typ): K.typ =
  K.TApp (option, [ t ])

let array_copy = ["Eurydice"], "array_copy"

let macros = Krml.Idents.LidSet.of_list [
  ["core"; "slice"; "{@Slice<T>}"], "len"
]

let result = ["core"; "result"], "Result"
let mk_result t1 t2 =
  K.TApp (result, [ t1; t2 ])

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

let array_to_subslice_to = {
  name = ["Eurydice"], "array_to_subslice_to";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 1, false);
    mk_range_to (TInt SizeT)
  ] (mk_slice (TBound 1));
  n_type_args = 2;
  cg_args = [ TInt SizeT ];
  arg_names = [ "a"; "r" ]
}

let array_to_subslice_from = {
  name = ["Eurydice"], "array_to_subslice_from";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TBound 1, false);
    mk_range_from (TInt SizeT)
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

let iterator: K.lident = ["core"; "iter"; "traits"; "iterator"], "Iterator"
let mk_iterator t = K.TApp (iterator, [ t ])

let array_into_iter = {
  name = ["Eurydice"], "array_into_iter";
  typ = Krml.Helpers.fold_arrow [
    TCgArray (TBound 0, 0)
  ] (mk_iterator (TCgArray (TBound 0, 0)));
  n_type_args = 1;
  cg_args = [ TInt SizeT ];
  arg_names = [ "arr" ]
}

let step_by: K.lident = ["core"; "iter"; "adapters"; "step_by"], "StepBy"
let mk_step_by t = K.TApp (step_by, [ t ])
let mk_range_step_by_iterator t =
  mk_iterator (mk_step_by t)

let range_iterator_step_by = {
  name = ["Eurydice"], "range_iterator_step_by";
  typ = Krml.Helpers.fold_arrow [
    mk_range (TBound 0);
    TInt SizeT
  ] (mk_step_by (mk_range (TBound 0)));
  n_type_args = 1;
  cg_args = [];
  arg_names = [ "iter" ]
}

let range_step_by_iterator_next = {
  name = ["Eurydice"], "range_step_by_iterator_next";
  typ = Krml.Helpers.fold_arrow [
    TBuf (mk_range_step_by_iterator (TBound 0), false)
  ] (mk_option (TBound 0));
  n_type_args = 1;
  cg_args = [];
  arg_names = [ "iter" ]
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

let slice_subslice_to = {
  name = ["Eurydice"], "slice_subslice_to";
  typ = Krml.Helpers.fold_arrow [
    mk_slice (TBound 1);
    mk_range_to (TInt SizeT)
  ] (mk_slice (TBound 1));
  n_type_args = 2;
  cg_args = [];
  arg_names = ["s"; "r"]
}

let slice_subslice_from = {
  name = ["Eurydice"], "slice_subslice_from";
  typ = Krml.Helpers.fold_arrow [
    mk_slice (TBound 1);
    mk_range_from (TInt SizeT)
  ] (mk_slice (TBound 1));
  n_type_args = 2;
  cg_args = [];
  arg_names = ["s"; "r"]
}

(* This one comes out naturally of MIR but can't be implemented in C for obvious reasons. *)
let slice_to_array = {
  name = ["Eurydice"], "slice_to_array";
  typ = Krml.Helpers.fold_arrow [
    TBound 1;
  ] (mk_result (TBound 0) (TQualified (["core"; "array"], "TryFromSliceError")));
  n_type_args = 2;
  cg_args = [ ];
  arg_names = ["s"]
}

(* This one can be implemented by hand. *)
let slice_to_array2 = {
  name = ["Eurydice"], "slice_to_array2";
  typ = Krml.Helpers.fold_arrow [
    TBuf (mk_result (TBound 0) (TQualified (["core"; "array"], "TryFromSliceError")), false);
    TBound 1;
  ] TUnit;
  n_type_args = 2;
  cg_args = [ ];
  arg_names = ["dst";"s"]
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

(* pointer, value *)
let bitand_pv_u8 = {
  name = ["Eurydice"], "bitand_pv_u8";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TInt UInt8, false);
    TInt UInt8;
  ] (TInt UInt8);
  n_type_args = 0;
  cg_args = [];
  arg_names = ["x"; "y"]
}

let shr_pv_u8 = {
  name = ["Eurydice"], "shr_pv_u8";
  typ = Krml.Helpers.fold_arrow [
    TBuf (TInt UInt8, false);
    TInt Int32;
  ] (TInt UInt8);
  n_type_args = 0;
  cg_args = [];
  arg_names = ["x"; "y"]
}

let files = [
  Krml.Builtin.lowstar_ignore;
  let externals = List.map (fun { name; typ; cg_args; n_type_args; arg_names } ->
      let typ = Krml.Helpers.fold_arrow cg_args typ in
      K.DExternal (None, [], List.length cg_args, n_type_args, name, typ, arg_names)
    ) [
      array_to_slice;
      array_to_subslice;
      array_to_subslice_to;
      array_to_subslice_from;
      array_repeat;
      array_into_iter;
      slice_index;
      slice_subslice;
      slice_subslice_to;
      slice_subslice_from;
      slice_to_array;
      slice_to_array2;
      range_iterator_step_by;
      range_step_by_iterator_next;
      vec_push;
      vec_new;
      vec_len;
      vec_drop;
      vec_index;
      box_new;
      replace;
      bitand_pv_u8;
      shr_pv_u8;
  ] in
  "Eurydice", externals
]

