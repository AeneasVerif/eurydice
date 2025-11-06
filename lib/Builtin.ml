module K = struct
  include Krml.Ast
end

(* Helpers to build krml types *)

let vec : K.lident = [ "Eurydice" ], "vec"
let mk_vec (t : K.typ) : K.typ = K.TApp (vec, [ t ])
let dst_ref = [ "Eurydice" ], "dst_ref"

(** Given `T` and `TMeta`, returns `Eurydice::dst_ref<T, TMeta>` *)
let mk_dst_ref (t : K.typ) (meta : K.typ) : K.typ = K.TApp (dst_ref, [ t; meta ])

let slice : K.lident = dst_ref
let mk_slice (t : K.typ) : K.typ = K.TApp (slice, [ t; TInt SizeT ])
let range : K.lident = [ "core"; "ops"; "range" ], "Range"
let mk_range (t : K.typ) : K.typ = K.TApp (range, [ t ])
let range_to : K.lident = [ "core"; "ops"; "range" ], "RangeTo"
let mk_range_to (t : K.typ) : K.typ = K.TApp (range_to, [ t ])
let range_from : K.lident = [ "core"; "ops"; "range" ], "RangeFrom"
let mk_range_from (t : K.typ) : K.typ = K.TApp (range_from, [ t ])
let option : K.lident = [ "core"; "option" ], "Option"
let mk_option (t : K.typ) : K.typ = K.TApp (option, [ t ])

(* special treatment for the array type: translating [T;C] as rust generic type
    struct <const C:usize, T> { data : [T;C]; } *)

let arr : K.lident = [ "Eurydice" ], "arr"
let mk_arr (t : K.typ) (cg : K.cg) : K.typ = K.TCgApp (K.TApp (arr, [ t ]), cg)
let decl_of_arr = K.DType (arr, [], 1, 1, Flat [ Some "data", (K.TCgArray (TBound 0, 0), true) ])
(* []  : no flags
     1  : we have one const generic C
     1  : we have one type argument T *)

(* Things that could otherwise be emitted as an extern prototype, but for some
   reason ought to be skipped. *)
let skip = Krml.Idents.LidSet.of_list [ [ "Eurydice" ], "assert"; [], "UNIT_METADATA" ]
let result = [ "core"; "result" ], "Result"
let mk_result t1 t2 = K.TApp (result, [ t1; t2 ])
let nonzero = [ "core"; "num"; "nonzero" ], "NonZero"
let mk_nonzero t = K.TApp (nonzero, [ t ])

(* Internal types *)
let char_t = K.(TInt UInt32)
let c_void_t = K.TQualified ([ "Eurydice" ], "c_void_t")
let c_char_t = K.TQualified ([ "Eurydice" ], "c_char_t")
let int128_t = K.TQualified ([ "Eurydice"; "Int128" ], "int128_t")
let uint128_t = K.TQualified ([ "Eurydice"; "Int128" ], "uint128_t")

(* Internal decls *)
let dst_ref_decl =
  K.DType
    ( dst_ref,
      [],
      0,
      2,
      Flat [ Some "ptr", (TBuf (TBound 1, false), false); Some "meta", (TBound 0, false) ] )

(** A record to hold a builtin *function* with all relevant information for both krml and the
    transpilation phase in AstOfLlbc *)

type builtin = {
  name : K.lident;
  typ : K.typ;
  n_type_args : int;
  cg_args : K.typ list;
  arg_names : string list;
}

let expr_of_builtin { name; typ; cg_args; _ } =
  let typ = List.fold_right (fun t acc -> K.TArrow (t, acc)) cg_args typ in
  K.(with_type typ (EQualified name))

let chop_cg_args n t =
  let t, ts = Krml.Helpers.flatten_arrow t in
  let _, ts = Krml.KList.split n ts in
  let t = Krml.Helpers.fold_arrow ts t in
  t

let expr_of_builtin_t builtin ?(cgs = 0, []) ts =
  let open Krml.DeBruijn in
  let builtin = expr_of_builtin builtin in
  let diff, cg_exprs = cgs in
  let cgs = List.map (cg_of_expr diff) cg_exprs in
  let t = chop_cg_args (List.length cgs) (subst_tn ts (subst_ctn' cgs builtin.typ)) in
  K.(with_type t (ETApp (builtin, cg_exprs, [], ts)))

module Op128Map = Map.Make (struct
  type t = string * string

  let compare = Stdlib.compare
end)

(* Builtins for i128 and u128 are defined here but implemented in C. *)
let mk_128_builtin_op kind op lhs_typ rhs_typ ret_typ =
  let name = [ "Eurydice"; "Int128" ], kind ^ "128_" ^ op in
  let args, arg_names =
    match rhs_typ with
    | K.TUnit -> [ lhs_typ ], [ "lhs" ]
    | rhs -> [ lhs_typ; rhs ], [ "lhs"; "rhs" ]
  in
  { name; typ = Krml.Helpers.fold_arrow args ret_typ; n_type_args = 0; cg_args = []; arg_names }

let op_128_cfgs =
  [
    ("i", "from_bits"), (Krml.Helpers.uint64, Krml.Helpers.uint64, int128_t);
    ("i", "add"), (int128_t, int128_t, int128_t);
    ("i", "sub"), (int128_t, int128_t, int128_t);
    ("i", "mul"), (int128_t, int128_t, int128_t);
    ("i", "div"), (int128_t, int128_t, int128_t);
    ("i", "mod"), (int128_t, int128_t, int128_t);
    ("i", "bor"), (int128_t, int128_t, int128_t);
    ("i", "band"), (int128_t, int128_t, int128_t);
    ("i", "bxor"), (int128_t, int128_t, int128_t);
    ("i", "shl"), (int128_t, TInt UInt32, int128_t);
    ("i", "shr"), (int128_t, TInt UInt32, int128_t);
    ("i", "bnot"), (int128_t, K.TUnit, int128_t);
    ("i", "neg"), (int128_t, K.TUnit, int128_t);
    ("u", "neg"), (uint128_t, K.TUnit, uint128_t);
    ("i", "eq"), (int128_t, int128_t, TBool);
    ("i", "lt"), (int128_t, int128_t, TBool);
    ("i", "gt"), (int128_t, int128_t, TBool);
    ("i", "lte"), (int128_t, int128_t, TBool);
    ("i", "gte"), (int128_t, int128_t, TBool);
    ("i", "neq"), (int128_t, int128_t, TBool);
    ("i", "addW"), (int128_t, int128_t, int128_t);
    ("i", "subW"), (int128_t, int128_t, int128_t);
    ("i", "divW"), (int128_t, int128_t, int128_t);
    ("i", "multW"), (int128_t, int128_t, int128_t);
    ("u", "from_bits"), (Krml.Helpers.uint64, Krml.Helpers.uint64, uint128_t);
    ("u", "add"), (uint128_t, uint128_t, uint128_t);
    ("u", "sub"), (uint128_t, uint128_t, uint128_t);
    ("u", "mul"), (uint128_t, uint128_t, uint128_t);
    ("u", "div"), (uint128_t, uint128_t, uint128_t);
    ("u", "mod"), (uint128_t, uint128_t, uint128_t);
    ("u", "bor"), (uint128_t, uint128_t, uint128_t);
    ("u", "band"), (uint128_t, uint128_t, uint128_t);
    ("u", "bxor"), (uint128_t, uint128_t, uint128_t);
    ("u", "shl"), (uint128_t, TInt UInt32, uint128_t);
    ("u", "shr"), (uint128_t, TInt UInt32, uint128_t);
    ("u", "bnot"), (uint128_t, K.TUnit, uint128_t);
    ("u", "eq"), (uint128_t, uint128_t, TBool);
    ("u", "lt"), (uint128_t, uint128_t, TBool);
    ("u", "gt"), (uint128_t, uint128_t, TBool);
    ("u", "lte"), (uint128_t, uint128_t, TBool);
    ("u", "gte"), (uint128_t, uint128_t, TBool);
    ("u", "neq"), (uint128_t, uint128_t, TBool);
    ("u", "addW"), (uint128_t, uint128_t, uint128_t);
    ("u", "subW"), (uint128_t, uint128_t, uint128_t);
    ("u", "divW"), (uint128_t, uint128_t, uint128_t);
    ("u", "multW"), (uint128_t, uint128_t, uint128_t);
  ]
  |> List.fold_left
       (fun acc ((kind, op), (lhs_typ, rhs_typ, ret_typ)) ->
         Op128Map.add (kind, op) (mk_128_builtin_op kind op lhs_typ rhs_typ ret_typ) acc)
       Op128Map.empty

let get_128_op (kind, op) : K.expr = expr_of_builtin @@ Op128Map.find (kind, op) op_128_cfgs

(** Get the size of the given type, corresponding to `sizeof` in C. This corresponds to
    `NullOp::SizeOf` in Charon, which is itself used in metadata field `size` in vtable. *)
let sizeof =
  {
    name = [ "Eurydice" ], "sizeof";
    typ = Krml.Helpers.fold_arrow [ TUnit ] (TInt SizeT);
    n_type_args = 1;
    cg_args = [];
    arg_names = [];
  }

(** Get the alignment of the given type, corresponding to `alignof` in C. This corresponds to
    `NullOp::AlignOf` in Charon, which is itself used in metadata field `align` in vtable. *)
let alignof =
  {
    name = [ "Eurydice" ], "alignof";
    typ = Krml.Helpers.fold_arrow [ TUnit ] (TInt SizeT);
    n_type_args = 1;
    cg_args = [];
    arg_names = [];
  }

let opaque =
  {
    name = [ "Eurydice" ], "opaque";
    typ = Krml.Helpers.fold_arrow [ TBuf (c_char_t, false) ] (TBound 0);
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "reason" ];
  }

let opaque_with_reason ret_t reason =
  let app_typ = Krml.Helpers.fold_arrow [ TBuf (c_char_t, false) ] ret_t in
  let opaque = K.with_type app_typ (K.ETApp (expr_of_builtin opaque, [], [], [ ret_t ])) in
  K.with_type ret_t (K.EApp (opaque, [ K.with_type (TBuf (c_char_t, false)) (K.EString reason) ]))

let array_to_slice =
  {
    name = [ "Eurydice" ], "array_to_slice";
    typ =
      Krml.Helpers.fold_arrow [ TBuf (mk_arr (TBound 0) (CgVar 0), false) ] (mk_slice (TBound 0));
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a" ];
  }

let array_to_subslice =
  {
    name = [ "Eurydice" ], "array_to_subslice";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_arr (TBound 2) (CgVar 0), false); mk_range (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a"; "r" ];
  }

let array_to_subslice_to =
  {
    name = [ "Eurydice" ], "array_to_subslice_to";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_arr (TBound 2) (CgVar 0), false); mk_range_to (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a"; "r" ];
  }

let array_to_subslice_from =
  {
    name = [ "Eurydice" ], "array_to_subslice_from";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_arr (TBound 2) (CgVar 0), false); mk_range_from (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a"; "r" ];
  }

(* These two are placeholders that are inserted by AstOfLlbc with the intent that they should be
   desugared later on, once monomorphization and data type compilation, respectively, have happened. *)
let array_repeat =
  {
    name = [ "Eurydice" ], "array_repeat";
    typ = Krml.Helpers.fold_arrow [ TBound 0 ] (TCgArray (TBound 0, 0));
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "init" ];
  }

(* Eurydice_discriminant<T,U>(x: T) -> U
   T = type of the argument (an ADT)
   U = expected type of the discriminant (usize, u8, etc.)
   There is an unverified invariant that the algorithm in CStarToC11 to automatically pick suitable
   sizes for the `tag` field is compatible with the expected type U here. *)
let discriminant =
  {
    name = [ "Eurydice" ], "discriminant";
    typ = Krml.Helpers.fold_arrow [ TBound 1 ] (TBound 0);
    n_type_args = 2;
    cg_args = [];
    arg_names = [ "adt" ];
  }

let iterator : K.lident = [ "core"; "iter"; "traits"; "iterator" ], "Iterator"
let mk_iterator t = K.TApp (iterator, [ t ])

let array_eq =
  {
    name = [ "Eurydice" ], "array_eq";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_arr (TBound 0) (CgVar 0), false); TBuf (mk_arr (TBound 0) (CgVar 0), false) ]
        TBool;
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "arr"; "arr2" ];
  }

let array_eq_slice =
  {
    name = [ "Eurydice" ], "array_eq_slice";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_arr (TBound 0) (CgVar 0), false); TBuf (mk_slice (TBound 0), false) ]
        TBool;
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "arr"; "slice" ];
  }

let slice_eq =
  {
    name = [ "Eurydice" ], "slice_eq";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_slice (TBound 0), false); TBuf (mk_slice (TBound 0), false) ]
        TBool;
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "s1"; "s2" ];
  }

let step_by : K.lident = [ "core"; "iter"; "adapters"; "step_by" ], "StepBy"
let mk_step_by t = K.TApp (step_by, [ t ])
let mk_range_step_by_iterator t = mk_iterator (mk_step_by t)

(* This is incorrect: the function receives e.g.
   - Range<usize> as its type argument,
   - &StepBy<Range<usize>> for the type of its argument,
   then returns Option<usize> for its return value. Which we can't really type. *)
let range_iterator_step_by =
  {
    name = [ "Eurydice" ], "range_iterator_step_by";
    typ =
      Krml.Helpers.fold_arrow [ mk_range (TBound 0); TInt SizeT ] (mk_step_by (mk_range (TBound 0)));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "iter" ];
  }

let range_step_by_iterator_next =
  {
    name = [ "Eurydice" ], "range_step_by_iterator_next";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_step_by (mk_range (TBound 0)), false) ]
        (mk_option (TBound 0));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "iter" ];
  }

let slice_index =
  {
    name = [ "Eurydice" ], "slice_index";
    typ = Krml.Helpers.fold_arrow [ mk_slice (TBound 0); TInt SizeT ] (TBound 0);
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "s"; "i" ];
  }

let slice_index_outparam =
  {
    name = [ "Eurydice" ], "slice_index_outparam";
    typ = Krml.Helpers.fold_arrow [ mk_slice (TBound 0); TInt SizeT; TBound 0 ] TUnit;
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "s"; "i"; "dst" ];
  }

let slice_subslice =
  {
    name = [ "Eurydice" ], "slice_subslice";
    typ =
      Krml.Helpers.fold_arrow [ mk_slice (TBound 2); mk_range (TInt SizeT) ] (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s"; "r" ];
  }

let slice_subslice_to =
  {
    name = [ "Eurydice" ], "slice_subslice_to";
    typ =
      Krml.Helpers.fold_arrow
        [ mk_slice (TBound 2); mk_range_to (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s"; "r" ];
  }

let slice_subslice_from =
  {
    name = [ "Eurydice" ], "slice_subslice_from";
    typ =
      Krml.Helpers.fold_arrow
        [ mk_slice (TBound 2); mk_range_from (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s"; "r" ];
  }

(* This is replaced in PreCleanup because we need to allocate a Arr<T,C> *)
let slice_to_array =
  {
    name = [ "Eurydice" ], "slice_to_array";
    typ = Krml.Helpers.fold_arrow [ TBound 2 ] (mk_result (TBound 1) (TBound 0));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s" ];
  }

(* This is replaced by slice_to_ref_array2 by allocate a  Arr<T,C> and pass the ref as arg*)
let slice_to_ref_array =
  {
    name = [ "Eurydice" ], "slice_to_ref_array";
    typ = Krml.Helpers.fold_arrow [ TBound 2 ] (mk_result (TBound 1) (TBound 0));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "s" ];
  }

let slice_to_ref_array2 =
  {
    name = [ "Eurydice" ], "slice_to_ref_array2";
    typ = Krml.Helpers.fold_arrow [ TBound 2; TBound 1 ] (mk_result (TBound 1) (TBound 0));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "s" ];
  }

let box_new =
  {
    name = [ "Eurydice" ], "box_new";
    typ = Krml.Helpers.fold_arrow [ TBound 0 ] (TBuf (TBound 0, false));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v" ];
  }

let empty_array =
  {
    name = [ "Eurydice" ], "empty_array";
    typ = Krml.Helpers.fold_arrow [ TBound 0 ] (mk_arr (TBound 0) (CgConst (SizeT, "0")));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "x" ];
  }

let replace =
  {
    name = [ "Eurydice" ], "replace";
    typ = Krml.Helpers.fold_arrow [ TBuf (TBound 0, false); TBound 0 ] (TBound 0);
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v"; "x" ];
  }

let derefed_slice = [ "Eurydice" ], "derefed_slice"

(** The C counterpart of `&str` *)
let str_t = mk_dst_ref c_char_t (TInt SizeT)

(** The C counterpart of `str` and serves twofold functionalities: (1) when in expressions, it
    serves as a placeholder to get referenced again; (2) when in customised DST definition, it is
    defined as [char []] to have 0-length. *)
let deref_str_t = K.TApp (derefed_slice, [ c_char_t ])

let c_string_def =
  K.DType (([ "Prims" ], "string"), [ Private ], 0, 0, Abbrev (TBuf (c_char_t, false)))

(* Take the type of the ptr field *)
let dst_new ~ptr ~len t =
  let open K in
  with_type (mk_slice t) (EFlat [ Some "ptr", ptr; Some "meta", len ])

(* pointer, value *)
let bitand_pv_u8 =
  {
    name = [ "Eurydice" ], "bitand_pv_u8";
    typ = Krml.Helpers.fold_arrow [ TBuf (TInt UInt8, false); TInt UInt8 ] (TInt UInt8);
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "x"; "y" ];
  }

let shr_pv_u8 =
  {
    name = [ "Eurydice" ], "shr_pv_u8";
    typ = Krml.Helpers.fold_arrow [ TBuf (TInt UInt8, false); TInt Int32 ] (TInt UInt8);
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "x"; "y" ];
  }

let min_u32 =
  {
    name = [ "Eurydice" ], "min_u32";
    typ = Krml.Helpers.fold_arrow [ TInt UInt32; TInt UInt32 ] (TInt UInt32);
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "x"; "y" ];
  }

(* A non error-checking function that returns a vector whose ptr component is potentially NULL *)
let vec_alloc =
  {
    name = [ "Eurydice" ], "vec_alloc";
    typ = Krml.Helpers.fold_arrow [ TInt SizeT ] (mk_vec (TBound 0));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "len" ];
  }

(* Will allocating len elements of type T overflow SIZE_MAX? *)
let vec_overflows =
  {
    name = [ "Eurydice" ], "vec_overflows";
    typ = Krml.Helpers.fold_arrow [ TInt SizeT ] TBool;
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "len" ];
  }

(* Since Eurydice_vec is opaque from the point of view of krml, we expose a helper (implemented with
   a macro) that can determine whether this a failed allocation *)
let vec_failed =
  {
    name = [ "Eurydice" ], "vec_failed";
    typ = Krml.Helpers.fold_arrow [ mk_vec (TBound 0) ] TBool;
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v" ];
  }

let layout_t = K.TQualified ([ "core"; "alloc"; "layout" ], "Layout")

(* Compute a layout from a type *)
let layout =
  {
    name = [ "Eurydice" ], "layout";
    typ = Krml.Helpers.fold_arrow [ TUnit ] layout_t;
    n_type_args = 1;
    cg_args = [];
    arg_names = [];
  }

(* Implementations, defined as DFunction using abstract syntax, mainly for generic slice type *)

(* Helpers for common use *)

let mk_sizeT = K.with_type (TInt SizeT)

(* a->data where a: Arr<t,n>*, to be used before eurydice monomorphization *)
let data_of_arrref (arrref : K.expr) t n_cgid =
  let arr = Krml.Helpers.(mk_deref (Krml.Helpers.assert_tbuf_or_tarray arrref.typ) arrref.node) in
  K.(with_type (TCgArray (t, n_cgid)) (EField (arr, "data")))

(* let array_to_slice<T;N> (a : &Arr<T,N>) = dst_ref { ptr = a->data; meta = N }  *)
let array_to_slice_func =
  let open Krml in
  let open Ast in
  let element_t = TBound 0 in
  let arrref_t = TBuf (mk_arr (TBound 0) (CgVar 0), false) in
  let ret_t = mk_slice element_t in
  let lid = [ "Eurydice" ], "array_to_slice" in
  let binders = [ Helpers.fresh_binder "N" (TInt SizeT); Helpers.fresh_binder "a" arrref_t ] in
  let expr =
    (* args *)
    let n = mk_sizeT (EBound 1) in
    let arrref = with_type arrref_t (EBound 0) in
    let data = data_of_arrref arrref element_t 0 in
    with_type ret_t (EFlat [ Some "ptr", data; Some "meta", n ])
  in
  DFunction (None, [ Private ], 1, 1, ret_t, lid, binders, expr)

(* let array_to_subslice<T;N> (r: Range<SizeT>, x : &Arr<T,N>)
   = dst_ref { ptr = a->data + r.start ; meta = r.end - r.start }  *)
let array_to_subslice_func =
  let open Krml in
  let open Ast in
  let element_t = TBound 2 in
  let arrref_t = TBuf (mk_arr (TBound 2) (CgVar 0), false) in
  let ret_t = mk_slice element_t in
  let lid = [ "Eurydice" ], "array_to_subslice" in
  let binders =
    [
      Helpers.fresh_binder "N" (TInt SizeT);
      Helpers.fresh_binder "a" arrref_t;
      Helpers.fresh_binder "r" (mk_range (TInt SizeT));
    ]
  in
  let expr =
    (* args *)
    let arrref = with_type arrref_t (EBound 1) in
    let range = with_type (mk_range (TInt SizeT)) (EBound 0) in
    let data = data_of_arrref arrref element_t 0 in
    let r_start = mk_sizeT (EField (range, "start")) in
    let r_end = mk_sizeT (EField (range, "end")) in
    let meta = mk_sizeT (EApp (Helpers.mk_op Sub SizeT, [ r_end; r_start ])) in
    let ptr = with_type (TBuf (element_t, false)) (EBufSub (data, r_start)) in
    with_type ret_t (EFlat [ Some "ptr", ptr; Some "meta", meta ])
  in
  DFunction (None, [ Private ], 1, 3, ret_t, lid, binders, expr)

(* let array_to_subslice_to<T;N> (r: RangeTo<SizeT>, x : &Arr<T,N>)
   = dst_ref { ptr = a->data; meta = r.end } *)
let array_to_subslice_to_func =
  let open Krml in
  let open Ast in
  let element_t = TBound 2 in
  let arrref_t = TBuf (mk_arr (TBound 2) (CgVar 0), false) in
  let ret_t = mk_slice element_t in
  let lid = [ "Eurydice" ], "array_to_subslice_to" in
  let binders =
    [
      Helpers.fresh_binder "N" (TInt SizeT);
      Helpers.fresh_binder "a" arrref_t;
      Helpers.fresh_binder "r" (mk_range_to (TInt SizeT));
    ]
  in
  let expr =
    (* args *)
    let arrref = with_type arrref_t (EBound 1) in
    let range = with_type (mk_range_to (TInt SizeT)) (EBound 0) in
    (* { .ptr = a->data + r.start, .meta = r.end - r.start }*)
    let data = data_of_arrref arrref element_t 0 in
    let meta = with_type (TInt SizeT) (EField (range, "end")) in
    with_type ret_t (EFlat [ Some "ptr", data; Some "meta", meta ])
  in
  DFunction (None, [ Private ], 1, 3, ret_t, lid, binders, expr)

(* let array_to_subslice_from<T;N> (r: RangeFrom<SizeT>, x : &Arr<T,N>)
   = dst_ref { ptr = a->data + r.start; meta = N - r.start } *)
let array_to_subslice_from_func =
  let open Krml in
  let open Ast in
  let element_t = TBound 2 in
  let arrref_t = TBuf (mk_arr (TBound 2) (CgVar 0), false) in
  let ret_t = mk_slice element_t in
  let lid = [ "Eurydice" ], "array_to_subslice_from" in
  let binders =
    [
      Helpers.fresh_binder "N" (TInt SizeT);
      Helpers.fresh_binder "a" arrref_t;
      Helpers.fresh_binder "r" (mk_range_from (TInt SizeT));
    ]
  in
  let expr =
    (* args *)
    let n = mk_sizeT (EBound 2) in
    let arrref = with_type arrref_t (EBound 1) in
    let range = with_type (mk_range_from (TInt SizeT)) (EBound 0) in
    let data = data_of_arrref arrref element_t 0 in
    let start = mk_sizeT (EField (range, "start")) in
    let meta = mk_sizeT (EApp (Helpers.mk_op Sub SizeT, [ n; start ])) in
    let ptr = with_type (TBuf (element_t, false)) (EBufSub (data, start)) in
    with_type ret_t (EFlat [ Some "ptr", ptr; Some "meta", meta ])
  in
  DFunction (None, [ Private ], 1, 3, ret_t, lid, binders, expr)

(* let slice_subslice<T> (r: Range<SizeT>, s : DstRef<T,N>)
   = dst_ref { ptr = s.ptr + r.start; meta = r.end - r.start } *)
let slice_subslice_func =
  let open Krml in
  let open Ast in
  let element_t = TBound 2 in
  let slice_t = mk_slice element_t in
  let lid = [ "Eurydice" ], "slice_subslice" in
  let binders =
    [ Helpers.fresh_binder "s" slice_t; Helpers.fresh_binder "r" (mk_range (TInt SizeT)) ]
  in
  let expr =
    (* args *)
    let slice = with_type slice_t (EBound 1) in
    let range = with_type (mk_range (TInt SizeT)) (EBound 0) in
    let ptr = with_type (TBuf (element_t, false)) (EField (slice, "ptr")) in
    let r_start = mk_sizeT (EField (range, "start")) in
    let r_end = mk_sizeT (EField (range, "end")) in
    let meta = mk_sizeT (EApp (Helpers.mk_op Sub SizeT, [ r_end; r_start ])) in
    let ptr = with_type (TBuf (element_t, false)) (EBufSub (ptr, r_start)) in
    with_type slice_t (EFlat [ Some "ptr", ptr; Some "meta", meta ])
  in
  DFunction (None, [ Private ], 0, 3, slice_t, lid, binders, expr)

(* let slice_subslice_to<T> (r: RangeTo<SizeT>, s : DstRef<T,N>)
   = dst_ref { ptr = s.ptr ; meta = r.end } *)
let slice_subslice_to_func =
  let open Krml in
  let open Ast in
  let element_t = TBound 2 in
  let slice_t = mk_slice element_t in
  let lid = [ "Eurydice" ], "slice_subslice_to" in
  let binders =
    [ Helpers.fresh_binder "s" slice_t; Helpers.fresh_binder "r" (mk_range_to (TInt SizeT)) ]
  in
  let expr =
    (* args *)
    let slice = with_type slice_t (EBound 1) in
    let range = with_type (mk_range_to (TInt SizeT)) (EBound 0) in
    let ptr = with_type (TBuf (element_t, false)) (EField (slice, "ptr")) in
    let meta = with_type (TInt SizeT) (EField (range, "end")) in
    with_type slice_t (EFlat [ Some "ptr", ptr; Some "meta", meta ])
  in
  DFunction (None, [ Private ], 0, 3, slice_t, lid, binders, expr)

(* let slice_subslice_from<T> (r: RangeFrom<SizeT>, s : DstRef<T,N>)
   = dst_ref { ptr = s.ptr + r.start ; meta = s.meta - r.start } *)
let slice_subslice_from_func =
  let open Krml in
  let open Ast in
  let element_t = TBound 2 in
  let slice_t = mk_slice element_t in
  let lid = [ "Eurydice" ], "slice_subslice_from" in
  let binders =
    [ Helpers.fresh_binder "s" slice_t; Helpers.fresh_binder "r" (mk_range_from (TInt SizeT)) ]
  in
  let expr =
    (* args *)
    let slice = with_type slice_t (EBound 1) in
    let range = with_type (mk_range_from (TInt SizeT)) (EBound 0) in
    let ptr = with_type (TBuf (element_t, false)) (EField (slice, "ptr")) in
    let meta = mk_sizeT (EField (slice, "meta")) in
    let start = mk_sizeT (EField (range, "start")) in
    let ptr = with_type (TBuf (element_t, false)) (EBufSub (ptr, start)) in
    let meta = mk_sizeT (EApp (Helpers.mk_op Sub SizeT, [ meta; start ])) in
    with_type slice_t (EFlat [ Some "ptr", ptr; Some "meta", meta ])
  in
  DFunction (None, [ Private ], 0, 3, slice_t, lid, binders, expr)

(* Not fully general *)
let static_assert, static_assert_ref =
  let name = [ "Eurydice" ], "assert" in
  let typ = Krml.Helpers.fold_arrow [ TBool; Krml.Checker.c_string ] TUnit in
  ( K.DExternal (None, [ Krml.Common.Private; Macro ], 0, 0, name, typ, [ "test"; "msg" ]),
    K.(with_type typ (EQualified name)) )

(* Replacements, now applied on-the-fly in AstOfLlbc.

 IMPORTANT: such replacements are written in abstract syntax that *already* has cleanups applied,
 meaning that some passes like Cleanup1.remove_assignments and un-necessary and will actually error
 out. We maintain a list of such functions in Cleanup1, to be kept in sync with this. *)

let unwrap =
  let open Krml in
  let open Ast in
  let t_T = TBound 1 in
  let t_E = TBound 0 in
  let b = Krml.Helpers.fresh_binder "f0" t_T in
  let t_result = mk_result t_T t_E in
  let binders = [ Helpers.fresh_binder "self" t_result ] in
  (* Ensures this returns always the same term (structurally equal) *)
  fun lid ->
    DFunction
      ( None,
        [ Private ],
        0,
        2,
        t_T,
        lid,
        binders,
        with_type t_T
          (EMatch
             ( Unchecked,
               with_type t_result (EBound 0),
               [
                 ( [ b ],
                   with_type t_result (PCons ("Ok", [ with_type t_T (PBound 0) ])),
                   with_type t_T (EBound 0) );
                 ( [],
                   with_type t_result PWild,
                   with_type t_T (EAbort (Some t_T, Some "unwrap not Ok")) );
               ] )) )

(* Easier this way rather than implement a macro with an expression-statement.

   external core_slice_{@Slice<T>}_swap <1>:<cg: 0>
    Eurydice_slice
    0 ->
    size_t ->
      size_t ->
        ()
*)
let slice_swap =
  let open Krml in
  let open Ast in
  let t = TBound 0 in
  let binders =
    [
      Helpers.fresh_binder ~mut:true "s" (mk_slice t);
      Helpers.fresh_binder "i" (TInt SizeT);
      Helpers.fresh_binder "j" (TInt SizeT);
    ]
  in
  (* with slice type *)
  let mk_slice = with_type (mk_slice t) in
  (* with usize type *)
  let index s i =
    let slice_index = expr_of_builtin_t slice_index [ t ] in
    with_type t (EApp (slice_index, [ s; i ]))
  in
  let lhs s i =
    with_type t (EBufRead (with_type (TBuf (t, false)) (EAddrOf (index s i)), Helpers.zero_usize))
  in
  fun lid ->
    DFunction
      ( None,
        [ Private ],
        0,
        1,
        TUnit,
        lid,
        binders,
        (* let tmp = s[i]; *)
        with_type TUnit
          (ELet
             ( Helpers.fresh_binder "tmp" t,
               index (mk_slice (EBound 2)) (mk_sizeT (EBound 1)),
               with_type TUnit
                 (ESequence
                    [
                      (* s[i] = s[j] *)
                      with_type TUnit
                        (EAssign
                           ( lhs (mk_slice (EBound 3)) (mk_sizeT (EBound 2)),
                             index (mk_slice (EBound 3)) (mk_sizeT (EBound 1)) ));
                      (* s[j] = tmp *)
                      with_type TUnit
                        (EAssign
                           (lhs (mk_slice (EBound 3)) (mk_sizeT (EBound 1)), with_type t (EBound 0)));
                    ]) )) )

(* Formerly a macro, using GCC expression-statements:

#define alloc_vec__alloc__vec__Vec_T___try_with_capacity(len, t_elt, t_ret) \
  ({ \
    size_t element_sz = sizeof(t_elt); \
    Eurydice_vec v = Eurydice_vec_try_with_capacity(len, sizeof(t_elt)); \
    t_ret r; \
    if (!(len <= SIZE_MAX/element_sz)) \
      r = ((t_ret){ .tag = core_result_Err, .val = { .case_Err = { .tag = alloc_collections_CapacityOverflow } } }); \
    else if (v.ptr != NULL) { \
      r = ((t_ret){ .tag = core_result_Ok, .val = { .case_Ok = v }}); \
    } else { \
      r = ((t_ret){ .tag = core_result_Err, .val = { .case_Err = { \
         .tag = alloc_collections_AllocError, /* CHECK ??? */ \
         .layout = { .size = len * sizeof(t_elt), .align = 8 } \
       }}}); \
    } \
    r; \
  })

  Remember that this is all pre-monomorphization.

*)
let try_with_capacity =
  let open Krml in
  let open Ast in
  let t = TBound 0 in
  let t_try_reserve_error = TQualified ([ "alloc"; "collections" ], "TryReserveError") in
  (* Result<Vec<T>, TryReserveError> *)
  let t_ret = TApp (([ "core"; "result" ], "Result"), [ mk_vec t; t_try_reserve_error ]) in
  (* TryReserveError { kind = TryReserveErrorKind::Cons args } *)
  let mk_try_reserve_error cons args =
    with_type t_try_reserve_error
      (EFlat
         [
           ( Some "kind",
             with_type
               (TQualified ([ "alloc"; "collections" ], "TryReserveErrorKind"))
               (ECons (cons, args)) );
         ])
  in
  let mk_res_error err = with_type t_ret (ECons ("Err", [ err ])) in
  let mk_res_ok ok = with_type t_ret (ECons ("Ok", [ ok ])) in
  let binders = [ Helpers.fresh_binder "len" (TInt SizeT) ] in
  (* with size *)
  let ws = with_type (TInt SizeT) in
  fun lid ->
    DFunction
      ( None,
        [ Private ],
        0,
        1,
        t_ret,
        lid,
        binders,
        with_type t_ret
          (EIfThenElse
             ( (* if vec_overflows<t_elt>(len) then *)
               with_type TBool (EApp (expr_of_builtin_t vec_overflows [ t ], [ ws (EBound 0) ])),
               (* Result::Error(TryReserveError { kind = TryReserveErrorKind::CapacityOverflow }) *)
               mk_res_error (mk_try_reserve_error "CapacityOverflow" []),
               (* else let v: vec<t_elt> = vec_alloc len in *)
               with_type t_ret
                 (ELet
                    ( Helpers.fresh_binder "v" (mk_vec t),
                      with_type (mk_vec t)
                        (EApp (expr_of_builtin_t vec_alloc [ t ], [ ws (EBound 0) ])),
                      (* if vec_failed v then *)
                      with_type t_ret
                        (EIfThenElse
                           ( with_type TBool
                               (EApp
                                  ( expr_of_builtin_t vec_failed [ t ],
                                    [ with_type (mk_vec t) (EBound 0) ] )),
                             (* Result::Error(
                  TryReserveError { kind = TryReserveErrorKind::AllocError { layout: layout<T>(), non_exhaustive: () }}
                 )
              *)
                             mk_res_error
                               (mk_try_reserve_error "AllocError"
                                  [
                                    (* "layout", *)
                                    with_type layout_t
                                      (EApp (expr_of_builtin_t layout [ t ], [ Helpers.eunit ]));
                                    (* "non_exhaustive", *)
                                    Helpers.eunit;
                                  ]),
                             (* Result::Ok(v) *)
                             mk_res_ok (with_type (mk_vec t) (EBound 0)) )) )) )) )

let null_mut =
  let open Krml.Ast in
  let t = TBound 0 in
  fun lid ->
    DFunction
      ( None,
        [ Private ],
        0,
        1,
        TBuf (t, false),
        lid,
        [ Krml.Helpers.fresh_binder "_" TUnit ],
        with_type (TBuf (t, false)) EBufNull )

let nonzero_def = K.DType (nonzero, [], 0, 1, Abbrev (TBound 0))

(* -------------------------------------------------------------------------- *)

type usage = Used | Unused

let builtin_funcs =
  [
    sizeof;
    alignof;
    opaque;
    array_repeat;
    array_eq;
    array_eq_slice;
    slice_eq;
    slice_index;
    slice_index_outparam;
    slice_to_array;
    slice_to_ref_array;
    slice_to_ref_array2;
    discriminant;
    range_iterator_step_by;
    range_step_by_iterator_next;
    box_new;
    empty_array;
    replace;
    bitand_pv_u8;
    shr_pv_u8;
    min_u32;
    vec_alloc;
    vec_overflows;
    vec_failed;
    layout;
  ]
  (* Declares the 128-bit operations *)
  @ begin
      Op128Map.to_seq op_128_cfgs |> List.of_seq |> List.map snd
    end

let builtin_defined_funcs =
  [
    array_to_slice_func;
    array_to_subslice_func;
    array_to_subslice_to_func;
    array_to_subslice_from_func;
    slice_subslice_func;
    slice_subslice_to_func;
    slice_subslice_from_func;
  ]

let files =
  [
    Krml.Builtin.lowstar_ignore;
    (let externals =
       List.map
         (fun { name; typ; cg_args; n_type_args; arg_names } ->
           let typ = Krml.Helpers.fold_arrow cg_args typ in
           let flags = [ Krml.Common.Private ] in
           K.DExternal (None, flags, List.length cg_args, n_type_args, name, typ, arg_names))
         builtin_funcs
       @ builtin_defined_funcs
       @ [ nonzero_def; static_assert; c_string_def ]
     in
     "Eurydice", externals);
  ]

let is_a_builtin_func_name (name : K.lident) =
  (* Potentially make the list of built-in funcs a Map to speed up if necessary. *)
  List.exists (fun { name = n; _ } -> n = name) builtin_funcs
