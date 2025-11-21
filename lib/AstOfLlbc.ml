(* C for Charon *)
module C = struct
  include Charon.GAst
  include Charon.LlbcAst
  include Charon.Types
  include Charon.TypesUtils
  include Charon.Expressions
  include Charon.Values
  include Charon.GAstUtils

  (* Fails if the variable is bound *)
  let expect_free_var = function
    | Free id -> id
    | Bound _ -> failwith "Found unexpected bound variable"

  let tsubst cgs ts ty =
    begin
      object
        inherit [_] map_ty
        method! visit_TVar _ v = TypeVarId.nth ts (expect_free_var v)
        method! visit_CgVar _ v = ConstGenericVarId.nth cgs (expect_free_var v)
        method visit_'r _ x = x
      end
    end
      #visit_ty
      () ty
end

module LidMap = Krml.Idents.LidMap
module K = Krml.Ast
module L = Logging
open Krml.PrintAst.Ops

let fail fmt =
  let b = Buffer.create 256 in
  Printf.kbprintf (fun b -> failwith (Buffer.contents b)) b fmt

(** Environment *)

(* The various kinds of binders we insert in the expression scope. Usually come
   in this order, the first three being only ever inserted upon entering a function
   definition. *)
type var_id =
  | TraitClauseMethod of {
      clause_id : C.trait_ref_kind;
      item_name : string;
      pretty_name : string;
      ts : K.type_scheme;
    }
  | TraitClauseConstant of {
      clause_id : C.trait_ref_kind;
      item_name : string;
      pretty_name : string;
    }
  | ConstGenericVar of C.const_generic_var_id
  | Var of C.local_id * C.ety (* the ety aids code-generation, sometimes *)

type env = {
  (* Lookup functions to resolve various id's into actual declarations. *)
  get_nth_function : C.FunDeclId.id -> C.fun_decl;
  get_nth_type : C.TypeDeclId.id -> C.type_decl;
  get_nth_global : C.GlobalDeclId.id -> C.global_decl;
  get_nth_trait_impl : C.TraitImplId.id -> C.trait_impl;
  get_nth_trait_decl : C.TraitDeclId.id -> C.trait_decl;
  crate : C.crate;
  (* Needed by the name matching logic *)
  name_ctx : C.block Charon.NameMatcher.ctx;
  generic_params : C.generic_params;
  (* We have three lists of binders, which allow us to go from a Rust variable
     to a corresponding krml AST variable; everything is in De Bruijn, so
     looking up a variable is essentially List.nth. To understand why we have
     three lists here, we review the binding structure of the target (krml) AST.

     The target AST has three binding scopes: cg vars, type vars and expression
     vars. Type vars and type expressions are standard and have their own
     scopes, and corresponding variable nodes (TBound for type variables, Bound
     for expression variables). Const-generic variables are more complicted,
     because they appear in *both* types and expressions; there is a *third*
     scope of cg variables, with the following behavior:
     - in types, CgVar and TCgArray contain DeBruijn indices referring to the cg
       scope (standard), while
     - in expressions, there is no ECgVar, so we repeat cg vars at the beginning
       of the expression scope, and we rely on a regular EBound node to refer to
       cg variables (trick). This trick avoids a combinatorial explosion of
       substitution functions and makes sure all 60+ existing passes of krml do
       *not* need to be aware of the addition of const-generics (except for
       monomorphization, of course).
     In short, the third scope for cg variables only applies for CgVar and
     TCgArray; for expressions, cg variables masquerade as expression variables
     and live as the first N variables of that scope.

     To implement this, we rely on the corresponding three lists of binders,
     with the additional caveat that push_cg_binder pushes in both cg_binders
     and binders (see rationale above).

     Example: fn f<const N: usize, T: Copy>(x: [T; N]) -> usize { N }
     Upon entering the body of f, we have:
     - cg_binders: [ N, usize ]
     - type_binders: [ T ]
     - binders: [ `Cg (N, usize); `Clause (T: Copy, "copy"); `Var (x: [T; N]) ]

     After translation, we get:
     DFunction (..., 1 (* one type var *), 2 (* one cg var *), [
       "N": TInt usize;
       "x": TCgArray (TBound 0, 0); (* types use the cg scope *)
     ], EBound 2 (* expressions refer to the copy of the cg var as an expression var *)
  *)
  cg_binders : (C.const_generic_var_id * K.typ) list;
  type_binders : C.type_var_id list;
  binders : (var_id * K.typ) list;
  (* For printing. *)
  format_env : Charon.PrintLlbcAst.fmt_env;
  (* For picking pretty names *)
  crate_name : string;
}

let debug env =
  L.log "DebugEnv" "\n# Debug Env";
  List.iteri
    (fun i v ->
      L.log "DebugEnv" "type_binders[%d]: %s\n" i (Charon.PrintTypes.type_var_id_to_pretty_string v))
    env.type_binders

(* Environment: types *)

let findi p l =
  let rec findi i l =
    match l with
    | hd :: tl ->
        if p hd then
          i, hd
        else
          findi (i + 1) tl
    | [] -> raise Not_found
  in
  findi 0 l

let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let thd3 (_, _, x) = x

(* Suitable in types -- in expressions, use lookup_cg_in_expressions. *)
let lookup_cg_in_types env v1 =
  let i, (_, t) = findi (fun (v2, _) -> v1 = v2) env.cg_binders in
  i, t

let lookup_typ env (v1 : C.type_var_id) =
  let i, _ = findi (( = ) v1) env.type_binders in
  i

let push_type_binder env (t : C.type_param) =
  { env with type_binders = t.index :: env.type_binders }

let push_type_binders env (ts : C.type_param list) = List.fold_left push_type_binder env ts

(** Helpers: types *)

let with_any = K.(with_type TAny)
let is_dst_ref lid = Builtin.(lid = dst_ref_mut || lid = dst_ref_shared)

let assert_slice (t : K.typ) =
  match t with
  | TApp (lid, [ t; u ]) when is_dst_ref lid && u = TInt SizeT -> t
  | _ -> fail "Not a slice: %a" ptyp t

let string_of_path_elem (env : env) (p : Charon.Types.path_elem) : string =
  Charon.PrintTypes.path_elem_to_string env.format_env p

let string_of_name env ps = String.concat "::" (List.map (string_of_path_elem env) ps)

let mk_field_name f i =
  match f with
  | Some f -> f
  | None -> "f" ^ string_of_int i

let is_enum (env : env) (id : C.type_decl_id) : bool =
  let decl = env.get_nth_type id in
  match decl.C.kind with
  | Enum branches -> List.for_all (fun v -> v.C.fields = []) branches
  | _ -> false

let mk_enum_case lid c = fst lid @ [ snd lid ], c

(* Helpers: traits finding & matching *)

module RustNames = struct
  open Charon.NameMatcher

  let config =
    {
      map_vars_to_vars = false;
      match_with_trait_decl_refs = true;
      (* use_trait_decl_refs = true; *)
    }

  let vec = parse_pattern "alloc::vec::Vec<@>"
  let range = parse_pattern "core::ops::range::Range<@>"
  let option = parse_pattern "core::option::Option<@>"

  (* Just to have a uniform view of the table of distinguished declarations *)
  let builtin_of_function decl : Builtin.builtin =
    match decl with
    | Krml.Ast.DFunction (_, _, n_cgs, n_type_args, ret_t, name, binders, _) ->
        if n_cgs > List.length binders then
          Krml.Warn.fatal_error "n_cgs=%d, but List.length binders=%d for %a\n" n_cgs
            (List.length binders) Krml.PrintAst.Ops.plid name;
        let cg_args, rest = Krml.KList.split n_cgs binders in
        {
          name;
          typ = Krml.Helpers.fold_arrow (List.map (fun (x : Krml.Ast.binder) -> x.typ) rest) ret_t;
          n_type_args;
          cg_args = List.map (fun (x : Krml.Ast.binder) -> x.typ) cg_args;
          arg_names = List.map (fun (x : Krml.Ast.binder) -> x.node.name) rest;
        }
    | _ -> failwith "impossible"

  let known_builtins =
    [
    (* slices *)
    parse_pattern "SliceIndexShared<'_, @T>", Builtin.slice_index_shared;
    parse_pattern "SliceIndexMut<'_, @T>", Builtin.slice_index_mut;

    parse_pattern "core::slice::index::{core::ops::index::Index<[@T], @I, @Clause2_Output>}::index<'_, @, core::ops::range::Range<usize>, [@]>", builtin_of_function Builtin.slice_subslice_func_shared;
    parse_pattern "core::slice::index::{core::ops::index::IndexMut<[@T], @I, @Clause2_Output>}::index_mut<'_, @, core::ops::range::Range<usize>, [@]>", builtin_of_function Builtin.slice_subslice_func_mut;
    parse_pattern "core::slice::index::{core::ops::index::Index<[@T], @I, @Clause2_Output>}::index<'_, @, core::ops::range::RangeTo<usize>, [@]>", builtin_of_function Builtin.slice_subslice_to_func_shared;
    parse_pattern "core::slice::index::{core::ops::index::IndexMut<[@T], @I, @Clause2_Output>}::index_mut<'_, @, core::ops::range::RangeTo<usize>, [@]>", builtin_of_function Builtin.slice_subslice_to_func_mut;
    parse_pattern "core::slice::index::{core::ops::index::Index<[@T], @I, @Clause2_Output>}::index<'_, @, core::ops::range::RangeFrom<usize>, [@]>", builtin_of_function Builtin.slice_subslice_from_func_shared;
    parse_pattern "core::slice::index::{core::ops::index::IndexMut<[@T], @I, @Clause2_Output>}::index_mut<'_, @, core::ops::range::RangeFrom<usize>, [@]>", builtin_of_function Builtin.slice_subslice_from_func_mut;

    (* arrays *)
    parse_pattern "core::array::{core::ops::index::Index<[@T; @N], @I, @Clause2_Clause0_Output>}::index<'_, @, core::ops::range::Range<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_func_shared;
    parse_pattern "core::array::{core::ops::index::IndexMut<[@T; @N], @I, @Clause2_Clause0_Output>}::index_mut<'_, @, core::ops::range::Range<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_func_mut;
    parse_pattern "core::array::{core::ops::index::Index<[@T; @N], @I, @Clause2_Clause0_Output>}::index<'_, @, core::ops::range::RangeTo<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_to_func_shared;
    parse_pattern "core::array::{core::ops::index::IndexMut<[@T; @N], @I, @Clause2_Clause0_Output>}::index_mut<'_, @, core::ops::range::RangeTo<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_to_func_mut;
    parse_pattern "core::array::{core::ops::index::Index<[@T; @N], @I, @Clause2_Clause0_Output>}::index<'_, @, core::ops::range::RangeFrom<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_from_func_shared;
    parse_pattern "core::array::{core::ops::index::IndexMut<[@T; @N], @I, @Clause2_Clause0_Output>}::index_mut<'_, @, core::ops::range::RangeFrom<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_from_func_mut;

    (* slices <-> arrays *)
    parse_pattern "ArrayToSliceShared<'_, @T, @N>", builtin_of_function Builtin.array_to_slice_func_shared;
    parse_pattern "ArrayToSliceMut<'_, @T, @N>", builtin_of_function Builtin.array_to_slice_func_mut;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ [@T], [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_array;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ mut [@T], [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_array;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ [@T], &'_ [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_ref_array;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ mut [@T], &'_ mut [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_ref_array;

    (* bitwise & arithmetic operations *)
    parse_pattern "core::ops::bit::BitAnd<&'_ u8, u8>::bitand", Builtin.bitand_pv_u8;
    parse_pattern "core::ops::bit::Shr<&'_ u8, i32>::shr", Builtin.shr_pv_u8;

    (* misc *)
    parse_pattern "core::cmp::Ord<u32>::min", Builtin.min_u32;

    (* boxes *)
    parse_pattern "alloc::boxed::{alloc::boxed::Box<@T>}::new<@>", Builtin.box_new;
  ]
  [@ocamlformat "disable"]

  let known_builtins_no_const =
    [
    (* slices *)
    parse_pattern "SliceIndexShared<'_, @T>", Builtin.slice_index_mut;
    parse_pattern "SliceIndexMut<'_, @T>", Builtin.slice_index_mut;

    parse_pattern "core::slice::index::{core::ops::index::Index<[@T], @I, @Clause2_Output>}::index<'_, @, core::ops::range::Range<usize>, [@]>", builtin_of_function Builtin.slice_subslice_func_mut;
    parse_pattern "core::slice::index::{core::ops::index::IndexMut<[@T], @I, @Clause2_Output>}::index_mut<'_, @, core::ops::range::Range<usize>, [@]>", builtin_of_function Builtin.slice_subslice_func_mut;
    parse_pattern "core::slice::index::{core::ops::index::Index<[@T], @I, @Clause2_Output>}::index<'_, @, core::ops::range::RangeTo<usize>, [@]>", builtin_of_function Builtin.slice_subslice_to_func_mut;
    parse_pattern "core::slice::index::{core::ops::index::IndexMut<[@T], @I, @Clause2_Output>}::index_mut<'_, @, core::ops::range::RangeTo<usize>, [@]>", builtin_of_function Builtin.slice_subslice_to_func_mut;
    parse_pattern "core::slice::index::{core::ops::index::Index<[@T], @I, @Clause2_Output>}::index<'_, @, core::ops::range::RangeFrom<usize>, [@]>", builtin_of_function Builtin.slice_subslice_from_func_mut;
    parse_pattern "core::slice::index::{core::ops::index::IndexMut<[@T], @I, @Clause2_Output>}::index_mut<'_, @, core::ops::range::RangeFrom<usize>, [@]>", builtin_of_function Builtin.slice_subslice_from_func_mut;

    (* arrays *)
    parse_pattern "core::array::{core::ops::index::Index<[@T; @N], @I, @Clause2_Clause0_Output>}::index<'_, @, core::ops::range::Range<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_func_mut;
    parse_pattern "core::array::{core::ops::index::IndexMut<[@T; @N], @I, @Clause2_Clause0_Output>}::index_mut<'_, @, core::ops::range::Range<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_func_mut;
    parse_pattern "core::array::{core::ops::index::Index<[@T; @N], @I, @Clause2_Clause0_Output>}::index<'_, @, core::ops::range::RangeTo<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_to_func_mut;
    parse_pattern "core::array::{core::ops::index::IndexMut<[@T; @N], @I, @Clause2_Clause0_Output>}::index_mut<'_, @, core::ops::range::RangeTo<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_to_func_mut;
    parse_pattern "core::array::{core::ops::index::Index<[@T; @N], @I, @Clause2_Clause0_Output>}::index<'_, @, core::ops::range::RangeFrom<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_from_func_mut;
    parse_pattern "core::array::{core::ops::index::IndexMut<[@T; @N], @I, @Clause2_Clause0_Output>}::index_mut<'_, @, core::ops::range::RangeFrom<usize>, [@], @>", builtin_of_function Builtin.array_to_subslice_from_func_mut;

    (* slices <-> arrays *)
    parse_pattern "ArrayToSliceShared<'_, @T, @N>", builtin_of_function Builtin.array_to_slice_func_mut;
    parse_pattern "ArrayToSliceMut<'_, @T, @N>", builtin_of_function Builtin.array_to_slice_func_mut;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ [@T], [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_array;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ mut [@T], [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_array;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ [@T], &'_ [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_ref_array;
    parse_pattern "core::convert::{core::convert::TryInto<@T, @U, @Clause2_Error>}::try_into<&'_ mut [@T], &'_ mut [@T; @], core::array::TryFromSliceError>", Builtin.slice_to_ref_array;

    (* bitwise & arithmetic operations *)
    parse_pattern "core::ops::bit::BitAnd<&'_ u8, u8>::bitand", Builtin.bitand_pv_u8;
    parse_pattern "core::ops::bit::Shr<&'_ u8, i32>::shr", Builtin.shr_pv_u8;

    (* misc *)
    parse_pattern "core::cmp::Ord<u32>::min", Builtin.min_u32;

    (* boxes *)
    parse_pattern "alloc::boxed::{alloc::boxed::Box<@T>}::new<@>", Builtin.box_new;
  ]
  [@ocamlformat "disable"]

  let from_u16 = parse_pattern "core::convert::From<u16, @U>::from"
  let from_u32 = parse_pattern "core::convert::From<u32, @U>::from"
  let from_u64 = parse_pattern "core::convert::From<u64, @U>::from"
  let from_u128 = parse_pattern "core::convert::From<u128, @U>::from"
  let from_i16 = parse_pattern "core::convert::From<i16, @U>::from"
  let from_i32 = parse_pattern "core::convert::From<i32, @U>::from"
  let from_i64 = parse_pattern "core::convert::From<i64, @U>::from"
  let from_i128 = parse_pattern "core::convert::From<i128, @U>::from"
  let from = parse_pattern "core::convert::From<@T, @U>::from"
  let into_u16 = parse_pattern "core::convert::Into<@U, u16>::into"
  let into_u32 = parse_pattern "core::convert::Into<@U, u32>::into"
  let into_u64 = parse_pattern "core::convert::Into<@U, u64>::into"
  let into_u128 = parse_pattern "core::convert::Into<@U, u128>::into"
  let into_i16 = parse_pattern "core::convert::Into<@U, i16>::into"
  let into_i32 = parse_pattern "core::convert::Into<@U, i32>::into"
  let into_i64 = parse_pattern "core::convert::Into<@U, i64>::into"
  let into_i128 = parse_pattern "core::convert::Into<@U, i128>::into"
  let into = parse_pattern "core::convert::Into<@U, @T>::into"
  let is_vec env = match_pattern_with_type_id env.name_ctx config (mk_empty_maps ()) vec
  let is_range env = match_pattern_with_type_id env.name_ctx config (mk_empty_maps ()) range
  let is_option env = match_pattern_with_type_id env.name_ctx config (mk_empty_maps ()) option
end

let string_of_pattern pattern = Charon.NameMatcher.(pattern_to_string { tgt = TkPattern } pattern)

let pattern_of_fn_ptr env fn_ptr =
  Charon.NameMatcher.(
    fn_ptr_to_pattern env.name_ctx
      { tgt = TkPattern; use_trait_decl_refs = true }
      Charon.TypesUtils.empty_generic_params fn_ptr)

let pattern_of_name env name =
  Charon.NameMatcher.(
    name_to_pattern env.name_ctx { tgt = TkPattern; use_trait_decl_refs = true } name)

let string_of_fn_ptr env fn_ptr = string_of_pattern (pattern_of_fn_ptr env fn_ptr)

(** Translation of types *)

let lid_of_name (env : env) (name : Charon.Types.name) : K.lident =
  let prefix, name = Krml.KList.split_at_last name in
  List.map (string_of_path_elem env) prefix, string_of_path_elem env name

let width_of_integer_type (t : Charon.Types.integer_type) : K.width =
  match t with
  | Signed Isize -> PtrdiffT
  | Signed I8 -> Int8
  | Signed I16 -> Int16
  | Signed I32 -> Int32
  | Signed I64 -> Int64
  | Signed I128 ->
      failwith "Internal error: `i128` should not be handled in `width_of_integer_type`."
  | Unsigned Usize -> SizeT
  | Unsigned U8 -> UInt8
  | Unsigned U16 -> UInt16
  | Unsigned U32 -> UInt32
  | Unsigned U64 -> UInt64
  | Unsigned U128 ->
      failwith "Internal error: `u128` should not be handled in `width_of_integer_type`."

let lid_of_type_decl_id (env : env) (id : C.type_decl_id) =
  let { C.item_meta; _ } = env.get_nth_type id in
  lid_of_name env item_meta.name

let constant_of_scalar_value sv =
  let w = width_of_integer_type (Charon.Scalars.get_ty sv) in
  w, Z.to_string (Charon.Scalars.get_val sv)

let assert_cg_scalar = function
  | C.CgValue (VScalar n) -> n
  | cg -> failwith ("Unsupported: non-constant const generic: " ^ C.show_const_generic cg)

let cg_of_const_generic env cg =
  match cg with
  | C.CgVar var -> K.CgVar (fst (lookup_cg_in_types env (C.expect_free_var var)))
  | C.CgValue (VScalar sv) -> CgConst (constant_of_scalar_value sv)
  | _ ->
      failwith
        ("cg_of_const_generic: " ^ Charon.PrintTypes.const_generic_to_string env.format_env cg)

let float_width float_ty : K.width =
  match float_ty with
  | C.F32 -> Float32
  | C.F64 -> Float64
  | C.F16 | C.F128 -> failwith "TODO: f16 & f128 are not supported."

let typ_of_literal_ty (_env : env) (ty : Charon.Types.literal_type) : K.typ =
  match ty with
  | TBool -> K.TBool
  | TChar -> Builtin.char_t
  | TFloat f -> K.TInt (float_width f)
  | TInt C.I128 -> Builtin.int128_t
  | TUInt C.U128 -> Builtin.uint128_t
  | _ -> K.TInt (width_of_integer_type (Charon.TypesUtils.literal_as_integer ty))

let const_of_ref_kind kind =
  if !Options.no_const then
    false
  else
    match kind with
    | C.RMut -> false
    | C.RShared -> true

let const_of_borrow_kind bk =
  if !Options.no_const then
    false
  else
    match bk with
    | C.BShared -> true
    | C.BShallow -> true
    | _ -> false

let const_of_tbuf b =
  if !Options.no_const then
    false
  else
    match b with
    | K.TBuf (_, const) -> const
    | _ -> failwith "not a tbuf"

(* e: Eurydice_dst<t> *)
let mk_dst_deref _env t e =
  (* ptr_field: t* *)
  (* XXX need proper const here *)
  let ptr_field = K.(with_type (TBuf (t, true)) (EField (e, "ptr"))) in
  K.(with_type t (EBufRead (ptr_field, Krml.Helpers.zero_usize)))

let ensure_named i name =
  match name, i with
  | None, 0 -> "fst"
  | None, 1 -> "snd"
  | None, 2 -> "thd"
  | None, _ -> Printf.sprintf "field%d" i
  | Some name, _ -> name

let lookup_field env typ_id field_id =
  let ty_decl = env.get_nth_type typ_id in
  let fields =
    match ty_decl.kind with
    | Struct fields -> fields
    | _ -> failwith "not a struct"
  in
  let i = C.FieldId.to_int field_id in
  let field = List.nth fields i in
  ensure_named i field.field_name

let mk_expr_arr_struct (expr_array : K.expr) = K.EFlat [ Some "data", expr_array ]

(** A vtable type is actually just a struct type, this function takes out the hinted ID of the
    vtable struct from the dynamic predicate and then translate it, returns a reference to the
    struct type itself.

    Notably, a dynamic predicate is of the form:
    [dyn Trait<TraitArgs, AssocTy=T, ...> + AutoTraits + 'a]. Namely, it has the arguments to the
    unique non-auto trait and the assignments to each associated types as the **principal trait**
    [Trait<...>], and then some additional auto-traits having no methods and finally a region
    constraint. Here we care only about the principal trait. *)
let rec vtable_typ_of_dyn_pred (env : env) (pred : C.dyn_predicate) : K.typ =
  let binder_params = pred.binder.binder_params in
  match binder_params.trait_clauses with
  | [] -> failwith "DynTrait has empty clause! Please report this to Charon."
  (* As guaranteed by Rustc and hence Charon, the first clause must be the principal trait *)
  | principal_clause :: _ -> (
      let tref = principal_clause.trait in
      let decl = env.get_nth_trait_decl tref.binder_value.id in
      match decl.vtable with
      | None ->
          failwith "Fetching vtable from a trait without vtable! Please report this to Charon."
      | Some ty_ref ->
          (* The ty_ref here is of the form: `{vtable}<TraitParams, AssocTys>` *)
          (* Hence we need to firstly substitute `TraitParams` with the actual types provided by the dynamic predicate's `TraitArgs` *)
          (* And then substitute `AssocTys` with the actual types provided by the dynamic predicate's assignments to these associated types *)
          (* The trait ref is guaranteed to be with empty binding values in the principal clause *)
          (* Yet, we will need to move shift the internal DeBruijn indices with `extract_from_binder` *)
          let tref = Charon.Substitute.(extract_from_binder trait_decl_ref_substitute tref) in
          (* First step: get the `TraitArgs` *)
          let base_args =
            let generics = tref.generics in
            { generics with types = List.tl generics.types }
          in
          (* Second step: find from the assignments of assoc tys *)
          let assoc_tys =
            let base_removal = fun l -> snd (Krml.KList.split (List.length base_args.types) l) in
            let find_assoc_ty_arg = function
              | C.TTraitType (tref, name) ->
                  (* We match by the trait-ID and the assoc-ty name, which should be unique *)
                  let target_id = tref.trait_decl_ref.binder_value.id in
                  let assoc_ty_assns = binder_params.trait_type_constraints in
                  let finder (binded_assn : C.trait_type_constraint C.region_binder) =
                    let assn =
                      Charon.Substitute.(
                        extract_from_binder trait_type_constraint_substitute binded_assn)
                    in
                    if
                      assn.trait_ref.trait_decl_ref.binder_value.id = target_id
                      && assn.type_name = name
                    then
                      Some assn.ty
                    else
                      None
                  in
                  begin
                    match List.find_map finder assoc_ty_assns with
                    | Some ty -> ty
                    | None ->
                        fail "Could not find associated type assignment for associated type %s" name
                  end
              | _ ->
                  failwith
                    "This should not happen: the rest of the generic types in a vtable-ref in a \
                     trait-decl should all be referring to associated types."
            in
            ty_ref.generics.types
            (* Remove the base param types from it, leaving the associated types params *)
            |> base_removal
            (* For each assoc type param, find the corresponding arg from the dyn predicate *)
            |> List.map find_assoc_ty_arg
          in
          let args = { base_args with types = base_args.types @ assoc_tys } in
          let ty_args_ref = { ty_ref with generics = args } in
          typ_of_ty env (C.TAdt ty_args_ref))

(* This functions takes a Charon type, and returns the associated metadata as a krml type,
   or None if there is no metadata *)
and metadata_typ_of_ty (env : env) (ty : Charon.Types.ty) : K.typ option =
  match ty with
  | C.TAdt ty_decl_ref -> begin
      match ty_decl_ref.id with
      | C.TAdtId decl_id -> begin
          let decl = env.get_nth_type decl_id in
          match decl.ptr_metadata with
          | C.NoMetadata -> None
          | C.Length -> Some Krml.Helpers.usize
          | C.VTable ty_ref ->
              let ty_ref = { ty_ref with generics = ty_decl_ref.generics } in
              Some (K.TBuf (typ_of_ty env (C.TAdt ty_ref), false))
          | C.InheritFrom (C.TVar (C.Free var)) ->
              let ty = List.nth ty_decl_ref.generics.types (C.TypeVarId.to_int var) in
              metadata_typ_of_ty env ty
          | C.InheritFrom _ ->
              failwith
                "Eurydice does not handle PtrMetadata inheritance, please consider using \
                 monomorphized LLBC"
        end
      | C.TTuple -> begin
          match List.rev @@ ty_decl_ref.generics.types with
          (* Empty metadata for empty tuple *)
          | [] -> None
          (* For tuple, the type of metadata is the last element *)
          | last :: _ -> metadata_typ_of_ty env last
        end
      | C.TBuiltin C.TBox | C.TBuiltin C.TArray -> None
      | C.TBuiltin C.TSlice | C.TBuiltin C.TStr -> Some Krml.Helpers.usize
    end
  | C.TVar _ ->
      (* TEMPORARY:  this needs to be enabled once the monomorphization PR lands -- 
    for now, there are still polymorphic type definitions of the form type 
    Foo<T: ?Sized> = ... in the AST (those will be monomorphized away, eventually) *)
      let _var_is_sized (clause : C.trait_param) =
        let trait = clause.trait.binder_value in
        match trait.generics.types with
        | [] -> failwith "Unexpected empty `Self` from trait clause."
        (* The variable here in C.TVar should be free variable, hence no need to adjust the DB id. *)
        | self :: _ ->
            self = ty
            &&
            let decl = env.get_nth_trait_decl trait.id in
            (* It has the Sized marker, which is of lang_item "sized" *)
            decl.item_meta.lang_item = Some "sized"
      in
      let var_is_sized =
        true
        (*List.exists var_is_sized env.generic_params.trait_clauses*)
      in
      if var_is_sized then
        None
      else
        failwith
          "Eurydice does not handle taking metadata from non-Sized type vars, please consider \
           using monomorphized LLBC."
  | C.TTraitType (_, _) ->
      failwith
        "Eurydice does not handle taking metadata from assoc types, please consider using \
         monomorphized LLBC."
  (* The metadata of a &dyn Trait object is a pointer to its vtable *)
  | C.TDynTrait pred -> Some (K.TBuf (vtable_typ_of_dyn_pred env pred, false))
  | C.TLiteral _ | C.TRef (_, _, _) | C.TRawPtr (_, _) | C.TFnPtr _ | C.TFnDef _ -> None
  (* The metadata must not have ptr metadata as they must be Sized. *)
  | C.TPtrMetadata _ -> None
  | C.TNever | C.TError _ -> failwith "Error types to fetch metadata"

(* Translate Charon type &T as a krml type -- this handles special cases
 where address-taking in Rust creates a DST, which we represent as instances
  of the dst_ref type. There are many such cases: slices, &str, etc. *)
and ptr_typ_of_ty (env : env) ~const (ty : Charon.Types.ty) : K.typ =
  (* Handle special cases first *)
  match ty with
  (* Special case to handle slice : &[T] *)
  | TAdt { id = TBuiltin TSlice; generics = { types = [ t ]; _ } } ->
      Builtin.mk_slice ~const (typ_of_ty env t)
  (* Special case to handle &str *)
  | TAdt { id = TBuiltin TStr; _ } -> Builtin.str_t ~const
  (* Special case to handle DynTrait *)
  | TDynTrait pred ->
      Builtin.mk_dst_ref ~const Builtin.c_void_t (K.TBuf (vtable_typ_of_dyn_pred env pred, false))
  (* General case, all &T is turned to either thin T* or fat Eurydice::DstRef<T,Meta> *)
  | _ -> (
      let typ = typ_of_ty env ty in
      match metadata_typ_of_ty env ty with
      | None -> K.TBuf (typ, const)
      | Some meta -> Builtin.mk_dst_ref ~const typ meta)

and typ_of_ty (env : env) (ty : Charon.Types.ty) : K.typ =
  match ty with
  | TVar var -> K.TBound (lookup_typ env (C.expect_free_var var))
  | TLiteral t -> typ_of_literal_ty env t
  | TNever -> failwith "Impossible: Never"
  | TDynTrait _ -> failwith "TODO: dyn Trait"
  | TAdt { id = TBuiltin TBox; generics = { types = [ t ]; _ } } -> ptr_typ_of_ty ~const:false env t
  | TRef (_, t, rk) | TRawPtr (t, rk) ->
      let const = const_of_ref_kind rk in
      ptr_typ_of_ty env ~const t
  | TAdt { id; generics = { types = [ t ]; _ } as generics } when RustNames.is_vec env id generics
    -> Builtin.mk_vec (typ_of_ty env t)
  | TAdt { id = TAdtId id; generics = { types = args; const_generics = generic_args; _ } } ->
      let ts = List.map (typ_of_ty env) args in
      let cgs = List.map (cg_of_const_generic env) generic_args in
      let lid = lid_of_type_decl_id env id in
      K.fold_tapp (lid, ts, cgs)
  | TAdt { id = TTuple; generics = { types = args; const_generics; _ } } ->
      assert (const_generics = []);
      begin
        match args with
        | [] -> TUnit
        | [ t ] -> typ_of_ty env t (* charon issue #205 *)
        | _ -> TTuple (List.map (typ_of_ty env) args)
      end
  | TAdt { id = TBuiltin TArray; generics = { types = [ t ]; const_generics = [ cg ]; _ } } ->
      typ_of_struct_arr env t cg
  | TAdt { id = TBuiltin TSlice; generics = { types = [ t ]; _ } } ->
      (* Appears in instantiations of patterns and generics, so we translate it to a placeholder. *)
      TApp (Builtin.derefed_slice, [ typ_of_ty env t ])
  | TAdt { id = TBuiltin TStr; generics = { types = []; _ } } -> Builtin.deref_str_t
  | TAdt { id = TBuiltin f; generics = { types = args; const_generics; _ } } ->
      List.iter (fun x -> print_endline (C.show_const_generic x)) const_generics;
      fail "TODO: Adt/Builtin %s (%d) %d " (C.show_builtin_ty f) (List.length args)
        (List.length const_generics)
  | TTraitType _ -> failwith ("TODO: TraitTypes " ^ Charon.PrintTypes.ty_to_string env.format_env ty)
  | TFnPtr fn_sig ->
      let ts, t = fn_sig.binder_value in
      let typs = List.map (typ_of_ty env) ts in
      let typs =
        match typs with
        | [] -> [ K.TUnit ]
        | typs -> typs
      in
      begin
        match typ_of_ty env t with
        | TArrow _ ->
            failwith
              "Function pointer `fn` currying is not supported, consider using `&'static dyn Fn` \
               instead."
        | typ -> Krml.Helpers.fold_arrow typs typ
      end
  | TFnDef bound_fn_ref -> begin
      match Charon.Substitute.lookup_fndef_sig env.crate bound_fn_ref with
      | None -> failwith "Missing function declaration"
      | Some fn_sig -> typ_of_ty env (TFnPtr fn_sig)
    end
  | TPtrMetadata _ ->
      failwith
        "The type-level computation `PtrMetadata(t)` should be handled by Charon, consider using \
         monomorphised LLBC."
  | TError _ -> failwith "Found type error in charon's output"

and typ_of_struct_arr (env : env) (t : C.ty) (cg : C.const_generic) : K.typ =
  let typ_t = typ_of_ty env t in
  let cg = cg_of_const_generic env cg in
  Builtin.mk_arr typ_t cg

let maybe_cg_array (env : env) (t : C.ty) (cg : C.const_generic) =
  match cg with
  | CgValue _ -> K.TArray (typ_of_ty env t, constant_of_scalar_value (assert_cg_scalar cg))
  | CgVar var ->
      let id, cg_t = lookup_cg_in_types env (C.expect_free_var var) in
      assert (cg_t = K.TInt SizeT);
      K.TCgArray (typ_of_ty env t, id)
  | _ -> failwith "TODO: CgGlobal"

(* Environment: expressions *)

let is_var v2 v1 =
  match v2 with
  | Var (v2, _) -> v2 = v1
  | _ -> false

let assert_var = function
  | Var (v2, ty) -> v2, ty
  | _ -> assert false

let assert_trait_clause_method = function
  | TraitClauseMethod { clause_id; item_name; ts; _ } -> clause_id, item_name, ts
  | _ -> assert false

(* Regular binders *)

let lookup env v1 =
  let i, (_, t) = findi (fun (v2, _) -> is_var v2 v1) env.binders in
  i, t

let lookup_with_original_type env v1 =
  let i, (v, t) = findi (fun (v2, _) -> is_var v2 v1) env.binders in
  let _, ty = assert_var v in
  i, t, ty

(* Const generic binders *)

let push_cg_binder env (t : C.const_generic_param) =
  {
    env with
    cg_binders = (t.index, typ_of_literal_ty env t.ty) :: env.cg_binders;
    binders = (ConstGenericVar t.index, typ_of_literal_ty env t.ty) :: env.binders;
  }

let push_cg_binders env (ts : C.const_generic_param list) = List.fold_left push_cg_binder env ts

let push_binder env (t : C.local) =
  { env with binders = (Var (t.index, t.local_ty), typ_of_ty env t.local_ty) :: env.binders }

let push_binders env (ts : C.local list) = List.fold_left push_binder env ts

(* Clause binders, which only appear as function parameters, and hold an unknown
   trait method (dictionary-style). *)
let push_clause_binder env b = { env with binders = b :: env.binders }
let push_clause_binders env bs = List.fold_left push_clause_binder env bs

let lookup_clause_binder env clause_id item_name =
  let i, (v, t) =
    try
      findi
        (function
          | TraitClauseMethod { clause_id = clause_id2; item_name = item_name2; _ }, _
          | TraitClauseConstant { clause_id = clause_id2; item_name = item_name2; _ }, _ ->
              clause_id2 = clause_id && item_name2 = item_name
          | _ -> false)
        env.binders
    with Not_found ->
      Krml.KPrint.bprintf "Error looking up %s.%s\n" (C.show_trait_ref_kind clause_id) item_name;
      raise Not_found
  in
  i, t, v

let lookup_clause_method env clause_id item_name =
  let i, t, v = lookup_clause_binder env clause_id item_name in
  i, t, thd3 (assert_trait_clause_method v)

let lookup_clause_constant env clause_id item_name =
  let i, t, _ = lookup_clause_binder env clause_id item_name in
  i, t

(** Translation of expressions (statements, operands, rvalues, places) *)

let uu =
  let r = ref 0 in
  fun () ->
    let suffix = string_of_int !r in
    incr r;
    "uu____" ^ suffix

let binder_of_var (env : env) (l : C.local) : K.binder =
  let name = Option.value ~default:(uu ()) l.name in
  let meta =
    match name with
    | "left_val" | "right_val" -> [ K.AttemptInline ]
    | _ -> []
  in
  let binder = Krml.Helpers.fresh_binder ~mut:true name (typ_of_ty env l.local_ty) in
  { binder with node = { binder.node with meta = meta @ binder.node.meta } }

let find_nth_variant (env : env) (typ : C.type_decl_id) (var : C.variant_id) =
  match env.get_nth_type typ with
  | { kind = Enum variants; _ } -> Charon.Types.VariantId.nth variants var
  | _ -> failwith "impossible: type is not a variant"

let rec with_locals (env : env) (t : K.typ) (locals : C.local list) (k : env -> 'a) : 'a =
  match locals with
  | [] -> k env
  | l :: locals ->
      let env = push_binder env l in
      let b = binder_of_var env l in
      K.(with_type t (ELet (b, Krml.Helpers.any, with_locals env t locals k)))

let lookup_cg_in_expressions (env : env) (v1 : C.const_generic_var_id) =
  let i, (_, t) = findi (fun (v2, _) -> v2 = ConstGenericVar v1) env.binders in
  i, t

let expression_of_cg_var_id env v =
  let i, t = lookup_cg_in_expressions env v in
  K.(with_type t (EBound i))

let expression_of_var_id (env : env) (v : C.local_id) : K.expr =
  let i, t = lookup env v in
  K.(with_type t (EBound i))

(** Assume here the maximum length is 128-bit -- will throw away the larger if larger. This is a
    helper function to split a 128-bit integer into two 64-bit integers and is not assumed to be
    used in other contexts. Returns the **expr** pair (high64bits, low64bits) *)
let split_128bit (value : Z.t) =
  let mask128 = Z.sub (Z.shift_left Z.one 128) Z.one in
  let mask64 = Z.sub (Z.shift_left Z.one 64) Z.one in
  (* Always truncate to 128 bits using bitwise AND *)
  let value = Z.logand value mask128 in
  (* Extract low 64 bits *)
  let low64 = Z.logand mask64 value in
  (* Shift right without sign extension (use logical shift) *)
  let high64 = Z.shift_right value 64 in
  let to_expr_u64bits v =
    let print_Z z = Z.format "%#x" z in
    K.with_type (K.TInt UInt64) @@ K.EConstant (UInt64, print_Z v)
  in
  to_expr_u64bits high64, to_expr_u64bits low64

let expression_of_int128_t (value : Z.t) =
  let i128_max = Z.sub (Z.shift_left Z.one 127) Z.one in
  if value > i128_max then
    failwith "value is larger than the maximum value of i128";
  let i128_min = Z.neg (Z.shift_left Z.one 127) in
  if value < i128_min then
    failwith "value is smaller than the minimum value of i128";
  let high64, low64 = split_128bit value in
  K.(with_type Builtin.int128_t (EApp (Builtin.(get_128_op ("i", "from_bits")), [ high64; low64 ])))

let expression_of_uint128_t (value : Z.t) =
  let u128_max = Z.sub (Z.shift_left Z.one 128) Z.one in
  if value > u128_max then
    failwith "value is larger than the maximum value of u128";
  let high64, low64 = split_128bit value in
  K.(
    with_type Builtin.uint128_t (EApp (Builtin.(get_128_op ("u", "from_bits")), [ high64; low64 ])))

let expression_of_scalar_value sv : K.expr =
  let int_ty = Charon.Scalars.get_ty sv in
  let value = Charon.Scalars.get_val sv in
  match int_ty with
  | C.Signed C.I128 -> expression_of_int128_t value
  | C.Unsigned C.U128 -> expression_of_uint128_t value
  | _ ->
      let w = width_of_integer_type int_ty in
      K.(with_type (TInt w) (EConstant (constant_of_scalar_value sv)))

let expression_of_literal (_env : env) (l : C.literal) : K.expr =
  match l with
  | VScalar sv -> expression_of_scalar_value sv
  | VBool b -> K.(with_type TBool (EBool b))
  | VStr s ->
      let ascii = Utf8.ascii_of_utf8_str s in
      let len = String.length s in
      K.(
        with_type (Builtin.str_t ~const:true)
          (EFlat
             [
               Some "ptr", with_type Krml.Checker.c_string (EString ascii);
               Some "meta", with_type Krml.Helpers.usize (EConstant (SizeT, string_of_int len));
             ]))
  | VChar c -> K.(with_type Builtin.char_t (EConstant (UInt32, string_of_int @@ Uchar.to_int c)))
  | VByteStr lst ->
      let str = List.map (Printf.sprintf "%#x") lst |> String.concat "" in
      K.(with_type Krml.Checker.c_string (EString str))
  | VFloat { C.float_ty; float_value } ->
      let w = float_width float_ty in
      K.(with_type (TInt w) (EConstant (w, float_value)))

let expression_of_const_generic env cg =
  match cg with
  | C.CgGlobal _ -> failwith "TODO: CgGLobal"
  | C.CgVar var -> expression_of_cg_var_id env (C.expect_free_var var)
  | C.CgValue l -> expression_of_literal env l

let has_unresolved_generic (ty : K.typ) : bool =
  (object
     inherit [_] Krml.Ast.reduce
     method zero = false
     method plus = ( || )
     method! visit_TBound _ _ = true
  end)
    #visit_typ
    false ty

let rec expression_of_place (env : env) (p : C.place) : K.expr =
  (* We construct a target expression. Callers may still use the original type to tell arrays
     and references apart, since their *uses* (e.g. addr-of) compile in a type-directed way based on
     the *original* rust type *)
  match p.kind with
  | PlaceLocal var_id ->
      let i, t = lookup env var_id in
      K.(with_type t (EBound i))
  | PlaceGlobal { id; _ } ->
      let global = env.get_nth_global id in
      K.with_type (typ_of_ty env p.ty) (K.EQualified (lid_of_name env global.item_meta.name))
  | PlaceProjection (sub_place, PtrMetadata) -> begin
      let e = expression_of_place env sub_place in
      match e.typ with
      | TApp (lid, [ _; meta_ty ]) when is_dst_ref lid ->
          (* XXX need to adjust *)
          K.(with_type meta_ty (EField (e, "meta")))
      (* In cases like `PtrMetadata(T)` when `T` is a type variable or some types with unresolved type variable,
       We cannot tell the correct metadata type from it until fully monomorphized.
       But we can surely rely on monomorphized LLBC, and we ignore handling such cases in Eurydice. *)
      | ty when has_unresolved_generic ty ->
          failwith
            "Eurydice do not handle ptr-metadata for generic types. Consider using monomorphized \
             LLBC."
      (* Otherwise, fetching ptr-metadata from a non-DST simply results in `()`
       When a type is fully resolved and it is not `Eurydice::DstRef`, we can be confident that it is not a DST. *)
      | _ -> K.with_type TUnit K.EUnit
    end
  | PlaceProjection (sub_place, pe) -> begin
      (* Can't evaluate this here because of the special case for DSTs. *)
      let sub_e = lazy (expression_of_place env sub_place) in
      let ( !* ) = Lazy.force in
      (* L.log "AstOfLlbc" "e=%a\nty=%s\npe=%s\n" pexpr sub_e (C.show_ty sub_place.ty) *)
      (*   (C.show_projection_elem pe); *)
      match pe, sub_place, sub_place.ty with
      (* slices simply cannot be dereferenced into places which have unknown size.
         They are supposed to be reborrowed again directly after the deref which is handled in expression_of_rvalue *)
      | C.Deref, _, TRef (_, TAdt { id = TBuiltin TSlice; _ }, _)
      | C.Deref, _, TRawPtr (TAdt { id = TBuiltin TSlice; _ }, _) -> assert false
      | ( C.Deref,
          _,
          (TRawPtr _ | TRef _ | TAdt { id = TBuiltin TBox; generics = { types = [ _ ]; _ } }) ) ->
          (* All types represented as a pointer at run-time, compiled to a C pointer *)
          begin
            match !*sub_e.K.typ with
            | TBuf (t_pointee, _) ->
                let const =
                  match sub_place.ty with
                  | TRef (_, _, k) -> const_of_ref_kind k
                  | _ -> false
                in
                Krml.Helpers.(mk_deref ~const t_pointee !*sub_e.K.node)
            | t ->
                L.log "AstOfLlbc" "UNHANDLED DEREFERENCE\ne=%a\nt=%a\nty=%s\npe=%s\n" pexpr !*sub_e
                  ptyp t (C.show_ty sub_place.ty) (C.show_projection_elem pe);
                failwith "unhandled dereference"
          end
      | ( Field (ProjAdt (typ_id, None), field_id),
          { kind = PlaceProjection (sub_place, C.Deref); _ },
          C.TAdt _ ) ->
          let field_name = lookup_field env typ_id field_id in
          let sub_e = expression_of_place env sub_place in
          let place_typ = typ_of_ty env p.ty in
          let const =
            match sub_place.ty with
            | TRef (_, _, k) -> const_of_ref_kind k
            | _ -> false
          in
          begin
            match sub_e.K.typ with
            | K.TApp (dst_ref_hd, [ dst_t; _meta ]) when is_dst_ref dst_ref_hd ->
                (* getting field from a fat pointer of DST  *)
                (* XXX need to adjust *)
                K.with_type place_typ (K.EField (mk_dst_deref env dst_t sub_e, field_name))
            | _ ->
                (* Same as below *)
                K.with_type place_typ
                  (K.EField
                     ( Krml.Helpers.(
                         mk_deref ~const
                           (Krml.Helpers.assert_tbuf_or_tarray sub_e.K.typ)
                           sub_e.K.node),
                       field_name ))
          end
      | Field (ProjAdt (typ_id, variant_id), field_id), _, C.TAdt _ -> begin
          let place_typ = typ_of_ty env p.ty in
          match variant_id with
          | None ->
              let field_name = lookup_field env typ_id field_id in
              K.with_type place_typ (K.EField (!*sub_e, field_name))
          | Some variant_id ->
              let variant = find_nth_variant env typ_id variant_id in
              let field_id = C.FieldId.to_int field_id in
              let field = List.nth variant.fields field_id in
              let b =
                Krml.Helpers.fresh_binder (mk_field_name field.C.field_name field_id) place_typ
              in
              K.with_type place_typ
                K.(
                  EMatch
                    ( Unchecked,
                      !*sub_e,
                      [
                        ( [ b ],
                          with_type !*sub_e.typ
                            (PCons
                               ( variant.C.variant_name,
                                 List.init (List.length variant.fields) (fun i ->
                                     if i = field_id then
                                       with_type place_typ (PBound 0)
                                     else
                                       with_type TAny PWild) )),
                          with_type place_typ (EBound 0) );
                      ] ))
        end
      | ( Field (ProjTuple n, i),
          _,
          C.TAdt { id = _; generics = { types = tys; const_generics = cgs; _ } } ) ->
          let place_typ = typ_of_ty env p.ty in
          assert (cgs = []);
          (* match e with (_, ..., _, x, _, ..., _) -> x *)
          let i = Charon.Types.FieldId.to_int i in
          if List.length tys = 1 then begin
            assert (i = 0);
            (* Normalized one-element tuple *)
            !*sub_e
          end
          else
            let ts =
              match !*sub_e.typ with
              | TTuple ts -> ts
              | _ -> assert false
            in
            assert (List.length ts = n);
            let binders = [ Krml.Helpers.fresh_binder (uu ()) place_typ ] in
            let pattern =
              K.with_type !*sub_e.typ
                (K.PTuple
                   (List.mapi
                      (fun i' t ->
                        K.with_type t
                          (if i = i' then
                             K.PBound 0
                           else
                             PWild))
                      ts))
            in
            let expr = K.with_type place_typ (K.EBound 0) in
            K.with_type place_typ (K.EMatch (Unchecked, !*sub_e, [ binders, pattern, expr ]))
      (* | PlaceProjection () *)
      | _ -> fail "unexpected / ill-typed projection"
    end

let expression_of_place (env : env) (p : C.place) : K.expr =
  L.log "AstOfLlbc" "expression of place: %s" (C.show_place p);
  expression_of_place env p

(* We produce bit-wise operators first, then when the type is of booleans, we
   change into the non-B variants (Rust does not distinguish between bitwise and
   boolean operators) *)
let op_of_unop (op : C.unop) : Krml.Constant.op =
  match op with
  | C.Not -> BNot
  | C.Neg _ -> Neg
  | _ -> failwith (C.show_unop op)

let op_of_binop (op : C.binop) : Krml.Constant.op =
  match op with
  | C.BitXor -> BXor
  | C.BitAnd -> BAnd
  | C.BitOr -> BOr
  | C.Eq -> Eq
  | C.Lt -> Lt
  | C.Le -> Lte
  | C.Ne -> Neq
  | C.Ge -> Gte
  | C.Gt -> Gt
  | C.Div _ -> Div
  | C.Rem _ -> Mod
  | C.Add _ -> Add
  | C.Sub _ -> Sub
  | C.Mul _ -> Mult
  | C.Shl _ -> BShiftL
  | C.Shr _ -> BShiftR
  | _ -> fail "unsupported operator: %s" (C.show_binop op)

let op_128_of_op kind (op : K.op) : K.expr =
  let op_name =
    match op with
    | Add -> "add"
    | Sub -> "sub"
    | Mult -> "mul"
    | Div -> "div"
    | AddW -> "add"
    | SubW -> "sub"
    | MultW -> "mul"
    | DivW -> "div"
    | Mod -> "mod"
    | BShiftL -> "shl"
    | BShiftR -> "shr"
    | BAnd -> "band"
    | BOr -> "bor"
    | BXor -> "bxor"
    | Eq -> "eq"
    | Neq -> "neq"
    | Lt -> "lt"
    | Lte -> "lte"
    | Gt -> "gt"
    | Gte -> "gte"
    | Neg -> "neg"
    | BNot -> "bnot"
    | _ -> failwith "Unsupported operation for uint128"
  in
  Builtin.get_128_op (kind, op_name)

let mk_op_app (op : K.op) (first : K.expr) (rest : K.expr list) : K.expr =
  (* For 128-bit integers, the case is different: convert the operator & match the case here *)
  let op, ret_t =
    if first.typ = Builtin.int128_t || first.typ = Builtin.uint128_t then
      let op =
        if first.typ = Builtin.int128_t then
          op_128_of_op "i" op
        else
          op_128_of_op "u" op
      in
      let ret_t, _ = Krml.Helpers.flatten_arrow op.typ in
      op, ret_t
    else
      (* Otherwise, simply the normal case *)
      let w =
        match first.typ with
        | K.TInt w -> w
        | K.TBool -> Bool
        | t -> fail "Not an operator type: %a" ptyp t
      in
      let op =
        if first.typ = K.TBool then
          match op with
          | BNot -> Krml.Constant.Not
          | BAnd -> And
          | BOr -> Or
          | BXor -> Xor
          | op -> op
        else
          op
      in
      let op_t = Krml.Helpers.type_of_op op w in
      let op = K.(with_type op_t (EOp (op, w))) in
      let ret_t, _ = Krml.Helpers.flatten_arrow op_t in
      op, ret_t
  in
  (* Rust is super lenient regarding the type of shift operators, we impose u32 -- see
     https://doc.rust-lang.org/std/ops/trait.Shl.html
  *)
  (* Additionally, if the op is `shift` (BShiftL/R for usual, (u)int128_shl/r for 128 bits)
    then the `rest` should be with a single element of type `uint32_t`
    if it is not, turn to type casting. *)
  (* Helper functions for this process *)
  let is_128_bit_shift_lident lident =
    [ Krml.Constant.BShiftL; BShiftR ]
    |> List.concat_map (fun op -> [ op_128_of_op "i" op; op_128_of_op "u" op ])
    |> List.map (fun (x : K.expr) -> Krml.Helpers.assert_elid x.K.node)
    |> List.mem lident
  in
  let modify_rest : K.expr list -> K.expr list = function
    | [ e2 ] -> begin
        match e2.node with
        | EConstant (_, s) ->
            let i = int_of_string s in
            assert (i >= 0);
            [ Krml.Helpers.mk_uint32 i ]
        | _ -> [ K.(with_type (TInt UInt32) (ECast (e2, TInt UInt32))) ]
      end
    | _ ->
        failwith
          "Invalid call to binary operator `shiftl` or `shiftr` -- the number of operands is not 2"
  in
  (* Modify here *)
  let rest =
    match op.node with
    | EOp (BShiftL, _) | EOp (BShiftR, _) -> modify_rest rest
    | EQualified lident when is_128_bit_shift_lident lident -> modify_rest rest
    | _ -> rest
  in
  K.(with_type ret_t (EApp (op, first :: rest)))

let addrof ~const (e : K.expr) = K.(with_type (TBuf (e.typ, const)) (EAddrOf e))

(** Handling trait clauses as dictionaries *)

(* There are two ways that we skip synthesis of trait methods in function calls. The first one is if
   a trait declaration is blocklisted. This happens if the trait has built-in support (e.g.
   FnMut), or if the trait relies on unsupported features (e.g. provided methods,
   used by Iterator's chunk_next, for instance; or associated types; or parent
   clauses). The second way we skip trait methods (further down) is if the
   function is a known builtin implementation. *)
let blocklisted_trait_decls =
  [
    (* These don't have methods *)
    "core::marker::Sized";
    "core::marker::MetaSized";
    "core::marker::PointeeSized";
    "core::marker::Send";
    "core::marker::Sync";
    "core::marker::Tuple";
    "core::marker::Copy";
    (* The traits below *should* be handled properly ASAP. But for now, we have specific *instances*
       of those trait methods in the builtin lookup table, which we then implement by hand with
       macros. *)
    "core::iter::traits::iterator::Iterator";
    "core::iter::range::Step";
    (* TODO: figure out what to do with those *)
    "core::clone::Clone";
    (* TODO: these ones carry the drop_in_place code, but sometimes there's no
       explicit impl (because it's trivial e.g. for usize) which is tricky. *)
    "core::marker::Destruct";
    "core::fmt::Debug";
    "core::ptr::metadata::Thin";
  ]

(* Interpret a Rust function type, with trait bounds, into the krml Ast, providing:
   - the type scheme (fields may be zero)
   - the cg types, which only contains the original Rust const generic variables
   - the argument types, prefixed by the dictionary-style passing of trait clause methods
   - the return type
   - whether the function is builtin, or not. *)
type lookup_result = {
  ts : K.type_scheme; (* just for a sanity check *)
  cg_types : K.typ list;
  arg_types : K.typ list;
  ret_type : K.typ;
  is_known_builtin : bool;
}

let maybe_ts ts t =
  if ts.K.n <> 0 || ts.n_cgs <> 0 then
    K.TPoly (ts, t)
  else
    t

(* For a given function, a (flat) list of all the trait methods that are
   transitively, possibly called by this function, based on the trait bounds in
   its signature.

   Using tests/where_clauses_simple as an example.

   fn double<T: Ops + Copy, U: Ops+Copy> (...)

   this gets desugared to fn double<T,U> where
     T: Ops,      <-- ClauseId 0 (required_methods: add, of_u32)
     T: Copy,     <-- ClauseId 1 (builtin, so neither required nor provided methods)
     U: Ops,      <-- ClauseId 2 (required_methods: add, of_u32)
     U: Copy,     <-- ClauseId 3 (builtin, so neither required nor provided methods)

   the types we obtain by looking up the trait declaration have Self as 0
   (DeBruijn).

   When building a function declaration, this synthesizes all the extra binders
   required for trait methods (passed as function pointers). Assumes type
   variables have been suitably bound in the environment.
*)
let rec mk_clause_binders_and_args env ?depth ?clause_ref (trait_clauses : C.trait_param list) :
    (var_id * K.typ) list =
  let depth = Option.value ~default:"" depth in
  List.concat_map
    (fun tc ->
      let {
        C.clause_id;
        trait = { binder_value = { id = trait_decl_id; generics = trait_generics }; _ };
        _;
      } =
        tc
      in
      let trait_decl = env.get_nth_trait_decl trait_decl_id in
      (* Every item inside the `trait_decl` may refer to generic params of the
         trait. To get items that are valid to return outside of the scope of
         the trait, we must substitute them with the given generics. We should
         in principle substitute everything but we currently don't. This will
         likely be a source of bugs. *)
      let subst = Charon.Substitute.make_subst_from_generics trait_decl.generics trait_generics in
      let substitute_visitor = Charon.Substitute.st_substitute_visitor in

      let name = string_of_name env trait_decl.item_meta.name in
      let clause_ref : C.trait_ref =
        Option.value
          ~default:C.{ kind = C.Clause (Free clause_id); trait_decl_ref = tc.trait }
          clause_ref
      in

      if List.mem name blocklisted_trait_decls then
        []
      else begin
        (* FYI, some clauses like Copy have neither required nor provided methods. *)
        L.log "TraitClauses"
          "%sclause decl %s\n\
          \  id %d:\n\
          \  decl_generics.types are %s\n\
          \  decl_generics.const_generics are %s\n\
          \  methods: %d\n"
          depth name
          (C.TraitClauseId.to_int clause_id)
          (String.concat " ++ " (List.map C.show_ty trait_generics.C.types))
          (String.concat " ++ " (List.map C.show_const_generic trait_generics.C.const_generics))
          (List.length trait_decl.C.methods);

        (* 1. Associated constants *)
        List.map
          (fun (const : C.trait_assoc_const) ->
            let trait_name = trait_decl.C.item_meta.name in
            let pretty_name = string_of_name env trait_name ^ "_" ^ const.C.name in
            let t = substitute_visitor#visit_ty subst const.C.ty in
            let t = typ_of_ty env t in
            ( TraitClauseConstant
                { item_name = const.C.name; pretty_name; clause_id = clause_ref.kind },
              t ))
          trait_decl.C.consts
        (* 2. Trait methods *)
        @ List.map
            (fun (mthd : C.trait_method C.binder) ->
              let item_name = mthd.C.binder_value.C.name in
              let trait_name = trait_decl.C.item_meta.name in
              let pretty_name = string_of_name env trait_name ^ "_" ^ item_name in

              (* Ask charon for the properly bound method signature. *)
              let bound_method_sig : C.fun_sig C.binder C.item_binder =
                Option.get (Charon.Substitute.lookup_method_sig env.crate trait_decl_id item_name)
              in
              (* First we substitute the trait generics. *)
              let bound_method_sig : C.fun_sig C.binder =
                Charon.Substitute.apply_args_to_item_binder clause_ref.kind trait_generics
                  (substitute_visitor#visit_binder substitute_visitor#visit_fun_sig)
                  bound_method_sig
              in

              (* We then construct a polymorphic signature for this method.
                 Its generics are the method generics (the ones in the binder).
                 *)
              let method_sig =
                Charon.Substitute.(
                  (* Variables bound in the inner binder are `Bound`, which
                     eurydice doesn't handle. We therefore make them all `Free`
                     variables, shifting indices to avoid overlap with existing
                     in-scope variables. *)
                  let ambient_ts =
                    { K.n = List.length env.type_binders; K.n_cgs = List.length env.cg_binders }
                  in
                  let shift_ty_var varid =
                    C.TypeVarId.of_int (C.TypeVarId.to_int varid + ambient_ts.K.n)
                  in
                  let shift_cg_var varid =
                    C.ConstGenericVarId.of_int
                      (C.ConstGenericVarId.to_int varid + ambient_ts.K.n_cgs)
                  in
                  (* Replace bound variables with free variables that don't
                     overlap with existing ones. *)
                  let subst =
                    subst_remove_binder_zero
                      {
                        empty_free_sb_subst with
                        ty_sb_subst =
                          (fun varid -> empty_free_sb_subst.ty_sb_subst (shift_ty_var varid));
                        cg_sb_subst =
                          (fun varid -> empty_free_sb_subst.cg_sb_subst (shift_cg_var varid));
                      }
                  in

                  let signature =
                    st_substitute_visitor#visit_fun_sig subst bound_method_sig.binder_value
                  in
                  (* Gotta shift the params too, as trait clause may refer to bound types. *)
                  let method_params =
                    st_substitute_visitor#visit_generic_params subst bound_method_sig.binder_params
                  in
                  (* Finally, update the parameters so they use the new, shifted indices. *)
                  let method_params =
                    {
                      method_params with
                      types =
                        List.map
                          (fun (var : C.type_param) ->
                            { var with C.index = shift_ty_var var.C.index })
                          method_params.types;
                      const_generics =
                        List.map
                          (fun (var : C.const_generic_param) ->
                            { var with C.index = shift_cg_var var.C.index })
                          method_params.const_generics;
                    }
                  in
                  { signature with generics = method_params })
              in
              L.log "TraitClauses" "%s computed method signature %s::%s:\n%s" depth name item_name
                (Charon.PrintGAst.fun_sig_to_string env.format_env "" " " method_sig);
              let ts, t = typ_of_signature env method_sig in
              let t = maybe_ts ts t in
              TraitClauseMethod { item_name; pretty_name; clause_id = clause_ref.kind; ts }, t)
            trait_decl.C.methods
        (* 1 + 2, recursively, for parent traits *)
        @ List.concat_map
            (fun (parent_clause : C.trait_param) ->
              (* Make the clause valid outside the scope of the trait decl. *)
              let parent_clause = substitute_visitor#visit_trait_param subst parent_clause in
              (* Mapping of the methods of the parent clause *)
              let clause_ref : C.trait_ref =
                {
                  kind = ParentClause (clause_ref, parent_clause.clause_id);
                  trait_decl_ref = parent_clause.trait;
                }
              in
              mk_clause_binders_and_args env ~depth:(depth ^ "--") ~clause_ref [ parent_clause ])
            trait_decl.C.implied_clauses
      end)
    trait_clauses

and lookup_signature env depth signature : lookup_result =
  let { C.generics = { types = type_params; const_generics; trait_clauses; _ }; inputs; output; _ }
      =
    signature
  in
  L.log "Calls" "%s# Lookup Signature\n%s--> args: %s, ret: %s\n" depth depth
    (String.concat " ++ " (List.map (Charon.PrintTypes.ty_to_string env.format_env) inputs))
    (Charon.PrintTypes.ty_to_string env.format_env output);
  L.log "Calls" "%sType parameters for this signature: %s\n" depth
    (String.concat " ++ " (List.map Charon.PrintTypes.type_param_to_string type_params));
  let env = push_cg_binders env const_generics in
  let env = push_type_binders env type_params in

  let clause_binders = mk_clause_binders_and_args env trait_clauses in
  debug_trait_clause_mapping env clause_binders;
  let clause_ts = List.map snd clause_binders in

  {
    ts = { n = List.length type_params; n_cgs = List.length const_generics };
    cg_types =
      List.map (fun (v : C.const_generic_param) -> typ_of_literal_ty env v.ty) const_generics;
    arg_types =
      (clause_ts
      @ List.map (typ_of_ty env) inputs
      @
      if inputs = [] then
        [ K.TUnit ]
      else
        []);
    ret_type = typ_of_ty env output;
    is_known_builtin = false;
  }

(* Transforms a lookup result into a usable type, taking into account the fact that the internal Ast
   is ML-style and does not have zero-argument functions. *)
and typ_of_signature env signature =
  let { cg_types = const_generics_ts; arg_types = inputs; ret_type = output; ts; _ } =
    lookup_signature env "" signature
  in

  let adjusted_inputs = const_generics_ts @ inputs in

  let t = Krml.Helpers.fold_arrow adjusted_inputs output in
  ts, t

and debug_trait_clause_mapping env (mapping : (var_id * K.typ) list) =
  if mapping = [] then
    L.log "TraitClauses" "# Debug Mapping\nIn this function, trait clause mapping is empty"
  else
    L.log "TraitClauses"
      "# Debug Mapping\nIn this function, calls to trait bound methods are as follows:";
  List.iter
    (fun (clause_entry, t) ->
      match clause_entry with
      | TraitClauseMethod { clause_id; item_name; ts; _ } ->
          L.log "TraitClauses" "@@@ method name: %s" item_name;
          L.log "TraitClauses" "%s::%s: %a has trait-level %d const generics, %d type vars\n"
            (Charon.PrintTypes.trait_ref_kind_to_string env.format_env None clause_id)
            item_name ptyp t ts.K.n_cgs ts.n
      | TraitClauseConstant { clause_id; item_name; _ } ->
          L.log "TraitClauses" "@@@ method name: %s" item_name;
          L.log "TraitClauses" "%s::%s: associated constant %a\n"
            (Charon.PrintTypes.trait_ref_kind_to_string env.format_env None clause_id)
            item_name ptyp t
      | _ -> ())
    mapping

(** Compiling function instantiations into krml application nodes. *)

(* First step: produce an expression for the un-instantiated function reference, along with all the
   type information required to build a proper instantiation. The function reference is an expression
   that is either a reference to a variable in scope (trait methods), or to a top-level qualified
   name, which encompasses both externally-defined function (builtins), or regular functions. *)
let lookup_fun (env : env) depth (f : C.fn_ptr) : K.expr' * lookup_result =
  let open RustNames in
  let matches p = Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config p f in
  let builtin b =
    let { Builtin.name; typ; n_type_args; cg_args; _ } = b in
    let ret_type, arg_types = Krml.Helpers.flatten_arrow typ in
    let ts = { K.n = n_type_args; K.n_cgs = List.length cg_args } in
    K.EQualified name, { ts; arg_types; ret_type; cg_types = cg_args; is_known_builtin = true }
  in
  let known_builtins =
    if !Options.no_const then
      known_builtins_no_const
    else
      known_builtins
  in
  match List.find_opt (fun (p, _) -> matches p) known_builtins with
  | Some (_, b) -> builtin b
  | None -> (
      let lookup_result_of_fun_id fun_id =
        let { C.item_meta; signature; _ } = env.get_nth_function fun_id in
        let lid = lid_of_name env item_meta.name in
        L.log "Calls" "%s--> name: %a" depth plid lid;
        K.EQualified lid, lookup_signature env depth signature
      in

      match f.kind with
      | FunId (FRegular f) -> lookup_result_of_fun_id f
      | FunId (FBuiltin f) -> fail "unknown builtin function: %s" (C.show_builtin_fun_id f)
      | TraitMethod (trait_ref, method_name, _trait_opaque_signature) -> (
          match trait_ref.kind with
          | TraitImpl { id; _ } ->
              let trait = env.get_nth_trait_impl id in
              let f =
                try List.assoc method_name trait.methods
                with Not_found ->
                  fail "Error looking trait impl: %s %s%!"
                    (Charon.PrintTypes.trait_ref_to_string env.format_env trait_ref)
                    method_name
              in
              lookup_result_of_fun_id f.C.binder_value.id
          | (Clause _ | ParentClause _) as tcid ->
              let f, t, sig_info = lookup_clause_method env tcid method_name in
              (* the sig_info is kind of redundant here *)
              let t =
                match t with
                | TPoly (_, t) -> t
                | _ -> t
              in
              let ret_type, arg_types = Krml.Helpers.flatten_arrow t in
              let cg_types, arg_types = Krml.KList.split sig_info.n_cgs arg_types in
              EBound f, { ts = sig_info; cg_types; arg_types; ret_type; is_known_builtin = false }
          | _ ->
              fail "Error looking trait ref: %s %s%!"
                (Charon.PrintTypes.trait_ref_to_string env.format_env trait_ref)
                method_name))

let fn_ptr_is_opaque env (fn_ptr : C.fn_ptr) =
  match fn_ptr.kind with
  | FunId (FRegular id) -> ( try (env.get_nth_function id).body = None with Not_found -> false)
  | _ -> false

(* This is a very core piece of logic that transforms a Rust fn_ptr into a krml AST node that
   contains type application, const generic applications, and application of trait methods to
   implement the dictionary-passing style. *)
let rec expression_of_fn_ptr env depth (fn_ptr : C.fn_ptr) =
  let {
    C.generics = { types = type_args; const_generics = const_generic_args; trait_refs; _ };
    kind;
    _;
  } : C.fn_ptr =
    fn_ptr
  in

  (* We handle any kind of fn_ptr, whether it's a concrete function call, a
     concrete trait implementation method call, or an abstract trait method call
     (e.g. a call to T::f when T is a trait bound in scope). *)
  L.log "Calls" "%sVisiting call: %s" depth
    (Charon.PrintTypes.fn_ptr_to_string env.format_env fn_ptr);
  L.log "Calls" "%s--> %d type_args, %d const_generics, %d trait_refs" depth (List.length type_args)
    (List.length const_generic_args) (List.length trait_refs);

  (* In case this is a call to a trait implementation method, there are two
     levels of applications: the trait-generic arguments (i.e. impl <T: ...> for ...)
     and the method-generic arguments (i.e. fn f<U: ...>). The method appears as
     a top-level function that receives all the arguments for T and U (types,
     const generics, trait references), and when we synthesize a call node, we
     do behave accordingly and provide arguments for both T and U. *)
  let type_args, const_generic_args, trait_refs =
    let generics =
      match kind with
      | TraitMethod ({ kind = TraitImpl { generics; _ }; _ }, _, _) ->
          L.log "Calls" "%s--> this is a trait method" depth;
          generics
      | _ -> C.empty_generic_args
    in
    ( generics.types @ type_args,
      generics.const_generics @ const_generic_args,
      generics.trait_refs @ trait_refs )
  in

  L.log "Calls" "%s--> %d type_args, %d const_generics, %d trait_refs" depth (List.length type_args)
    (List.length const_generic_args) (List.length trait_refs);
  L.log "Calls" "%s--> trait_refs: %s\n" depth
    (String.concat " ++ "
       (List.map (Charon.PrintTypes.trait_ref_to_string env.format_env) trait_refs));
  L.log "Calls" "%s--> pattern: %s" depth (string_of_fn_ptr env fn_ptr);
  L.log "Calls" "%s--> type_args: %s" depth
    (String.concat ", " (List.map (Charon.PrintTypes.ty_to_string env.format_env) type_args));

  (* The function itself, along with information about its *signature*. *)
  let f, { ts; arg_types = inputs; ret_type = output; cg_types = cg_inputs; is_known_builtin } =
    lookup_fun env depth fn_ptr
  in
  L.log "Calls" "%s--> %d inputs: %a" depth (List.length inputs) ptyps inputs;
  L.log "Calls" "%s--> is_known_builtin?: %b" depth is_known_builtin;

  (* Translate effective type and cg arguments. *)
  let const_generic_args =
    match f, type_args with
    | ( EQualified lid,
        [
          _;
          TRef
            ( _,
              TAdt
                { id = TBuiltin TArray; generics = { types = [ _ ]; const_generics = [ cg ]; _ } },
              _ );
          _;
        ] )
      when lid = Builtin.slice_to_ref_array.name ->
        (* Special case, we *do* need to retain the length, which would disappear if we simply did
           typ_of_ty (owing to array decay rules). *)
        [ expression_of_const_generic env cg ]
    | _ -> List.map (expression_of_const_generic env) const_generic_args
  in
  let type_args = List.map (typ_of_ty env) type_args in

  (* Handling trait implementations for generic trait bounds in the callee. We
     synthesize krml expressions that correspond to each one of the trait methods
     that the callee expects. Order matters here. *)
  let fn_ptrs : K.expr list =
    if is_known_builtin then
      (* If this is a known builtin implementation, we do not materialize trait methods, on the
         basis that this is likely something from the standard library that exercises more features
         that we can support, and that since we hand-write it, we don't need this level of precision
         anyhow. *)
      []
    else
      (* MUST have the same structure as mk_clause_binders_and_args *)
      let rec build_trait_ref_mapping depth (trait_refs : C.trait_ref list) =
        List.concat_map
          (fun (trait_ref : C.trait_ref) ->
            let name =
              string_of_name env
                (env.get_nth_trait_decl trait_ref.trait_decl_ref.binder_value.id).item_meta.name
            in
            L.log "Calls" "%s--> trait_ref %s: %s\n" depth name (C.show_trait_ref trait_ref);

            match trait_ref.kind with
            | _ when List.mem name blocklisted_trait_decls ->
                (* Trait not supported -- don't synthesize arguments *)
                []
            | TraitImpl { id = impl_id; generics = _generics } ->
                (* Call-site has resolved trait clauses into a concrete trait implementation. *)
                let trait_impl : C.trait_impl = env.get_nth_trait_impl impl_id in

                (* This must be in agreement, and in the same order as mk_clause_binders_and_args *)
                List.map
                  (fun ((_item_name, { C.id; generics }) : _ * C.global_decl_ref) ->
                    if
                      not
                        (generics.types = [] && generics.const_generics = []
                       && generics.trait_refs = [])
                    then
                      failwith "TODO: polymorphic globals";
                    let global = env.get_nth_global id in
                    K.with_type (typ_of_ty env global.ty)
                      (K.EQualified (lid_of_name env global.item_meta.name)))
                  trait_impl.consts
                @ List.map
                    (fun ((item_name, bound_fn) : _ * C.fun_decl_ref C.binder) ->
                      let fun_decl_id = bound_fn.C.binder_value.C.id in
                      let fn_ptr : C.fn_ptr =
                        {
                          kind = TraitMethod (trait_ref, item_name, fun_decl_id);
                          generics = Charon.TypesUtils.empty_generic_args;
                        }
                      in
                      let fn_ptr = fst3 (expression_of_fn_ptr env (depth ^ "  ") fn_ptr) in
                      fn_ptr)
                    trait_impl.methods
                @ build_trait_ref_mapping ("  " ^ depth)
                    (let subst =
                       Charon.Substitute.make_subst_from_generics trait_impl.generics _generics
                     in
                     (*_generics.trait_refs*)
                     List.map
                       (Charon.Substitute.st_substitute_visitor#visit_trait_ref subst)
                       trait_impl.implied_trait_refs)
            | Clause _ as clause_id ->
                (* Caller it itself polymorphic and refers to one of its own clauses to synthesize
                   the clause arguments at call-site. We must pass whatever is relevant for this
                   clause, *transitively* (this means all the reachable parents). *)
                let rec relevant = function
                  | C.ParentClause (tref', _) -> relevant tref'.kind
                  | clause_id' -> clause_id = clause_id'
                in
                List.rev
                  (Krml.KList.filter_mapi
                     (fun i (var, t) ->
                       match var with
                       | TraitClauseMethod { clause_id = clause_id'; _ }
                       | TraitClauseConstant { clause_id = clause_id'; _ }
                         when relevant clause_id' -> Some K.(with_type t (EBound i))
                       | _ -> None)
                     env.binders)
            | ParentClause (tref, clause_id) ->
                let decl_id = tref.trait_decl_ref.binder_value.id in
                let trait_decl = env.get_nth_trait_decl decl_id in
                let name = string_of_name env trait_decl.item_meta.name in
                let clause_id = C.TraitClauseId.to_int clause_id in
                let parent_clause = List.nth trait_decl.implied_clauses clause_id in
                let parent_clause_decl =
                  env.get_nth_trait_decl parent_clause.trait.binder_value.id
                in
                let parent_name = string_of_name env parent_clause_decl.item_meta.name in
                Krml.KPrint.bprintf "looking up parent clause #%d of decl=%s = %s\n" clause_id name
                  parent_name;
                if List.mem parent_name blocklisted_trait_decls then
                  []
                else
                  failwith
                    ("Don't know how to resolve trait_ref above (2): "
                    ^ Charon.PrintTypes.trait_ref_to_string env.format_env trait_ref)
            | _ ->
                failwith
                  ("Don't know how to resolve trait_ref above (2): "
                  ^ Charon.PrintTypes.trait_ref_to_string env.format_env trait_ref))
          trait_refs
      in
      build_trait_ref_mapping depth trait_refs
  in
  L.log "Calls" "%s--> trait method impls: %d" depth (List.length fn_ptrs);

  (* This needs to match what is done in the FunGroup case (i.e. when we extract
     a definition). There are two behaviors depending on whether the function is
     builtin or not. *)
  let inputs =
    if inputs = [] then
      [ K.TUnit ]
    else
      inputs
  in

  (* From here on, this is only krml logic, which is about building
     properly-annotated internal nodes that take care of instantiating generic
     type schemes, dealing with type applications, const genericss, etc.
     followed by the application of trait methods (in the special TApp node). *)
  let t_unapplied = maybe_ts ts (Krml.Helpers.fold_arrow (cg_inputs @ inputs) output) in
  let offset = List.length env.binders - List.length env.cg_binders in
  L.log "Calls" "%s--> t_unapplied: %a" depth ptyp t_unapplied;
  L.log "Calls" "%s--> inputs: %a" depth ptyps inputs;
  L.log "Calls" "%s--> const_generic_args: %a (offset: %d)" depth pexprs const_generic_args offset;
  L.log "Calls" "%s--> %d fn_ptrs: %a (offset: %d)" depth (List.length fn_ptrs)
    (fun k e ->
      List.iter
        (fun e ->
          pexpr k e;
          Buffer.add_string k ": ";
          ptyp k e.typ)
        e)
    fn_ptrs offset;

  let t_applied =
    match t_unapplied with
    | TPoly ({ n; n_cgs }, t) ->
        let ts =
          { K.n = n - List.length type_args; n_cgs = n_cgs - List.length const_generic_args }
        in
        if ts.n > 0 || ts.n_cgs > 0 then
          K.TPoly (ts, t)
        else
          t
    | t -> t
  in
  L.log "Calls" "%s--> t_applied (1): %a" depth ptyp t_applied;
  let t_applied =
    Krml.DeBruijn.(subst_tn type_args (subst_ctn offset const_generic_args t_applied))
  in
  L.log "Calls" "%s--> t_applied (2): %a" depth ptyp t_applied;
  let t_applied =
    match t_applied with
    | TPoly (ts, t) ->
        assert (fn_ptrs = []);
        let ret, args = Krml.Helpers.flatten_arrow t in
        let _, args = Krml.KList.split (List.length const_generic_args) args in
        K.TPoly (ts, Krml.Helpers.fold_arrow args ret)
    | t ->
        let ret, args = Krml.Helpers.flatten_arrow t in
        if List.length const_generic_args + List.length fn_ptrs > List.length args then
          L.log "Calls" "ERROR in %s" (Charon.PrintTypes.fn_ptr_to_string env.format_env fn_ptr);
        let _, args =
          Krml.KList.split (List.length const_generic_args + List.length fn_ptrs) args
        in
        Krml.Helpers.fold_arrow args ret
  in
  L.log "Calls" "%s--> t_applied: %a" depth ptyp t_applied;
  let hd =
    let hd = K.with_type t_unapplied f in
    if type_args <> [] || const_generic_args <> [] || fn_ptrs <> [] then
      K.with_type t_applied (K.ETApp (hd, const_generic_args, fn_ptrs, type_args))
    else
      hd
  in
  L.log "Calls" "%s--> hd: %a" depth pexpr hd;
  ( hd,
    is_known_builtin,
    match t_applied with
    | TPoly (ts, t) -> K.TPoly (ts, fst (Krml.Helpers.flatten_arrow t))
    | t -> fst (Krml.Helpers.flatten_arrow t) )

let expression_of_fn_ptr env (fn_ptr : C.fn_ptr) = expression_of_fn_ptr env "" fn_ptr

let global_is_const env id =
  match (env.get_nth_global id).global_kind with
  | NamedConst | AnonConst -> true
  | Static -> false

let expression_of_operand (env : env) (op : C.operand) : K.expr =
  match op with
  | Copy ({ kind = PlaceGlobal { id; _ }; _ } as p) when global_is_const env id ->
      (* No need to copy a const since by definition it cannot be modified *)
      expression_of_place env p
  | Copy p -> expression_of_place env p
  | Move p -> expression_of_place env p
  | Constant { kind = CLiteral l; _ } -> expression_of_literal env l
  | Constant { kind = CVar var; _ } -> expression_of_cg_var_id env (C.expect_free_var var)
  | Constant { kind = CFnPtr fn_ptr; _ } ->
      let e, _, _ = expression_of_fn_ptr env fn_ptr in
      e
  | Constant { kind = CTraitConst (({ C.kind; _ } as trait_ref), name); _ } -> begin
      (* Logic similar to lookup_fun *)
      match kind with
      | Clause _ | ParentClause _ ->
          let i, t = lookup_clause_constant env kind name in
          K.(with_type t (EBound i))
      | TraitImpl { id; _ } ->
          let trait = env.get_nth_trait_impl id in
          let global =
            try List.assoc name trait.consts
            with Not_found ->
              fail "Error looking trait impl: %s %s%!"
                (Charon.PrintTypes.trait_ref_to_string env.format_env trait_ref)
                name
          in
          let global = env.get_nth_global global.C.id in
          K.with_type (typ_of_ty env global.ty)
            (K.EQualified (lid_of_name env global.item_meta.name))
      | _ ->
          fail "expression_of_operand Constant: %s"
            (Charon.PrintExpressions.operand_to_string env.format_env op)
    end
  | Constant _ ->
      fail "expression_of_operand: %s" (Charon.PrintExpressions.operand_to_string env.format_env op)

let is_str env var_id =
  match lookup_with_original_type env var_id with
  | _, _, TRef (_, TAdt { id = TBuiltin TStr; generics = { types = []; _ } }, _) -> true
  | _ -> false

let is_box_place (p : C.place) =
  match p.ty with
  | C.TAdt { id = TBuiltin TBox; _ } -> true
  | _ -> false

(* returns either a regular naked C pointer, or a fat pointer in the case of DSTs (i.e. with non-empty metadata) *)
let mk_reference ~const (e : K.expr) (metadata : K.expr) : K.expr =
  match metadata.typ with
  (* When it is unit, it means there is no metadata, simply take the address *)
  | K.TUnit -> addrof ~const e
  | _ -> (
      match e.typ with
      | TApp (lid, [ t ]) when lid = Builtin.derefed_slice ->
          (* The special "base case" of DSTs: slice<T>
          where we have to cast [derefed_slice<T>] into [T*] for the .ptr field in the fat pointer  *)
          let ptr = K.(with_type (TBuf (t, const)) (ECast (e, TBuf (t, const)))) in
          K.(
            with_type
              (Builtin.mk_dst_ref ~const t metadata.typ)
              (EFlat [ Some "ptr", ptr; Some "meta", metadata ]))
      | _ ->
          K.(
            with_type
              (Builtin.mk_dst_ref ~const e.typ metadata.typ)
              (EFlat [ Some "ptr", addrof ~const e; Some "meta", metadata ])))

(* To destruct a DST reference type into its base and metadata types
   I.e., from Eurydice_dst_ref<T, meta> to (T, meta) *)
let destruct_dst_ref_typ t =
  match t with
  | K.TApp (dst_ref_hd, [ t_base; t_meta ]) when is_dst_ref dst_ref_hd -> Some (t_base, t_meta)
  | _ -> None

(* Get the base pointer expression from a DST reference expression
   I.e., from `e : Eurydice_dst_ref<T, meta>` to `e.ptr : T*` *)
let get_dst_ref_base dst_ref =
  match destruct_dst_ref_typ dst_ref.K.typ with
  (* XXX fixme *)
  | Some (base, _) -> Some K.(with_type (TBuf (base, false)) (EField (dst_ref, "ptr")))
  | None -> None

(* Parse the fat pointer Eurydice_dst_ref<T<U>, _> into (T,T<U>),
   where the ignored field `_` must be `usize` as metadata,
   used to handle the unsized cast from &T<V> to &T<U> *)
let destruct_arr_dst_ref t =
  match t with
  | K.TApp (dst_ref_hd, [ (TApp (lid, _) as t_u); _ ]) when is_dst_ref dst_ref_hd -> Some (lid, t_u)
  | _ -> None

(* Reborrows allow going from a mutable slice to an immutable one. *)
let maybe_reborrow_slice t_dst e_src =
  let open K in
  match e_src.typ, t_dst with
  | TApp (hd1, [ t_ptr; t_meta ]), TApp (hd2, _) when is_dst_ref hd1 && is_dst_ref hd2 && hd1 <> hd2
    ->
      let mk e =
        with_type t_dst
          (EFlat
             [
               Some "ptr", with_type (TBuf (t_ptr, true)) (EField (e, "ptr"));
               Some "meta", with_type t_meta (EField (e, "meta"));
             ])
      in
      if Krml.Helpers.is_readonly_c_expression e_src then
        mk e_src
      else
        with_type t_dst
          (ELet
             ( Krml.Helpers.fresh_binder "reborrowed_slice" e_src.typ,
               e_src,
               mk (with_type e_src.typ (EBound 0)) ))
  | _ -> e_src

let expression_of_rvalue (env : env) (p : C.rvalue) expected_ty : K.expr =
  match p with
  | Use op -> expression_of_operand env op
  (* Generally, MIR / current LLBC is guaranteed to apply [Deref] only to places that are
     references or raw pointers, in these cases [&*p] == [p].
     The [Deref] traits types are desugared to function calls to [deref].
     The ONLY exception is when the place is a [Box]. That is, MIR/LLBC might generate [*b]
     where [b] is a [Box]. This refers to taking the value out of the [Box].
     Recall that [Box] is a wrapper of [Unique], which is in turn a wrapper of a [NonNull],
     which is a wrapper of a raw pointer. Hence, [*b] when [b] is a [Box] is equivalent to
     [*(b.0.pointer.pointer)]. This is a compiler magic.

     However, in Eurydice *now*, [Box] types are instantly unboxed to raw pointers, which
     coincides exactly with our current implementation, hence no extra handling is needed.
     In the future however, we might want to handle [Box] types differently, so this is a note
     to ourselves to be careful with this.
     *)
  | RvRef ({ kind = PlaceProjection (p, Deref); _ }, _, _)
  | RawPtr ({ kind = PlaceProjection (p, Deref); _ }, _, _) ->
      (* Notably, this is NOT simply an optimisation, as this represents re-borrowing, and [p] might
         be a reference to DST (fat pointer). *)
      (* This also works for when the case has metadata, simply ignore it *)
      maybe_reborrow_slice (typ_of_ty env expected_ty) (expression_of_place env p)
  | RvRef (p, bk, metadata) ->
      let metadata = expression_of_operand env metadata in
      let e = expression_of_place env p in
      mk_reference ~const:(const_of_borrow_kind bk) e metadata
  | RawPtr (p, rk, metadata) ->
      let metadata = expression_of_operand env metadata in
      let e = expression_of_place env p in
      mk_reference ~const:(const_of_ref_kind rk) e metadata
  | NullaryOp (SizeOf, ty) ->
      let t = typ_of_ty env ty in
      K.(with_type TBool (EApp (Builtin.(expr_of_builtin_t sizeof [ t ]), [])))
  | NullaryOp (AlignOf, ty) ->
      let t = typ_of_ty env ty in
      K.(with_type TBool (EApp (Builtin.(expr_of_builtin_t alignof [ t ]), [])))
  | UnaryOp (Cast (CastScalar (_, dst)), e) ->
      let dst = typ_of_literal_ty env dst in
      K.with_type dst (K.ECast (expression_of_operand env e, dst))
  | UnaryOp (Cast (CastRawPtr (_from, to_)), e) ->
      let dst = typ_of_ty env to_ in
      K.with_type dst (K.ECast (expression_of_operand env e, dst))
  | UnaryOp (Cast (CastTransmute ((TRawPtr _ as _from), (TLiteral (TUInt Usize) as to_))), e) ->
      let dst = typ_of_ty env to_ in
      K.with_type dst (K.ECast (expression_of_operand env e, dst))
  | UnaryOp (Cast (CastFnPtr (TFnDef _from, TFnPtr _to)), e) ->
      (* From FnDef to FnPtr *)
      if Charon.Substitute.lookup_fndef_sig env.crate _from = Some _to then
        expression_of_operand env e
      else
        let dst = typ_of_ty env (TFnPtr _to) in
        K.with_type dst (K.ECast (expression_of_operand env e, dst))
  | UnaryOp (Cast (CastFnPtr (TFnPtr _, TFnPtr _)), e) ->
      (* possible safe fn to unsafe fn, same in C *)
      expression_of_operand env e
  | UnaryOp (Cast (CastUnsize (ty_from, ty_to, meta) as ck), e) ->
      (* DSTs: we support going from &T<S1> to &T<S2> where S1 is sized,  S2 is
         unsized and &T<S2> becomes a fat pointer. The base case is from &T<[U;N]>
         to T<[U]>. See test/more_dst.rs for user-defined DST case. We build this 
         coercion by hand, and slightly violate C's strict aliasing rules. *)
      let t_from = typ_of_ty env ty_from and t_to = typ_of_ty env ty_to in
      let e = expression_of_operand env e in
      begin
        match meta, t_from, destruct_arr_dst_ref t_to with
        | MetaLength cg, TBuf (TApp (lid1, _), const), Some (lid2, t_u) when lid1 = lid2 ->
            (* Cast from a struct whose last field is `t data[n]` to a struct whose last field is
             `Eurydice_derefed_slice data` (a.k.a. `char data[]`) *)
            let len = expression_of_const_generic env cg in
            let ptr = K.with_type (TBuf (t_u, const)) (K.ECast (e, TBuf (t_u, const))) in
            Builtin.dst_new ~const ~len ~ptr t_u
        | MetaLength cg, TBuf (K.TCgApp (K.TApp (lid_arr, [ t ]), _), _), _
          when lid_arr = Builtin.arr ->
            (* Cast from Box<[T;N]> (represented as a mut reference to an array) to Box<[T]> (which we
               represent as a slice). See the translation of types. *)
            let len = expression_of_const_generic env cg in
            let const =
              match t_to with
              | K.TApp (dst_ref_hd, _) when dst_ref_hd = Builtin.dst_ref_shared -> true
              | K.TApp (dst_ref_hd, _) when dst_ref_hd = Builtin.dst_ref_mut -> false
              | _ -> assert false
            in
            let array_to_slice =
              RustNames.builtin_of_function
                (if const then
                   Builtin.array_to_slice_func_shared
                 else
                   Builtin.array_to_slice_func_mut)
            in
            let t_without_cg = array_to_slice.typ in
            (* array_to_slice: size_t -> arr -> Eurydice_slice 0 *)
            let array_to_slice = Builtin.(expr_of_builtin array_to_slice) in
            let diff = List.length env.binders - List.length env.cg_binders in
            let array_to_slice =
              K.with_type
                Krml.DeBruijn.(subst_t t 0 (subst_ct diff len 0 t_without_cg))
                (K.ETApp (array_to_slice, [ len ], [], [ t ]))
            in
            K.(with_type (Builtin.mk_slice ~const t) (EApp (array_to_slice, [ e ])))
        | _, _, _ ->
            Krml.KPrint.bprintf "t_to = %a\n" ptyp t_to;
            Krml.KPrint.bprintf "destruct_arr_dst_ref t_to = None? %b\n"
              (destruct_arr_dst_ref t_to = None);
            Krml.Warn.fatal_error "unknown unsize cast: `%s`\nt_to=%a\nt_from=%a"
              (Charon.PrintExpressions.cast_kind_to_string env.format_env ck)
              ptyp t_to ptyp t_from
      end
  | UnaryOp (Cast (CastConcretize (_from_ty, to_ty)), e) -> (
      (* Concretization cast is a no-op at runtime *)
      let op_e = expression_of_operand env e in
      let typ = typ_of_ty env to_ty in
      match get_dst_ref_base op_e with
      | Some base_ptr -> K.(with_type typ (ECast (base_ptr, typ)))
      | None ->
          failwith
            ("unknown concretize cast: `"
            ^ Charon.PrintExpressions.cast_kind_to_string env.format_env
                (CastConcretize (_from_ty, to_ty))
            ^ "`"))
  | UnaryOp (Cast ck, e) ->
      (* Add a simpler case: identity cast is allowed *)
      let is_ident =
        match ck with
        (* Here are `literal_type`s *)
        | C.CastScalar (f, t) -> f = t
        (* The following are `type`s *)
        | C.CastFnPtr (f, t) | C.CastRawPtr (f, t) | C.CastUnsize (f, t, _) | C.CastTransmute (f, t)
          -> f = t
        | C.CastConcretize _ -> false
      in
      if is_ident then
        expression_of_operand env e
      else
        failwith
          ("unknown cast: `" ^ Charon.PrintExpressions.cast_kind_to_string env.format_env ck ^ "`")
  (* | UnaryOp (PtrMetadata, e) ->
    let e = expression_of_operand env e in begin
    match e.typ with
    | TApp (lid, [ _; meta_ty ]) when lid = Builtin.dst_ref ->
      K.(with_type meta_ty (EField (e, "meta")))
    (* In cases like `PtrMetadata(T)` when `T` is a type variable or some types with unresolved type variable,
       We cannot tell the correct metadata type from it until fully monomorphized.
       But we can surely rely on monomorphized LLBC, and we ignore handling such cases in Eurydice. *)
    | ty when has_unresolved_generic ty ->
      failwith "Eurydice do not handle ptr-metadata for generic types. Consider using monomorphized LLBC."
    (* Otherwise, fetching ptr-metadata from a non-DST simply results in `()`
       When a type is fully resolved and it is not `Eurydice::DstRef`, we can be confident that it is not a DST. *)
    | _ -> K.with_type TUnit K.EUnit
    end *)
  | UnaryOp (op, o1) -> mk_op_app (op_of_unop op) (expression_of_operand env o1) []
  | BinaryOp (op, o1, o2) ->
      mk_op_app (op_of_binop op) (expression_of_operand env o1) [ expression_of_operand env o2 ]
  | Discriminant sub_p ->
      let e = expression_of_place env sub_p in
      let expected_t = typ_of_ty env expected_ty in
      K.(
        with_type expected_t
          (EApp (Builtin.(expr_of_builtin_t discriminant) [ e.typ; expected_t ], [ e ])))
  | Aggregate (AggregatedAdt ({ id = TTuple; _ }, _, None), ops) -> begin
      match ops with
      | [] -> K.with_type TUnit K.EUnit
      | [ op ] -> expression_of_operand env op
      | _ ->
          let ops = List.map (expression_of_operand env) ops in
          let ts = List.map (fun x -> x.K.typ) ops in
          K.with_type (TTuple ts) (K.ETuple ops)
    end
  | Aggregate
      ( AggregatedAdt
          ( { id = TAdtId typ_id; generics = { types = typ_args; const_generics; _ } },
            variant_id,
            None ),
        args ) ->
      let { C.item_meta; kind; _ } = env.get_nth_type typ_id in
      let name = item_meta.name in
      let typ_lid = lid_of_name env name in
      let typ_args = List.map (typ_of_ty env) typ_args in
      let cg_args = List.map (cg_of_const_generic env) const_generics in
      let t = K.fold_tapp (typ_lid, typ_args, cg_args) in
      let args = List.map (expression_of_operand env) args in
      begin
        match variant_id with
        | Some variant_id ->
            let variant_id = (find_nth_variant env typ_id variant_id).variant_name in
            if is_enum env typ_id then
              K.with_type t (K.EEnum (mk_enum_case typ_lid variant_id))
            else
              K.with_type t (K.ECons (variant_id, args))
        | None ->
            let fields =
              match kind with
              | Struct fields -> fields
              | Enum _ -> failwith "TODO: Enum"
              | Union _ -> failwith "TODO: Union"
              | Opaque -> failwith "TODO: Opaque"
              | Alias _ -> failwith "TODO: Alias"
              | TDeclError _ -> failwith "TODO: TDeclError"
            in
            K.with_type t
              (K.EFlat
                 (List.mapi
                    (fun i (f, a) -> Some (ensure_named i f.C.field_name), a)
                    (List.combine fields args)))
      end
  | Aggregate (AggregatedAdt ({ id = TBuiltin _; _ }, _, _), _) ->
      failwith "unsupported: AggregatedAdt / TAssume"
  | Aggregate (AggregatedArray (t, cg), ops) ->
      let ty = typ_of_ty env t in
      let typ_arr = typ_of_struct_arr env t cg in
      begin
        match ops with
        | [] ->
            let empty_array =
              K.with_type
                Krml.DeBruijn.(subst_t ty 0 Builtin.empty_array.typ)
                (K.ETApp (Builtin.(expr_of_builtin empty_array), [], [], [ ty ]))
            in
            K.with_type typ_arr (K.EApp (empty_array, [ K.with_type ty K.EAny ]))
            (* a dummy arg is needed to pass the checker *)
        | _ ->
            let array_expr =
              K.with_type
                (TArray (typ_of_ty env t, constant_of_scalar_value (assert_cg_scalar cg)))
                (K.EBufCreateL (Stack, List.map (expression_of_operand env) ops))
            in
            K.with_type typ_arr (mk_expr_arr_struct array_expr)
      end
  | rvalue ->
      failwith
        ("unsupported rvalue: `"
        ^ Charon.PrintExpressions.rvalue_to_string env.format_env rvalue
        ^ "`")

let expression_of_assertion (env : env) ({ cond; expected; _ } : C.assertion) : K.expr =
  let cond =
    if not expected then
      expression_of_operand env cond
    else
      Krml.Helpers.mk_not (expression_of_operand env cond)
  in
  K.(
    with_type TAny
      (EIfThenElse (cond, with_type TAny (EAbort (None, Some "assert failure")), Krml.Helpers.eunit)))

let lesser t1 t2 =
  if t1 = K.TAny then
    t2
  else if t2 = K.TAny then
    t2
  else if t1 <> t2 then
    fail "lesser t1=%a t2=%a" ptyp t1 ptyp t2
  else
    t1

(* A `fn` pointer, which does not have trait bounds, and cannot be partially applied. This is a
   much simplified version of expression_of_fn_ptr. *)
let expression_of_fn_op_move (env : env) ({ func; args; dest } : C.call) =
  let fHd =
    match func with
    | C.FnOpMove place -> expression_of_place env place
    | _otw ->
        failwith @@ "Internal error: the given call is not to `FnOpMove`."
        ^ "The function `expression_of_fn_op_move` handles only call to `FnOpMove`."
  in
  let lhs = expression_of_place env dest in
  let args = List.map (expression_of_operand env) args in
  let args =
    if args = [] then
      [ Krml.Helpers.eunit ]
    else
      args
  in
  (* Asserting that this is not a partial application *)
  let ret_t, args_t = Krml.Helpers.flatten_arrow fHd.typ in
  assert (List.length args_t = List.length args);
  let rhs = K.with_type ret_t @@ K.EApp (fHd, args) in
  Krml.Helpers.with_unit @@ K.EAssign (lhs, rhs)

(** Handles only the `SwitchInt` for 128-bit integers. Turn the switch expression into if-then-else
    expressions. This is to work around the Krml integer type limitations. *)
let rec expression_of_switch_128bits env ret_var scrutinee branches default : K.expr =
  let scrutinee = expression_of_operand env scrutinee in
  let else_branch = expression_of_block env ret_var default in
  let folder (svs, stmt) else_branch =
    (* [i1, i2, ..., in] ==> scrutinee == i1 || scrutinee == i2 || ... || scrutinee == in *)
    let guard =
      let make_eq sv = mk_op_app Eq scrutinee [ expression_of_scalar_value sv ] in
      List.map make_eq svs |> function
      | [] -> Krml.Helpers.etrue
      | x :: lst -> List.fold_left Krml.Helpers.mk_or x lst
    in
    (* the "then" body of the if-then-else expression *)
    let body = expression_of_block env ret_var stmt in
    (* combines the types: compare each branch and then generate the correct type *)
    let typ = lesser body.K.typ else_branch.K.typ in
    K.(with_type typ (EIfThenElse (guard, body, else_branch)))
  in
  List.fold_right folder branches else_branch

and expression_of_statement_kind (env : env) (ret_var : C.local_id) (s : C.statement_kind) : K.expr
    =
  match s with
  | Assign (p, rv) ->
      let expected_ty = p.ty in
      let p = expression_of_place env p in
      let rv = expression_of_rvalue env rv expected_ty in
      K.(with_type TUnit (EAssign (p, rv)))
  | SetDiscriminant (_, _) -> failwith "C.SetDiscriminant"
  | StorageLive _ -> Krml.Helpers.eunit
  | StorageDead _ -> Krml.Helpers.eunit
  | Deinit p | Drop (p, _) ->
      let _ = expression_of_place env p in
      begin
        match p.ty with
        (* doesn't do the right thing yet, need to understand why there are
           several drops per variable *)
        (* | C.Adt (Builtin Vec, _) when false -> *)
        (*     (1* p is a vec t *1) *)
        (*     let t = match p.typ with TApp ((["Eurydice"], "vec"), [ t ]) -> t | _ -> assert false in *)
        (*     Krml.Helpers.(with_unit K.(EApp ( *)
        (*       with_type (Krml.DeBruijn.subst_tn [ t ] Builtin.vec_drop.typ) (ETApp ( *)
        (*         with_type Builtin.vec_drop.typ (EQualified Builtin.vec_drop.name), *)
        (*         [ t ])), *)
        (*       [ p ]))) *)
        | _ -> Krml.Helpers.eunit
      end
  | Assert a -> expression_of_assertion env a
  | Call
      {
        func =
          FnOpRegular
            {
              kind = FunId (FBuiltin ArrayRepeat);
              generics = { types = [ ty ]; const_generics = [ c ]; _ };
              _;
            };
        args = [ e ];
        dest;
        _;
      } ->
      (* Special treatment *)
      let e = expression_of_operand env e in
      let t = typ_of_ty env ty in
      let t_array = maybe_cg_array env ty c in
      let len = expression_of_const_generic env c in
      let dest = expression_of_place env dest in
      let repeat =
        K.(
          with_type
            (Krml.Helpers.fold_arrow Builtin.array_repeat.cg_args Builtin.array_repeat.typ)
            (EQualified Builtin.array_repeat.name))
      in
      let diff = List.length env.binders - List.length env.cg_binders in
      let repeat =
        K.(
          with_type
            Krml.DeBruijn.(subst_t t 0 (subst_ct diff len 0 Builtin.array_repeat.typ))
            (ETApp (repeat, [ len ], [], [ t ])))
      in
      Krml.Helpers.with_unit
        K.(
          EAssign
            ( dest,
              with_type dest.typ (mk_expr_arr_struct (with_type t_array (EApp (repeat, [ e ])))) ))
  | Call
      {
        func =
          FnOpRegular
            {
              kind = FunId (FBuiltin (Index { is_array = true; mutability = _; is_range = false }));
              generics = { types = [ ty ]; const_generics = [ cg ]; _ };
              _;
            };
        args = [ e1; e2 ];
        dest;
        _;
      } ->
      (* Special treatment for e1[e2] of array which are translated into struct.
         e1[e2] is translated as fn ArrayIndexShared<T,N>(&[T;N], usize) -> &T

         Since [T;N] is translated into arr$T$N, we need to first dereference
         the e1 to get the struct, and then take its field "data" to get the
         array
          
         We construct dest := &( *e1).data[e2]
         *)
      let e1 = expression_of_operand env e1 in
      let e2 = expression_of_operand env e2 in
      let t = typ_of_ty env ty in
      let t_array = maybe_cg_array env ty cg in
      (* let const = const_of_tbuf e1.K.typ in *)
      let e1 = Krml.Helpers.(mk_deref ~const:true (Krml.Helpers.assert_tbuf e1.K.typ) e1.K.node) in
      let e1 = K.with_type t_array (K.EField (e1, "data")) in
      let dest = expression_of_place env dest in
      Krml.Helpers.with_unit
        K.(EAssign (dest, addrof ~const:false (with_type t (EBufRead (e1, e2)))))
  | Call { func = FnOpRegular fn_ptr; args; dest; _ }
    when Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.from_u16 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.from_u32 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.from_u64 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.from_i16 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.from_i32 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.from_i64 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.into_u16 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.into_u32 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.into_u64 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.into_i16 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.into_i32 fn_ptr
         || Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.into_i64 fn_ptr
         || false ->
      (* TODO: this can now be properly represented in the AST, this should go
         away! there is *one* case in Kyber that is not caught by
         Cleanup2.remove_trivial_into, and we need to figure out why. *)
      let matches p = Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config p fn_ptr in
      let w : Krml.Constant.width =
        if matches RustNames.from_u16 || matches RustNames.into_u16 then
          UInt16
        else if matches RustNames.from_u32 || matches RustNames.into_u32 then
          UInt32
        else if matches RustNames.from_u64 || matches RustNames.into_u64 then
          UInt64
        else if matches RustNames.from_i16 || matches RustNames.into_i16 then
          Int16
        else if matches RustNames.from_i32 || matches RustNames.into_i32 then
          Int32
        else if matches RustNames.from_i64 || matches RustNames.into_i64 then
          Int64
        else
          fail "Unknown from-cast: %s" (string_of_fn_ptr env fn_ptr)
      in
      let dest = expression_of_place env dest in
      let e = expression_of_operand env (Krml.KList.one args) in
      Krml.Helpers.with_unit K.(EAssign (dest, with_type (TInt w) (ECast (e, TInt w))))
  | Call { func = FnOpRegular fn_ptr; args; dest; _ } ->
      (* For now, we take trait type arguments to be part of the code-gen *)
      let hd, _is_known_builtin, output_t = expression_of_fn_ptr env fn_ptr in
      let dest = expression_of_place env dest in
      let args = List.map (expression_of_operand env) args in
      (* This needs to match what is done in the FunGroup case (i.e. when we extract
         a definition). There are two behaviors depending on whether the function is
         builtin or not. *)
      (* Krml.KPrint.bprintf "Call to %s is builtin %b\n" (string_of_fn_ptr env fn_ptr) is_builtin; *)
      let args =
        if args = [] then
          [ Krml.Helpers.eunit ]
        else
          args
      in
      let rhs =
        if args = [] then
          hd
        else
          K.with_type output_t (K.EApp (hd, args))
      in
      (* This does something similar to maybe_addrof *)
      let rhs =
        (* TODO: determine whether extra_types is necessary *)
        let extra_types =
          match fn_ptr.kind with
          | TraitMethod ({ kind = TraitImpl { id = _; generics }; _ }, _, _) -> generics.types
          | _ -> []
        in
        match fn_ptr.kind, fn_ptr.generics.types @ extra_types with
        | ( FunId (FBuiltin (Index { is_array = false; mutability = _; is_range = false })),
            [ TAdt { id = TBuiltin TSlice; _ } ] ) ->
            (* Will decay. See comment above maybe_addrof *)
            rhs
        | ( FunId (FBuiltin (Index { is_array = false; mutability = _; is_range = false })),
            [ TAdt { id; generics } ] )
          when RustNames.is_vec env id generics ->
            (* Will decay. See comment above maybe_addrof *)
            rhs
        | FunId (FBuiltin (Index { is_array = false; mutability = _; is_range = false })), _ ->
            K.(with_type (TBuf (rhs.typ, false)) (EAddrOf rhs))
        | _ -> rhs
      in
      Krml.Helpers.with_unit K.(EAssign (dest, rhs))
  | Call ({ func = FnOpMove _; _ } as call) -> expression_of_fn_op_move env call
  | Abort _ -> with_any (K.EAbort (None, Some "panic!"))
  | Return ->
      let e = expression_of_var_id env ret_var in
      K.(with_type TAny (EReturn e))
  | Break _ -> K.(with_type TAny EBreak)
  | Continue _ -> K.(with_type TAny EContinue)
  | Nop -> Krml.Helpers.eunit
  | Switch (If (op, s1, s2)) ->
      let e1 = expression_of_block env ret_var s1 in
      let e2 = expression_of_block env ret_var s2 in
      let t = lesser e1.typ e2.typ in
      K.(with_type t (EIfThenElse (expression_of_operand env op, e1, e2)))
  | Switch (SwitchInt (scrutinee, int_ty, branches, default)) ->
      let branches =
        List.map
          (fun (litl, block) -> List.map Charon.ValuesUtils.literal_as_scalar litl, block)
          branches
      in
      if int_ty = TInt I128 || int_ty = TUInt U128 then
        expression_of_switch_128bits env ret_var scrutinee branches default
      else
        let scrutinee = expression_of_operand env scrutinee in
        let branches =
          List.concat_map
            (fun (svs, stmt) ->
              List.map
                (fun sv ->
                  K.SConstant (constant_of_scalar_value sv), expression_of_block env ret_var stmt)
                svs)
            branches
          @ [ K.SWild, expression_of_block env ret_var default ]
        in
        let t = Krml.KList.reduce lesser (List.map (fun (_, e) -> e.K.typ) branches) in
        K.(with_type t (ESwitch (scrutinee, branches)))
  | Switch (Match (p, branches, default)) ->
      let scrutinee = expression_of_place env p in
      let typ_id, typ_lid, variant_name_of_variant_id =
        match p.ty with
        | TAdt { id = TAdtId typ_id; _ } ->
            let ty = env.get_nth_type typ_id in
            let variants =
              match ty.kind with
              | Enum variants -> variants
              | _ -> assert false
            in
            ( typ_id,
              lid_of_name env ty.item_meta.name,
              fun v ->
                let v = C.VariantId.nth variants v in
                v.variant_name, List.length v.fields )
        | _ -> failwith "TODO: match on not adt, not option"
      in

      let branches =
        List.concat_map
          (fun (variant_ids, branch) ->
            List.map
              (fun variant_id ->
                let variant_name, n_fields = variant_name_of_variant_id variant_id in
                let dummies = List.init n_fields (fun _ -> K.(with_type TAny PWild)) in
                let pat =
                  if is_enum env typ_id then
                    K.PEnum (mk_enum_case typ_lid variant_name)
                  else
                    K.PCons (variant_name, dummies)
                in
                let pat = K.with_type scrutinee.typ pat in
                [], pat, expression_of_block env ret_var branch)
              variant_ids)
          branches
      in
      let branches =
        branches
        @
        match default with
        | Some default ->
            [ [], K.with_type scrutinee.typ K.PWild, expression_of_block env ret_var default ]
        | None -> []
      in
      let t = Krml.KList.reduce lesser (List.map (fun (_, _, e) -> e.K.typ) branches) in
      K.(with_type t (EMatch (Unchecked, scrutinee, branches)))
  | Loop s -> K.(with_type TUnit (EWhile (Krml.Helpers.etrue, expression_of_block env ret_var s)))
  | _ ->
      failwith
        ("Unsupported statement: "
        ^ Charon.PrintLlbcAst.Ast.statement_kind_to_string env.format_env "" "" s)

and expression_of_statement (env : env) (ret_var : C.local_id) (s : C.statement) : K.expr =
  {
    (expression_of_statement_kind env ret_var s.kind) with
    meta =
      (if !Options.comments then
         List.map (fun x -> K.CommentBefore x) s.comments_before
       else
         []);
  }

and expression_of_block (env : env) (ret_var : C.local_id) (b : C.block) : K.expr =
  let statements = List.map (expression_of_statement env ret_var) b.statements in
  match List.rev statements with
  | [] -> Krml.Helpers.eunit
  | last :: _ -> K.(with_type last.typ (ESequence statements))

(** Top-level declarations: orchestration *)

let of_declaration_group (dg : 'id C.g_declaration_group) (f : 'id -> 'a) : 'a list =
  (* We do not care about recursion as in C, everything is mutually recursive
     thanks to header inclusion. *)
  match dg with
  | NonRecGroup id -> [ f id ]
  | RecGroup ids -> List.map f ids

let seen = ref 0
let total = ref 0

(** List all the ids in this declaration group. *)
let declaration_group_to_list (g : C.declaration_group) : C.item_id list =
  match g with
  | FunGroup g -> List.map (fun id -> C.IdFun id) (C.g_declaration_group_to_list g)
  | TypeGroup g -> List.map (fun id -> C.IdType id) (C.g_declaration_group_to_list g)
  | GlobalGroup g -> List.map (fun id -> C.IdGlobal id) (C.g_declaration_group_to_list g)
  | TraitDeclGroup g -> List.map (fun id -> C.IdTraitDecl id) (C.g_declaration_group_to_list g)
  | TraitImplGroup g -> List.map (fun id -> C.IdTraitImpl id) (C.g_declaration_group_to_list g)
  | MixedGroup g -> C.g_declaration_group_to_list g

let flags_of_meta (meta : C.item_meta) : K.flags =
  [
    Krml.Common.Comment
      (String.concat "\n"
         (List.filter_map
            (function
              | Charon.Meta.AttrDocComment s -> Some s
              | _ -> None)
            meta.attr_info.attributes));
  ]

let decl_of_id (env : env) (id : C.item_id) : K.decl option =
  match id with
  | IdType id -> begin
      let decl = env.get_nth_type id in
      let { C.item_meta; def_id; kind; generics = { types = type_params; const_generics; _ }; _ } =
        decl
      in
      let name = item_meta.name in
      L.log "AstOfLlbc" "Visiting type: %s\n%s" (string_of_name env name)
        (Charon.PrintTypes.type_decl_to_string env.format_env decl);

      assert (def_id = id);
      let name = lid_of_name env name in
      let env = push_cg_binders env const_generics in
      let env = push_type_binders env type_params in

      match kind with
      | Union _ | Opaque | TDeclError _ -> None
      | Struct fields ->
          let fields =
            List.mapi
              (fun i { C.field_name; field_ty; _ } ->
                Some (ensure_named i field_name), (typ_of_ty env field_ty, true))
              fields
          in
          Some
            (K.DType (name, [], List.length const_generics, List.length type_params, Flat fields))
      | Enum branches when List.for_all (fun v -> v.C.fields = []) branches ->
          let has_custom_constants =
            let rec has_custom_constants i = function
              | { C.discriminant; _ } :: bs ->
                  Charon.Scalars.get_val (Charon.ValuesUtils.literal_as_scalar discriminant)
                  <> Z.of_int i
                  || has_custom_constants (i + 1) bs
              | _ -> false
            in
            has_custom_constants 0 branches
          in

          let cases =
            List.map
              (fun ({ C.variant_name; discriminant; _ } : C.variant) ->
                let v =
                  if has_custom_constants then
                    Some
                      (Charon.Scalars.get_val (Charon.ValuesUtils.literal_as_scalar discriminant))
                  else
                    None
                in
                mk_enum_case name variant_name, v)
              branches
          in
          Some (K.DType (name, [], List.length const_generics, List.length type_params, Enum cases))
      | Enum branches ->
          let branches =
            List.map
              (fun ({ C.variant_name; fields; _ } : C.variant) ->
                ( variant_name,
                  List.mapi
                    (fun i { C.field_name; field_ty; _ } ->
                      mk_field_name field_name i, (typ_of_ty env field_ty, true))
                    fields ))
              branches
          in
          Some
            (K.DType
               (name, [], List.length const_generics, List.length type_params, Variant branches))
      | Alias ty ->
          Some
            (K.DType
               ( name,
                 [],
                 List.length const_generics,
                 List.length type_params,
                 Abbrev (typ_of_ty env ty) ))
    end
  | IdFun id -> (
      let decl = try Some (env.get_nth_function id) with Not_found -> None in
      match decl with
      | None -> None
      | Some decl -> (
          let { C.def_id; signature; body; item_meta; src; _ } = decl in
          let env = { env with generic_params = signature.generics } in
          L.log "AstOfLlbc" "Visiting %sfunction: %s\n%s"
            (if body = None then
               "opaque "
             else
               "")
            (string_of_name env item_meta.name)
            (Charon.PrintLlbcAst.Ast.fun_decl_to_string env.format_env "  " "  " decl);

          assert (def_id = id);
          let name = lid_of_name env item_meta.name in
          match body, src with
          | _, TraitDeclItem (_, _, false) ->
              (* We skip those on the basis that they generate useless external prototypes, which we
                 do not really need. *)
              None
          | None, _ ->
              (* Opaque function *)
              let { K.n_cgs; n }, t = typ_of_signature env signature in
              Some (K.DExternal (None, [], n_cgs, n, name, t, []))
          | Some { locals; body; _ }, _ ->
              if Option.is_some decl.is_global_initializer then
                None
              else
                let env = push_cg_binders env signature.C.generics.const_generics in
                let env = push_type_binders env signature.C.generics.types in

                L.log "AstOfLlbc" "ty of locals: %s"
                  (String.concat " ++ "
                     (List.map
                        (fun (local : C.local) ->
                          Charon.PrintTypes.ty_to_string env.format_env local.local_ty)
                        locals.locals));
                L.log "AstOfLlbc" "ty of inputs: %s"
                  (String.concat " ++ "
                     (List.map
                        (fun t -> Charon.PrintTypes.ty_to_string env.format_env t)
                        signature.C.inputs));

                (* `locals` contains, in order: special return variable; function arguments;
                   local variables *)
                let args, locals = Krml.KList.split (locals.arg_count + 1) locals.locals in
                let return_var = List.hd args in
                let args = List.tl args in

                let return_type = typ_of_ty env return_var.local_ty in

                (* Note: Rust allows zero-argument functions but the krml internal
                   representation wants a unit there. This is aligned with typ_of_signature. *)
                let args =
                  let t_unit =
                    C.(
                      TAdt
                        {
                          id = TTuple;
                          generics =
                            { types = []; const_generics = []; regions = []; trait_refs = [] };
                        })
                  in
                  let v_unit =
                    {
                      C.index = Charon.Expressions.LocalId.of_int max_int;
                      name = None;
                      local_ty = t_unit;
                    }
                  in
                  if args = [] then
                    [ v_unit ]
                  else
                    args
                in

                (* At this stage, env has:
                   cg_binders = <<all cg binders>>
                   type_binders = <<all type binders>>
                   binders = <<all cg binders>>
                *)
                let clause_binders =
                  mk_clause_binders_and_args env signature.C.generics.trait_clauses
                in
                debug_trait_clause_mapping env clause_binders;
                (* Now we turn it into:
                   binders = <<all cg binders>> ++ <<all clause binders>> ++ <<regular function args>>
                *)
                let env = push_clause_binders env clause_binders in
                let env = push_binders env args in

                let arg_binders =
                  List.map
                    (fun (arg : C.const_generic_param) ->
                      Krml.Helpers.fresh_binder ~mut:true arg.name (typ_of_literal_ty env arg.ty))
                    signature.C.generics.const_generics
                  @ List.map
                      (function
                        | TraitClauseMethod { pretty_name; _ }, t
                        | TraitClauseConstant { pretty_name; _ }, t ->
                            Krml.Helpers.fresh_binder pretty_name t
                        | _ -> assert false)
                      clause_binders
                  @ List.map
                      (fun (arg : C.local) ->
                        let name = Option.value ~default:"_" arg.name in
                        Krml.Helpers.fresh_binder ~mut:true name (typ_of_ty env arg.local_ty))
                      args
                in

                L.log "AstOfLlbc" "type of binders: %a" ptyps
                  (List.map (fun o -> o.K.typ) arg_binders);
                let body =
                  with_locals env return_type (return_var :: locals) (fun env ->
                      expression_of_block env return_var.index body)
                in
                let flags =
                  match item_meta.attr_info.inline with
                  | Some Hint -> [ Krml.Common.Inline ]
                  | Some Always -> [ Krml.Common.MustInline ]
                  | Some Never -> [ Krml.Common.NoInline ]
                  | _ -> []
                in
                (* This is kind of a hack here: we indicate that this function is intended to be
                   specialized, at monomorphization-time (which happens quite early on), on the cg
                   binders but also on the clause binders... This is ok because even though the
                   clause binders are not in env.cg_binders, well, types don't refer to clause
                   binders, so we won't have translation errors. *)
                let n_cg = List.length signature.C.generics.const_generics in
                let n = List.length signature.C.generics.types in
                Some
                  (K.DFunction
                     ( None,
                       flags @ flags_of_meta item_meta,
                       n_cg,
                       n,
                       return_type,
                       name,
                       arg_binders,
                       body ))))
  | IdGlobal id ->
      let global = env.get_nth_global id in
      let { C.item_meta; ty; def_id; _ } = global in
      let name = item_meta.name in
      let def = env.get_nth_global def_id in
      L.log "AstOfLlbc" "Visiting global: %s\n%s" (string_of_name env name)
        (Charon.PrintLlbcAst.Ast.global_decl_to_string env.format_env "  " "  " def);
      let ty = typ_of_ty env ty in
      let flags =
        [ Krml.Common.Const "" ]
        @
        match global.global_kind with
        | NamedConst | AnonConst ->
            (* This is trickier: const can be evaluated at compile-time, so in theory, we could just
               emit a macro, except (!) in C, arrays need to be top-level declarations (not macros)
               because even with compound literals, you can't do `((int[1]){0})[0]` in expression
               position.

               We can't use the test "is_bufcreate" because the expression might only be a bufcreate
               *after* simplification, so we rely on the type here. *)
            if Krml.Helpers.is_array ty then
              []
            else
              [ Macro ]
        | Static ->
            (* This one needs to have an address, so definitely not emitting it as a macro. *)
            []
      in
      let body = env.get_nth_function def.init in
      L.log "AstOfLlbc" "Corresponding body:%s"
        (Charon.PrintLlbcAst.Ast.fun_decl_to_string env.format_env "  " "  " body);
      begin
        match body.body with
        | Some body ->
            let ret_var = List.hd body.locals.locals in
            let body =
              with_locals env ty body.locals.locals (fun env ->
                  expression_of_block env ret_var.index body.body)
            in
            Some (K.DGlobal (flags, lid_of_name env name, 0, ty, body))
        | None -> Some (K.DExternal (None, [], 0, 0, lid_of_name env name, ty, []))
      end
  | IdTraitDecl _ | IdTraitImpl _ -> None

let name_of_id env (id : C.item_id) =
  match id with
  | IdType id -> (env.get_nth_type id).item_meta.name
  | IdFun id -> (env.get_nth_function id).item_meta.name
  | IdGlobal id -> (env.get_nth_global id).item_meta.name
  | _ -> failwith "unsupported"

let known_failures =
  List.map Charon.NameMatcher.parse_pattern
    [
      (* Failure("TODO: TraitTypes Self::Output") *)
      "core::array::{core::ops::index::Index<[@T; @N], @I, @C>}::index";
      (* Failure("TODO: TraitTypes parent(Self)::TraitClause@0::Output") *)
      "core::array::{core::ops::index::IndexMut<[@T; @N], @I, @C>}::index_mut";
      (* Failure("TODO: TraitTypes core::marker::DiscriminantKind<T@0>::Discriminant") *)
      "core::intrinsics::discriminant_value";
      (* Failure("TODO: TraitTypes Self::Output") *)
      "core::slice::index::{core::ops::index::Index<[@T], @I, @C>}::index";
      (* Failure("TODO: TraitTypes Self::Output") *)
      "core::slice::index::{core::ops::index::IndexMut<[@T], @I, @C>}::index_mut";
      (* File "lib/AstOfLlbc.ml", line 389, characters 6-12: Assertion failed *)
      "core::slice::index::{core::slice::index::SliceIndex<core::ops::range::Range<usize>, [@T], \
       [@T]>}::get_unchecked";
      (* File "lib/AstOfLlbc.ml", line 389, characters 6-12: Assertion failed *)
      "core::slice::index::{core::slice::index::SliceIndex<core::ops::range::Range<usize>, [@T], \
       [@T]>}::get_unchecked_mut";
      (* File "lib/AstOfLlbc.ml", line 389, characters 6-12: Assertion failed *)
      "core::slice::index::{core::slice::index::SliceIndex<core::ops::range::RangeFrom<usize>, \
       [@T], [@T]>}::get_unchecked";
      (* File "lib/AstOfLlbc.ml", line 389, characters 6-12: Assertion failed *)
      "core::slice::index::{core::slice::index::SliceIndex<core::ops::range::RangeFrom<usize>, \
       [@T], [@T]>}::get_unchecked_mut";
      (* File "lib/AstOfLlbc.ml", line 389, characters 6-12: Assertion failed *)
      "core::slice::index::{core::slice::index::SliceIndex<core::ops::range::RangeTo<usize>, [@T], \
       [@T]>}::get_unchecked";
      (* File "lib/AstOfLlbc.ml", line 389, characters 6-12: Assertion failed *)
      "core::slice::index::{core::slice::index::SliceIndex<core::ops::range::RangeTo<usize>, [@T], \
       [@T]>}::get_unchecked_mut";
      (* Failure("TODO: TraitTypes core::marker::DiscriminantKind<T@0>::Discriminant") *)
      "issue_123::{core::cmp::PartialEq<issue_123::E2, issue_123::E2>}::eq";
      (* Failure("Can't handle arbitrary closures") *)
      "mismatch::{mismatch::MlKemKeyPairUnpacked<@Vector, @K>}::default";
    ]

let replacements =
  List.map
    (fun (p, d) -> Charon.NameMatcher.parse_pattern p, d)
    [
      "core::result::{core::result::Result<@T, @E>}::unwrap", Builtin.unwrap;
      "core::slice::{[@T]}::swap", Builtin.slice_swap;
      "alloc::vec::{alloc::vec::Vec<@T>}::try_with_capacity", Builtin.try_with_capacity;
      "core::ptr::null_mut", Builtin.null_mut;
    ]

(* Catch-all error handler (last resort) *)
let decl_of_id env decl =
  match
    List.find_map
      (fun (p, d) ->
        match
          Charon.NameMatcher.match_name env.name_ctx RustNames.config p (name_of_id env decl)
        with
        | true -> Some d
        | false -> None
        | exception _ -> None)
      replacements
  with
  | Some d ->
      let lid = lid_of_name env (name_of_id env decl) in
      L.log "AstOfLlbc" "Found replacement for %a" plid lid;
      Some (d lid)
  | None -> (
      try decl_of_id env decl
      with e ->
        let matches p =
          Charon.NameMatcher.match_name env.name_ctx RustNames.config p (name_of_id env decl)
        in
        if not (List.exists matches known_failures) then begin
          Printf.eprintf "ERROR translating %s: %s\n%s"
            (string_of_pattern (pattern_of_name env (name_of_id env decl)))
            (Printexc.to_string e) (Printexc.get_backtrace ());
          if not !Options.keep_going then
            exit 255
          else
            None
        end
        else
          None)

let flatten_declarations (d : C.declaration_group list) : C.item_id list =
  List.flatten (List.map declaration_group_to_list d)

let decls_of_declarations (env : env) (d : C.item_id list) : K.decl list =
  incr seen;
  L.log "Progress" "%s: %d/%d" env.crate_name !seen !total;
  Krml.KList.filter_some @@ List.map (decl_of_id env) d

(*
let impl_obligation (ob: decl_obligation) : K.decl =
    match ob with ObliArray (lid, t_array) ->
      L.log "AstOfLlbc" "append new decl of struct: %a" plid lid;
      K.DType (lid, [], 1, 1, Flat [(Some "data",(t_array,true))])
     
let impl_obligations (obpairs : (decl_obligation * unit) list) : K.decl list =
  List.map impl_obligation (List.map fst obpairs)
  *)

let file_of_crate (crate : Charon.LlbcAst.crate) : Krml.Ast.file =
  let {
    C.name;
    declarations;
    type_decls;
    fun_decls;
    global_decls;
    trait_decls;
    trait_impls;
    options;
    _;
  } =
    crate
  in
  if options.preset <> Some Eurydice then begin
    Printf.eprintf "ERROR: Eurydice expects Charon to be invoked with `--preset=eurydice`\n";
    exit 255
  end;
  let declarations = flatten_declarations declarations in
  seen := 0;
  total := List.length declarations;
  let format_env = Charon.PrintLlbcAst.Crate.crate_to_fmt_env crate in
  let get_nth_function id =
    match C.FunDeclId.Map.find_opt id fun_decls with
    | Some f -> f
    | None ->
        fail "Function id not found: %s"
          (Charon.PrintExpressions.fun_decl_id_to_string format_env id)
  in
  let get_nth_type id =
    match C.TypeDeclId.Map.find_opt id type_decls with
    | Some ty -> ty
    | None -> fail "Type id not found: %s" (Charon.PrintTypes.type_decl_id_to_string format_env id)
  in
  let get_nth_global id =
    match C.GlobalDeclId.Map.find_opt id global_decls with
    | Some g -> g
    | None ->
        fail "Global id not found: %s" (Charon.PrintTypes.global_decl_id_to_string format_env id)
  in
  let get_nth_trait_impl id =
    match C.TraitImplId.Map.find_opt id trait_impls with
    | Some impl -> impl
    | None ->
        fail "Trait impl id not found: %s" (Charon.PrintTypes.trait_impl_id_to_string format_env id)
  in
  let get_nth_trait_decl id =
    match C.TraitDeclId.Map.find_opt id trait_decls with
    | Some decl -> decl
    | None ->
        fail "Trait decl id not found: %s" (Charon.PrintTypes.trait_decl_id_to_string format_env id)
  in
  let name_ctx = Charon.NameMatcher.ctx_from_crate crate in
  let env =
    {
      get_nth_function;
      get_nth_type;
      get_nth_global;
      get_nth_trait_impl;
      get_nth_trait_decl;
      crate;
      cg_binders = [];
      binders = [];
      type_binders = [];
      format_env;
      crate_name = name;
      name_ctx;
      generic_params = Charon.TypesUtils.empty_generic_params;
    }
  in
  let trans_decls = decls_of_declarations env declarations in
  let extra_decls = Builtin.[ dst_ref_shared_decl; dst_ref_mut_decl; decl_of_arr ] in
  name, trans_decls @ extra_decls
