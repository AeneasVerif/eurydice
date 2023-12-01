(* C for Charon *)
module C = struct
  include Charon.GAst
  include Charon.LlbcAst
  include Charon.Types
  include Charon.Expressions
  include Charon.Values

  let tsubst args ty =
    (object
      inherit [_] map_ty

      method! visit_TVar _ v =
        TypeVarId.nth args v

      method visit_'r _ x = x
    end)#visit_ty () ty
end

module K = Krml.Ast

module L = Logging

module NameMap = Map.Make(struct type t = C.name let compare = compare end)

(** Environment *)

type env = {
  (* Lookup functions to resolve various id's into actual declarations. *)
  get_nth_function: C.FunDeclId.id -> C.fun_decl;
  get_nth_type: C.TypeDeclId.id -> C.type_decl;
  get_nth_global: C.GlobalDeclId.id -> C.global_decl;
  get_nth_trait_impl: C.TraitImplId.id -> C.trait_impl;

  (* Needed by the name matching logic *)
  name_ctx: Charon.NameMatcher.ctx;
  generic_params: C.generic_params;
  names: C.fun_decl_id NameMap.t;

  (* To compute DeBruijn indices *)
  binders: (C.var_id * K.typ * C.ety) list;
  type_binders: C.type_var_id list;

  (* For printing. *)
  format_env: Charon.PrintLlbcAst.fmt_env;

  (* For picking pretty names *)
  crate_name: string;
}

(* Environment: types *)

let lookup_typ env (v1: C.type_var_id) =
  let exception Found of int in
  try
    List.iteri (fun i v2 ->
      if v1 = v2 then
        raise (Found i)
    ) env.type_binders;
    raise Not_found
  with Found i ->
    i

let push_type_binder env (t: C.type_var) =
  { env with type_binders = t.index :: env.type_binders }

let push_type_binders env (ts: C.type_var list) =
  List.fold_left push_type_binder env ts


(** Helpers: types *)

let with_any = K.(with_type TAny)

let assert_slice (t: K.typ) =
  match t with
  | TApp (lid, [ t ]) when lid = Builtin.slice ->
      t
  | _ ->
      Krml.Warn.fatal_error "Not a slice: %a" Krml.PrintAst.Ops.ptyp t

let string_of_path_elem (env: env) (p: Charon.Types.path_elem): string =
  match p with
  | PeIdent (s, _) -> s
  | PeImpl i -> Charon.PrintTypes.impl_elem_to_string env.format_env i

let string_of_name env ps = String.concat "::" (List.map (string_of_path_elem env) ps)

let mk_field_name f i =
  match f with
  | Some f -> f
  | None -> "f" ^ string_of_int i

(* Helpers: traits finding & matching *)

let push_name env name id =
  { env with names = NameMap.add name id env.names }

module RustNames = struct
  open Charon.NameMatcher

  let config = {
    map_vars_to_vars = false;
    match_with_trait_decl_refs = true;
    (* use_trait_decl_refs = true; *)
  }

  let vec = parse_pattern "alloc::vec::Vec<@>"
  let range = parse_pattern "core::ops::range::Range<@>"
  let option = parse_pattern "core::option::Option<@>"

  let slice_subslice =
    parse_pattern "core::ops::index::Index<[@T], core::ops::range::Range<usize>>::index"
  let slice_subslice_mut =
    parse_pattern "core::ops::index::IndexMut<[@T], core::ops::range::Range<usize>>::index_mut"
  let array_subslice =
    parse_pattern "core::ops::index::Index<[@T; @N], core::ops::range::Range<usize>>::index"
  let array_subslice_mut =
    parse_pattern "core::ops::index::IndexMut<[@T; @N], core::ops::range::Range<usize>>::index_mut"
  let array_to_slice =
    parse_pattern "ArrayToSliceShared<'_, @T, @N>"
  let array_to_slice_mut =
    parse_pattern "ArrayToSliceMut<'_, @T, @N>"
  let slice_index =
    parse_pattern "SliceIndexShared<'_, @T>"
  let slice_index_mut =
    parse_pattern "SliceIndexMut<'_, @T>"
  let from_u16 =
    parse_pattern "core::convert::From<u16, @U>::from"
  let from_u32 =
    parse_pattern "core::convert::From<u32, @U>::from"
  let from_u64 =
    parse_pattern "core::convert::From<u64, @U>::from"
  let from_u128 =
    parse_pattern "core::convert::From<u128, @U>::from"
  let from_i16 =
    parse_pattern "core::convert::From<i16, @U>::from"
  let from_i32 =
    parse_pattern "core::convert::From<i32, @U>::from"
  let from_i64 =
    parse_pattern "core::convert::From<i64, @U>::from"
  let from_i128 =
    parse_pattern "core::convert::From<i128, @U>::from"
  let from =
    parse_pattern "core::convert::From<@T, @U>::from"

  let is_vec env =
    match_pattern_with_type_id env.name_ctx config (mk_empty_maps ()) vec

  let is_range env =
    match_pattern_with_type_id env.name_ctx config (mk_empty_maps ()) range

  let is_option env =
    match_pattern_with_type_id env.name_ctx config (mk_empty_maps ()) option
end

let string_of_pattern pattern =
  Charon.NameMatcher.(pattern_to_string { tgt = TkPattern } pattern)

let pattern_of_fn_ptr env fn_ptr =
  Charon.NameMatcher.(fn_ptr_to_pattern env.name_ctx {
      tgt = TkPattern; use_trait_decl_refs = true
    } Charon.TypesUtils.empty_generic_params fn_ptr
  )

let string_of_fn_ptr env fn_ptr =
  string_of_pattern (pattern_of_fn_ptr env fn_ptr)

let find_name env pattern =
  let matches = NameMap.filter (fun n _ ->
    let r = string_of_name env n = pattern in
    L.log "Calls" "Matching %s against %s: %b" (string_of_name env n) pattern r;
    r
    (* let r = Charon.NameMatcher.match_name env.name_ctx RustNames.config pattern n in *)
    (* L.log "Calls" "Matching %s against %s: %b" (string_of_name env n) (string_of_pattern pattern) r; *)
    (* r *)
  ) env.names in
  let n = NameMap.cardinal matches in
  if n = 1 then
    Some (snd (NameMap.choose matches))
  else if n = 0 then begin
    L.log "Calls" "No matches for %s" pattern;
    None
  end else begin
    L.log "Calls" "Too many matches for %s" pattern;
    None
  end

(** Translation of types *)

let lid_of_name (env: env) (name: Charon.Types.name): K.lident =
  let prefix, name = Krml.KList.split_at_last name in
  List.map (string_of_path_elem env) prefix, string_of_path_elem env name

let width_of_integer_type (t: Charon.Types.integer_type): K.width =
  match t with
  | Isize -> failwith "TODO: Isize"
  | I8 -> Int8
  | I16 -> Int16
  | I32 -> Int32
  | I64 -> Int64
  | I128 -> failwith "TODO: I128"
  | Usize -> SizeT
  | U8 -> UInt8
  | U16 -> UInt16
  | U32 -> UInt32
  | U64 -> UInt64
  | U128 -> failwith "TODO: U128"

let lid_of_type_decl_id (env: env) (id: C.type_decl_id) =
  let { C.name; _ } = env.get_nth_type id in
  lid_of_name env name

let constant_of_scalar_value { C.value; int_ty } =
  let w = width_of_integer_type int_ty in
  w, Z.to_string value

let rec typ_of_ty (env: env) (ty: Charon.Types.ty): K.typ =
  match ty with
  | TVar id ->
      K.TBound (lookup_typ env id)
  | TLiteral TBool ->
      K.TBool
  | TLiteral TChar ->
      failwith "TODO: Char"
  | TNever ->
      failwith "Impossible: Never"
  | TLiteral (TInteger k) ->
      K.TInt (width_of_integer_type k)

  | TRef (_, TAdt (id, ({ types = [ t ]; _ } as generics)), _) when RustNames.is_vec env id generics ->
      (* We compile vecs to fat pointers, which hold the pointer underneath -- no need for an
         extra reference here. *)
      Builtin.mk_vec (typ_of_ty env t)

  | TRef (_, TAdt (TAssumed TSlice, { types = [ t ]; _ }), _) ->
      (* We compile slices to fat pointers, which hold the pointer underneath -- no need for an
         extra reference here. *)
      Builtin.mk_slice (typ_of_ty env t)

  | TRef (_, TAdt (TAssumed TArray, { types = [ t ]; _ }), _) ->
      (* We collapse Ref(Array) into a pointer type, leveraging C's implicit decay between array
         types and pointer types. *)
      K.TBuf (typ_of_ty env t, false)

  | TRef (_, t, _) ->
      (* Normal reference *)
      K.TBuf (typ_of_ty env t, false)

  | TAdt (id, ({ types = [ t ]; _ } as generics)) when RustNames.is_vec env id generics ->
      Builtin.mk_vec (typ_of_ty env t)

  | TAdt (id, ({ types = [ t ]; _ } as generics)) when RustNames.is_range env id generics->
      Builtin.mk_range (typ_of_ty env t)

  | TAdt (id, ({ types = [ t ]; _ } as generics)) when RustNames.is_option env id generics ->
      Builtin.mk_option (typ_of_ty env t)

  | TAdt (TAdtId id, { types = []; const_generics = []; _ }) ->
      TQualified (lid_of_type_decl_id env id)

  | TAdt (TAdtId id, { types = args; const_generics = generic_args; _ }) ->
      if List.length generic_args > 0 then
        failwith "TODO: Adt/generic_args";
      TApp (lid_of_type_decl_id env id, List.map (typ_of_ty env) args)

  | TAdt (TTuple, { types = args; const_generics; _ }) ->
      assert (const_generics = []);
      if args = [] then
        TUnit
      else begin
        assert (List.length args > 1);
        TTuple (List.map (typ_of_ty env) args)
      end

  | TAdt (TAssumed TArray, { types = [ t ]; const_generics = [ CgValue (VScalar n) ]; _ }) ->
      K.TArray (typ_of_ty env t, constant_of_scalar_value n)

  | TAdt (TAssumed TSlice, _) ->
      (* Slice values cannot be materialized since their storage space cannot be computed at
         compile-time; we should never encounter this case. *)
      assert false

  | TAdt (TAssumed TBox, { types = [ t ]; _ }) ->
      K.TBuf (typ_of_ty env t, false)

  | TAdt (TAssumed f, { types = args; const_generics; _ }) ->
      List.iter (fun x -> print_endline (C.show_const_generic x)) const_generics;
      Krml.Warn.fatal_error "TODO: Adt/Assumed %s (%d) %d " (C.show_assumed_ty f) (List.length args)
        (List.length const_generics)

  | TRawPtr _ ->
      failwith "TODO: TRawPtr"

  | TTraitType _ ->
      failwith "TODO: TraitTypes"

  | TArrow (ts, t) ->
      Krml.Helpers.fold_arrow (List.map (typ_of_ty env) ts) (typ_of_ty env t)

(* Helpers: expressions *)

(* To be desugared later into variable hoisting, allocating suitable storage space, followed by a
   memcpy. *)
let mk_deep_copy (e: K.expr) (t: K.typ) (l: K.constant) =
  let builtin_copy_operator = K.EQualified Builtin.array_copy in
  let builtin_copy_operator_t = K.TArrow (TArray (t, l), TArray (t, l)) in
  K.(with_type (TArray (t, l)) (EApp (with_type builtin_copy_operator_t builtin_copy_operator, [ e ])))

(* Environment: expressions *)

let lookup_with_original_type env (v1: C.var_id) =
  let exception Found of int * K.typ * C.ety in
  try
    List.iteri (fun i (v2, t, ty) ->
      if v1 = v2 then
        raise (Found (i, t, ty))
    ) env.binders;
    raise Not_found
  with Found (i, t, ty) ->
    i, t, ty

let lookup env v1 =
  let i, t, _ = lookup_with_original_type env v1 in
  i, t

let push_binder env (t: C.var) =
  { env with binders = (t.index, typ_of_ty env t.var_ty, t.var_ty) :: env.binders }

let push_binders env (ts: C.var list) =
  List.fold_left push_binder env ts


(** Translation of expressions (statements, operands, rvalues, places) *)

let uu =
  let r = ref 0 in
  fun () ->
    let suffix = string_of_int !r in
    incr r;
    "uu____" ^ suffix

let binder_of_var (env: env) (l: C.var): K.binder =
  let name = Option.value ~default:(uu ()) l.name in
  Krml.Helpers.fresh_binder ~mut:true name (typ_of_ty env l.var_ty)

let find_nth_variant (env: env) (typ: C.type_decl_id) (var: C.variant_id) =
  match env.get_nth_type typ with
  | { kind = Enum variants; _ } ->
      Charon.Types.VariantId.nth variants var
  | _ ->
      failwith "impossible: type is not a variant"

let rec with_locals (env: env) (t: K.typ) (locals: C.var list) (k: env -> 'a): 'a =
  match locals with
  | [] -> k env
  | l :: locals ->
      let env = push_binder env l in
      let b = binder_of_var env l in
      K.(with_type t (ELet (b, Krml.Helpers.any, with_locals env t locals k)))

let expression_of_var_id (env: env) (v: C.var_id): K.expr =
  let i, t = lookup env v in
  K.(with_type t (EBound i))

let expression_and_original_type_of_var_id (env: env) (v: C.var_id): K.expr * C.ety =
  let i, t, ty = lookup_with_original_type env v in
  K.(with_type t (EBound i)), ty

let expression_of_place (env: env) (p: C.place): K.expr * C.ety =
  let { C.var_id; projection } = p in
  let e, ty = expression_and_original_type_of_var_id env var_id in
  (* We construct a target expression, but retain the original type so that callers can tell arrays
     and references apart, since their *uses* (e.g. addr-of) compile in a type-directed way based on
     the *original* rust type *)
  List.fold_left (fun (e, (ty: C.ety)) pe ->
    match pe, ty with
    | C.Deref, TRef (_, (TAdt (TAssumed TArray, { types = [ t ]; _ }) as ty), _) ->
        (* Array is passed by reference; when appearing in a place, it'll automatically decay in C *)
        K.with_type (TBuf (typ_of_ty env t, false)) e.K.node, ty

    | C.Deref, TRef (_, (TAdt (TAssumed TSlice, _) as t), _) ->
        e, t

    | C.Deref, TRef (_, (TAdt (id, generics) as t), _) when RustNames.is_vec env id generics ->
        e, t

    | C.Deref, TRef (_, ty, _) ->
        Krml.Helpers.(mk_deref (Krml.Helpers.assert_tbuf_or_tarray e.K.typ) e.K.node), ty

    | DerefBox, TAdt (TAssumed TBox, { types = [ ty ]; _ }) ->
        Krml.Helpers.(mk_deref (Krml.Helpers.assert_tbuf_or_tarray e.K.typ) e.K.node), ty

    | Field (ProjAdt (typ_id, variant_id), field_id), C.TAdt (_, { types; _ }) ->
        begin match variant_id with
        | None ->
            let { C.kind; _ } = env.get_nth_type typ_id in
            let fields = match kind with Struct fields -> fields | _ -> failwith "not a struct" in
            let field_name, field_ty =
              let field = List.nth fields (C.FieldId.to_int field_id) in
              match field.C.field_name with
              | Some field_name -> field_name, C.tsubst types field.C.field_ty
              | None -> failwith "TODO: understand what empty field name means"
            in
            K.with_type (typ_of_ty env field_ty) (K.EField (e, field_name)),
            field_ty
        | Some variant_id ->
            let variant = find_nth_variant env typ_id variant_id in
            let field_id = C.FieldId.to_int field_id in
            let field = List.nth variant.fields field_id in
            let field_ty = C.tsubst types field.C.field_ty in
            let field_t = typ_of_ty env field_ty in
            let b = Krml.Helpers.fresh_binder (mk_field_name field.C.field_name field_id) field_t in
            K.with_type field_t K.(EMatch (Unchecked,
              e,
              [[ b ],
              with_type e.typ (
                PCons (variant.C.variant_name,
                Krml.KList.make (List.length variant.fields) (fun i ->
                  if i = field_id then
                    with_type field_t (PBound 0)
                  else
                    with_type TAny PWild))),
              with_type field_t (EBound 0)])),
            field_ty
        end

    | Field (ProjTuple n, i), C.TAdt (_, { types = tys; const_generics = cgs; _ }) ->
        assert (cgs = []);
        (* match e with (_, ..., _, x, _, ..., _) -> x *)
        let i = Charon.Types.FieldId.to_int i in
        let ts, t_i =
          match e.typ with
          | TTuple ts ->
              assert (List.length ts = n);
              ts, List.nth ts i
          | _ ->
              failwith "impossible: mismatch ProjTuple/TTuple"
        in
        let binders = [ Krml.Helpers.fresh_binder (uu ()) t_i ] in
        let pattern =
          K.with_type e.typ (K.PTuple (List.mapi (fun i' t ->
            K.with_type t (if i = i' then K.PBound 0 else PWild)) ts))
        in
        let expr = K.with_type t_i (K.EBound 0) in
        K.with_type t_i (K.EMatch (Checked, e, [ binders, pattern, expr ])), List.nth tys i

    | _ ->
        failwith "unexpected / ill-typed projection"
  ) (e, ty) projection

let expression_of_scalar_value ({ C.int_ty; _ } as sv) =
  let w = width_of_integer_type int_ty in
  K.(with_type (TInt w) (EConstant (constant_of_scalar_value sv)))

let expression_of_operand (env: env) (p: C.operand): K.expr =
  match p with
  | Copy p ->
      let p, ty = expression_of_place env p in
      begin match ty with
      | C.TAdt (TAssumed TArray, { types = [ t ]; const_generics = [ CgValue (VScalar n) ]; _ }) ->
          mk_deep_copy p (typ_of_ty env t) (constant_of_scalar_value n)
      | _ ->
          p
      end

  | Move p ->
      fst (expression_of_place env p)
  | Constant ({ value = CLiteral (VScalar sv); _ }) ->
      expression_of_scalar_value sv
  | Constant ({ value = CLiteral (VBool b); _ }) ->
      K.(with_type TBool (EBool b))
  | Constant _ ->
      failwith "expression_of_operand Constant"

let op_of_unop (op: C.unop): Krml.Constant.op =
  match op with
  | C.Not -> Not
  | C.Neg -> Neg
  | _ -> assert false

let op_of_binop (op: C.binop): Krml.Constant.op =
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
  | C.Div -> Div
  | C.Rem -> Mod
  | C.Add -> Add
  | C.Sub -> Sub
  | C.Mul -> Mult
  | C.Shl -> BShiftL
  | C.Shr -> BShiftR

let mk_op_app (op: K.op) (first: K.expr) (rest: K.expr list): K.expr =
  let w = match first.typ with
    | K.TInt w -> w
    | K.TBool -> Bool
    | t -> Krml.Warn.fatal_error "Not an operator type: %a" Krml.PrintAst.Ops.ptyp t
  in
  let op_t = Krml.Helpers.type_of_op op w in
  let op = K.(with_type op_t (EOp (op, w))) in
  let ret_t, _ = Krml.Helpers.flatten_arrow op_t in
  K.(with_type ret_t (EApp (op, first :: rest)))

(* According to the rules (see my notebook), array and slice types do not need
   the address-taking because they are already addresses. Therefore, the
   compilation scheme skips the address-taking operation and represents a value
   of type [T; N] as T[N] and a value of type &[T; N] as T*, relying on the fact
   that the former converts automatically to the latter. This is a type-driven
   translation that does not work with polymorphism, so perhaps there ought to
   be a MAYBE_CAST operator that gets desugared post-krml monomorphization. TBD.
   *)
let maybe_addrof (env: env) (ty: C.ty) (e: K.expr) =
  (* ty is the *original* Rust type *)
  match ty with
  | TAdt (id, generics) when RustNames.is_vec env id generics ->
      e
  | TAdt (TAssumed (TArray | TSlice), _) ->
      e
  | _ ->
      K.(with_type (TBuf (e.typ, false)) (EAddrOf e))

let expression_of_rvalue (env: env) (p: C.rvalue): K.expr =
  match p with
  | Use op ->
      expression_of_operand env op
  | RvRef (p, _) ->
      let e, ty = expression_of_place env p in
      (* Arrays and ref to arrays are compiled as pointers in C; we allow on implicit array decay to
         pass one for the other *)
      maybe_addrof env ty e

  | UnaryOp (Cast (CastInteger (_, dst)), e) ->
      let dst = K.TInt (width_of_integer_type dst) in
      K.with_type dst (K.ECast (expression_of_operand env e, dst))
  | UnaryOp (op, o1) ->
      mk_op_app (op_of_unop op) (expression_of_operand env o1) []
  | BinaryOp (op, o1, o2) ->
      mk_op_app (op_of_binop op) (expression_of_operand env o1) [ expression_of_operand env o2 ]
  | Discriminant _ ->
      failwith "expression_of_rvalue Discriminant"
  | Aggregate (AggregatedAdt (TTuple, _, _), ops) ->
      let ops = List.map (expression_of_operand env) ops in
      let ts = List.map (fun x -> x.K.typ) ops in
      if ops = [] then
        K.with_type TUnit K.EUnit
      else begin
        assert (List.length ops > 1);
        K.with_type (TTuple ts) (K.ETuple ops)
      end
  | Aggregate (AggregatedAdt (TAdtId typ_id, variant_id, { types = typ_args; _ }), args) ->
      let { C.name; kind; _ } = env.get_nth_type typ_id in
      let typ_lid = lid_of_name env name in
      let typ_args = List.map (typ_of_ty env) typ_args in
      let t = if typ_args = [] then K.TQualified typ_lid else TApp (typ_lid, typ_args) in
      let args = List.map (expression_of_operand env) args in
      begin match variant_id with
      | Some variant_id ->
          let variant_id = (find_nth_variant env typ_id variant_id).variant_name in
          K.with_type t (K.ECons (variant_id, args))
      | None ->
          let fields = match kind with Struct fields -> fields | _ -> failwith "not a struct" in
          K.with_type t (K.EFlat (List.map2 (fun f a -> f.C.field_name, a) fields args))
      end
  | Aggregate (AggregatedAdt (TAssumed _, _, _), _) ->
      failwith "unsupported: AggregatedAdt / TAssume"
  | Aggregate (AggregatedArray (t, _), ops) ->
      K.with_type (typ_of_ty env t) (K.EBufCreateL (Stack, List.map (expression_of_operand env) ops))
  | Global id ->
      let global = env.get_nth_global id in
      K.with_type (typ_of_ty env global.ty) (K.EQualified (lid_of_name env global.name))

let expression_of_assertion (env: env) ({ cond; expected }: C.assertion): K.expr =
  let cond =
    if not expected then
      expression_of_operand env cond
    else
      Krml.Helpers.mk_not (expression_of_operand env cond)
  in
  K.(with_type TAny (
    EIfThenElse (cond,
      with_type TAny (EAbort (None, Some "assert failure")),
      Krml.Helpers.eunit)))

let lookup_fun (env: env) (f: C.fn_ptr): K.lident * int * K.typ list * K.typ =
  let open RustNames in
  let matches p = Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config p f in
  let builtin b =
    let { Builtin.name; typ; n_type_args; _ } = b in
    let ret, args = Krml.Helpers.flatten_arrow typ in
    name, n_type_args, args, ret
  in
  if matches slice_subslice || matches slice_subslice_mut then
    builtin Builtin.slice_subslice
  else if matches array_subslice || matches array_subslice_mut then
    builtin Builtin.array_to_subslice
  else if matches array_to_slice || matches array_to_slice_mut then
    builtin Builtin.array_to_slice
  else if matches slice_index || matches slice_index_mut then
    builtin Builtin.slice_index
  else
    let regular f =
      let { C.name; signature = { generics = { types = type_params; _ }; inputs; output; _ }; _ } = env.get_nth_function f in
      L.log "Calls" "--> name: %s" (string_of_name env name);
      let env = push_type_binders env type_params in
      lid_of_name env name, List.length type_params, List.map (typ_of_ty env) inputs, typ_of_ty env output
    in
    match f.func with
    | FunId (FRegular f) ->
        regular f

    | FunId (FAssumed f) ->
        Krml.Warn.fatal_error "unknown assumed function: %s" (C.show_assumed_fun_id f)

    | TraitMethod (trait_ref, method_name, _trait_opaque_signature) ->
        let pp_name = Charon.PrintTypes.trait_instance_id_to_string env.format_env trait_ref.trait_id ^ "::" ^ method_name in
        begin match trait_ref.trait_id with
        | TraitImpl id ->
            L.log "Calls" "trait id: %d" (C.TraitImplId.to_int id);
            L.log "Calls" "trait name: %s" (string_of_name env (env.get_nth_trait_impl id).name)
        | _ -> ()
        end;
        match find_name env pp_name with
        | Some f ->
            L.log "Calls" "fun id: %d" (C.FunDeclId.to_int f);
            L.log "Calls" "FOUND trait ref: %s %s"
              (Charon.PrintTypes.trait_ref_to_string env.format_env trait_ref) method_name;
            regular f
        | _ ->
            Krml.Warn.fatal_error "Error looking trait ref: %s %s"
              (Charon.PrintTypes.trait_ref_to_string env.format_env trait_ref) method_name

(* Runs after the adjustment above *)
let args_of_const_generic_args (f: C.fun_id_or_trait_method_ref) (const_generic_args: C.const_generic list): K.expr list =
  match f, const_generic_args with
  | FunId (FAssumed (ArrayToSliceShared | ArrayToSliceMut)), [ CgValue (VScalar n) ] ->
      [ expression_of_scalar_value n ]
  | _ ->
      []

let lesser t1 t2 =
  if t1 = K.TAny then
    t2
  else if t2 = K.TAny then
    t2
  else if t1 <> t2 then
    Krml.Warn.fatal_error "lesser t1=%a t2=%a" Krml.PrintAst.Ops.ptyp t1 Krml.PrintAst.Ops.ptyp t2
  else
    t1

let rec expression_of_raw_statement (env: env) (ret_var: C.var_id) (s: C.raw_statement): K.expr =
  match s with
  | Assign (p, rv) ->
      let p, _ = expression_of_place env p in
      let rv = expression_of_rvalue env rv in
      K.(with_type TUnit (EAssign (p, rv)))
  | SetDiscriminant (_, _) ->
      failwith "C.SetDiscriminant"
  | FakeRead _ ->
      Krml.Helpers.eunit
  | Drop p ->
      let _p, ty = expression_of_place env p in
      begin match ty with
      (* doesn't do the right thing yet, need to understand why there are
         several drops per variable *)
      (* | C.Adt (Assumed Vec, _) when false -> *)
      (*     (1* p is a vec t *1) *)
      (*     let t = match p.typ with TApp ((["Eurydice"], "vec"), [ t ]) -> t | _ -> assert false in *)
      (*     Krml.Helpers.(with_unit K.(EApp ( *)
      (*       with_type (Krml.DeBruijn.subst_tn [ t ] Builtin.vec_drop.typ) (ETApp ( *)
      (*         with_type Builtin.vec_drop.typ (EQualified Builtin.vec_drop.name), *)
      (*         [ t ])), *)
      (*       [ p ]))) *)
      | _ ->
          Krml.Helpers.eunit
      end
  | Assert a ->
      expression_of_assertion env a

  | Call {
      func = { func = FunId (FAssumed ArrayRepeat); generics = { types = [ ty ]; const_generics = [ c ]; _ }; _ };
      args = [ e ];
      dest;
      _
    } ->
      (* Special treatment because of the insertion of a CreateL *)
      let e = expression_of_operand env e in
      let t = typ_of_ty env ty in
      let dest, _ = expression_of_place env dest in
      let n = match c with
        | CgValue (VScalar n) -> n
        | _ -> failwith "unexpected const generic for ArrayRepeat" in
      let c = constant_of_scalar_value n in
      let n = Z.to_int n.value in
      Krml.Helpers.with_unit K.(
        EAssign (dest, (with_type (TArray (t, c)) (EBufCreateL (Stack, List.init n (fun _ -> e))))))

  | Call {
      func = { func = FunId (FAssumed (ArrayIndexShared | ArrayIndexMut)); generics = { types = [ ty ]; _ }; _ };
      args = [ e1; e2 ];
      dest;
      _
    } ->
      (* Special treatment because of the usage of maybe_addrof *)
      let e1 = expression_of_operand env e1 in
      let e2 = expression_of_operand env e2 in
      let t = typ_of_ty env ty in
      let dest, _ = expression_of_place env dest in
      Krml.Helpers.with_unit K.(EAssign (dest, maybe_addrof env ty (with_type t (EBufRead (e1, e2)))))

  | Call { func = fn_ptr; args; dest; _ }
    when Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config RustNames.from fn_ptr ->
      let matches p = Charon.NameMatcher.match_fn_ptr env.name_ctx RustNames.config p fn_ptr in
      let w: Krml.Constant.width =
        if matches RustNames.from_u16 then UInt16
        else if matches RustNames.from_u32 then UInt32
        else if matches RustNames.from_u64 then UInt64
        else if matches RustNames.from_i16 then Int16
        else if matches RustNames.from_i32 then Int32
        else if matches RustNames.from_i64 then Int64
        else Krml.Warn.fatal_error "Unknown from-cast: %s" (string_of_fn_ptr env fn_ptr)
      in
      let dest, _ = expression_of_place env dest in
      let e = expression_of_operand env (Krml.KList.one args) in
      Krml.Helpers.with_unit K.(EAssign (dest, with_type (TInt w) (ECast (e, TInt w))))

  | Call { func = { func; generics = { types = type_args; const_generics = const_generic_args; _ }; _ } as fn_ptr; args; dest; _ } ->
      (* General case for function calls and trait method calls. *)
      L.log "Calls" "Visiting call: %s" (Charon.PrintExpressions.fn_ptr_to_string env.format_env fn_ptr);
      L.log "Calls" "--> pattern: %s" (string_of_fn_ptr env fn_ptr);
      L.log "Calls" "--> %d type_args, %d const_generics" (List.length type_args) (List.length const_generic_args);

      (* For now, we take trait type arguments to be part of the code-gen *)
      let type_args, const_generic_args =
        match func with
        | FunId _ ->
            type_args, const_generic_args
        | TraitMethod ({ generics = { types; const_generics; _ }; _ }, _, _) ->
            types @ type_args, const_generics @ const_generic_args
      in
      let dest, _ = expression_of_place env dest in
      let args = List.map (expression_of_operand env) args in
      let original_type_args = type_args in
      let type_args = List.map (typ_of_ty env) type_args in
      let args = args @ args_of_const_generic_args func const_generic_args in
      let args = if args = [] then [ Krml.Helpers.eunit ] else args in
      let name, n_type_params, inputs, output = lookup_fun env fn_ptr in
      let inputs = if inputs = [] then [ K.TUnit ] else inputs in
      if not (n_type_params = List.length type_args) then
        Krml.Warn.fatal_error "%a: n_type_params %d != type_args %d"
          Krml.PrintAst.Ops.plid name
          n_type_params (List.length type_args);
      let poly_t = Krml.Helpers.fold_arrow inputs output in
      let output, t =
        Krml.DeBruijn.subst_tn type_args output, Krml.DeBruijn.subst_tn type_args poly_t
      in
      let hd =
        let hd = K.with_type poly_t (K.EQualified name) in
        if type_args <> [] then
          K.with_type t (K.ETApp (hd, type_args))
        else
          hd
      in
      let rhs = K.with_type output (K.EApp (hd, args)) in
      (* This does something similar to maybe_addrof *)
      let rhs =
        match func, original_type_args with
        | FunId (FAssumed (SliceIndexShared | SliceIndexMut)), [ TAdt (TAssumed (TArray | TSlice), _) ] ->
            (* Will decay. See comment above maybe_addrof *)
            rhs
        | FunId (FAssumed (SliceIndexShared | SliceIndexMut)), [ TAdt (id, generics) ] when RustNames.is_vec env id generics ->
            (* Will decay. See comment above maybe_addrof *)
            rhs
        | FunId (FAssumed (SliceIndexShared | SliceIndexMut)), _ ->
            K.(with_type (TBuf (rhs.typ, false)) (EAddrOf rhs))
        | _ ->
            rhs
      in
      Krml.Helpers.with_unit K.(EAssign (dest, rhs))

  | Panic ->
      with_any (K.EAbort (None, Some "panic!"))

  | Return ->
      let e = expression_of_var_id env ret_var in
      K.(with_type TAny (EReturn e))

  | Break _ ->
      K.(with_type TAny EBreak)

  | Continue _ ->
      K.(with_type TAny EContinue)

  | Nop ->
      Krml.Helpers.eunit

  | Sequence (s1, s2) ->
      let e1 = expression_of_raw_statement env ret_var s1.content in
      let e2 = expression_of_raw_statement env ret_var s2.content in
      K.(with_type e2.typ (ESequence [ e1; e2 ]))

  | Switch (If (op, s1, s2)) ->
      let e1 = expression_of_raw_statement env ret_var s1.content in
      let e2 = expression_of_raw_statement env ret_var s2.content in
      let t = lesser e1.typ e2.typ in
      K.(with_type t (EIfThenElse (expression_of_operand env op, e1, e2 )))

  | Switch (SwitchInt (_, _, _, _)) ->
      failwith "expression_of_raw_statement SwitchInt"

  | Switch (Match (p, branches, default)) ->
      let p, ty = expression_of_place env p in
      let variant_name_of_variant_id =
        match ty with
        | TAdt (TAdtId typ_id, _) ->
            let { C.kind; _ } = env.get_nth_type typ_id in
            let variants = match kind with Enum variants -> variants | _ -> assert false in
            fun v ->
              let v = C.VariantId.nth variants v in
              v.variant_name, List.length v.fields
        | _ ->
            failwith "TODO: match on not adt, not option"
      in

      let branches = List.concat_map (fun (variant_ids, e) ->
        List.map (fun variant_id ->
          let variant_name, n_fields = variant_name_of_variant_id variant_id in
          let dummies = Krml.KList.make n_fields (fun _ -> K.(with_type TAny PWild)) in
          let pat = K.with_type p.typ (K.PCons (variant_name, dummies)) in
          [], pat, expression_of_raw_statement env ret_var e.C.content
        ) variant_ids
      ) branches in
      let branches = branches @ [ [], K.with_type p.typ K.PWild, expression_of_raw_statement env ret_var default.C.content ] in
      let t = Krml.KList.reduce lesser (List.map (fun (_, _, e) -> e.K.typ) branches) in
      K.(with_type t (EMatch (Checked, p, branches)))

  | Loop s ->
      K.(with_type TUnit (EWhile (Krml.Helpers.etrue,
        expression_of_raw_statement env ret_var s.content)))


(** Top-level declarations: orchestration *)

let of_declaration_group (env: 'env) (dg: 'id C.g_declaration_group) (f: 'env -> 'id -> 'env * 'a option): 'env * 'a list =
  (* We do not care about recursion as in C, everything is mutually recursive
     thanks to header inclusion. *)
  match dg with
  | NonRecGroup id ->
      let env, x = f env id in
      env, Option.to_list x
  | RecGroup ids ->
      let env, xs = List.fold_left (fun (env, xs) id ->
        let env, x = f env id in
        env, Option.to_list x @ xs
      ) (env, []) ids in
      env, List.rev xs

let decls_of_declarations (env: env) (d: C.declaration_group): env * K.decl list =
  match d with
  | TypeGroup dg ->
      of_declaration_group env dg (fun env0 (id: C.TypeDeclId.id) ->
        let decl = env0.get_nth_type id in
        let { C.name; def_id; kind; generics = { types = type_params; _ }; _ } = decl in
        L.log "AstOfLlbc" "Visiting type: %s\n%s" (string_of_name env0 name)
          (Charon.PrintTypes.type_decl_to_string env0.format_env decl);

        assert (def_id = id);
        let name = lid_of_name env0 name in

        match kind with
        | Opaque ->
            env0, None
        | Struct fields ->
            let env = push_type_binders env0 type_params in
            let fields = List.map (fun { C.field_name; field_ty; _ } ->
              field_name, (typ_of_ty env field_ty, true)
            ) fields in
            env0, Some (K.DType (name, [], List.length type_params, Flat fields))
        | Enum branches ->
            let env = push_type_binders env0 type_params in
            let branches = List.map (fun { C.variant_name; fields; _ } ->
              variant_name, List.mapi (fun i { C.field_name; field_ty; _ } ->
                mk_field_name field_name i,
                (typ_of_ty env field_ty, true)
              ) fields
            ) branches in
            env0, Some (K.DType (name, [], List.length type_params, Variant branches))
      )
  | FunGroup dg ->
      of_declaration_group env dg (fun env0 (id: C.FunDeclId.id) ->
        let decl = env0.get_nth_function id in
        let { C.def_id; name; signature; body; is_global_decl_body; _ } = decl in
        let env = { env0 with generic_params = signature.generics } in
        L.log "AstOfLlbc" "Visiting %sfunction: %s\n%s"
          (if body = None then "opaque " else "")
          (string_of_name env name)
          (Charon.PrintLlbcAst.Ast.fun_decl_to_string env.format_env "  " "  " decl);

        assert (def_id = id);
        match body with
        | None ->
            begin try
              (* Opaque function *)
              let { C.generics = { types = type_params; _ }; inputs; output ; _ } = signature in
              let env = push_type_binders env type_params in
              let inputs = List.map (typ_of_ty env) inputs in
              let output = typ_of_ty env output in
              let t = Krml.Helpers.fold_arrow inputs output in
              let name = lid_of_name env name in
              let name_hints: string list =
                List.map (fun (x: (_, _) Charon.Types.indexed_var) -> x.name) type_params
              in
              push_name env0 decl.name id, Some (K.DExternal (None, [], List.length type_params, name, t, name_hints))
            with _ ->
              L.log "AstOfLLbc" "ERROR translating %s:\n%s" (string_of_name env decl.name)
                (Printexc.get_backtrace ());
              env0, None
            end
        | Some { arg_count; locals; body; _ } ->
            if is_global_decl_body then
              failwith "TODO: C.Fun is_global decl"
            else
              let env = push_type_binders env signature.C.generics.types in
              let name = lid_of_name env name in
              (* `locals` contains, in order: special return variable; function arguments;
                 local variables *)
              let args, locals = Krml.KList.split (arg_count + 1) locals in
              let return_var = List.hd args in
              let args = List.tl args in

              let return_type = typ_of_ty env return_var.var_ty in
              let arg_binders = List.map (fun (arg: C.var) ->
                let name = Option.value ~default:"_" arg.name in
                Krml.Helpers.fresh_binder ~mut:true name (typ_of_ty env arg.var_ty)
              ) args in
              (* Note: Rust allows zero-argument functions but the krml internal
                 representation wants a unit there. *)
              let arg_binders = if arg_binders = [] then [ Krml.Helpers.fresh_binder "dummy" K.TUnit ] else arg_binders in
              let env = push_binders env args in
              let body =
                with_locals env return_type (return_var :: locals) (fun env ->
                  expression_of_raw_statement env return_var.index body.content)
              in
              push_name env0 decl.name id, Some (K.DFunction (None, [], List.length signature.C.generics.types, return_type, name, arg_binders, body))
      )
  | GlobalGroup id ->
      let global = env.get_nth_global id in
      let { C.name; ty; def_id; _ } = global in
      let def = env.get_nth_global def_id in
      L.log "AstOfLlbc" "Visiting global: %s\n%s" (string_of_name env name)
        (Charon.PrintLlbcAst.Ast.global_decl_to_string env.format_env "  "  "  " def);
      let ty = typ_of_ty env ty in
      let body = env.get_nth_function def.body in
      L.log "AstOfLlbc" "Corresponding body:%s"
        (Charon.PrintLlbcAst.Ast.fun_decl_to_string env.format_env "  " "  " body);
      begin match body.body with
      | Some body ->
          let ret_var = List.hd body.locals in
          let body =
            with_locals env ty body.locals (fun env ->
              expression_of_raw_statement env ret_var.index body.body.content)
          in
          env, [ K.DGlobal ([], lid_of_name env name, 0, ty, body) ]
      | None ->
          env, [ K.DExternal (None, [], 0, lid_of_name env name, ty, []) ]
      end

  | TraitDeclGroup _ ->
      env, []
  | TraitImplGroup _ ->
      env, []

let file_of_crate (crate: Charon.LlbcAst.crate): Krml.Ast.file =
  let { C.name; declarations; type_decls; fun_decls; global_decls; trait_decls; trait_impls } = crate in
  let get_nth_function = fun id -> C.FunDeclId.Map.find id fun_decls in
  let get_nth_type = fun id -> C.TypeDeclId.Map.find id type_decls in
  let get_nth_global = fun id -> C.GlobalDeclId.Map.find id global_decls in
  let get_nth_trait_impl = fun id -> C.TraitImplId.Map.find id trait_impls in
  let format_env = Charon.PrintLlbcAst.Crate.crate_to_fmt_env crate in
  let name_ctx: Charon.NameMatcher.ctx = { type_decls; global_decls; trait_decls; fun_decls; trait_impls } in
  let env = {
    get_nth_function;
    get_nth_type;
    get_nth_global;
    get_nth_trait_impl;
    binders = [];
    type_binders = [];
    format_env;
    crate_name = name;
    name_ctx;
    generic_params = { regions = []; types = []; const_generics = []; trait_clauses = [] };
    names = NameMap.empty;
  } in
  let _, decls = List.fold_left (fun (env, dss) d ->
    let env, ds = decls_of_declarations env d in
    env, ds :: dss
  ) (env, []) declarations in
  name, List.concat (List.rev decls)
