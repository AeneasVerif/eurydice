(* C for Charon *)
module C = struct
  include Charon.GAst
  include Charon.LlbcAst
  include Charon.Types
  include Charon.Expressions
  include Charon.PrimitiveValues

  let tsubst args ty =
    (object
      inherit [_] map_ty

      method! visit_TypeVar _ v =
        TypeVarId.nth args v

      method visit_'r _ x = x
    end)#visit_ty () ty

  let rec ety_of_typ (ty: 'a gr_ty): ety =
    match ty with
    | Adt (type_id, generics) ->
        let generics = egeneric_args_of_args generics in
        Adt (type_id, generics)
    | TypeVar v -> TypeVar v
    | Literal ty -> Literal ty
    | Never -> Never
    | TraitType (trait_ref, generics, type_name) ->
        let trait_ref = etrait_ref_of_trait_ref trait_ref in
        let generics = egeneric_args_of_args generics in
        TraitType (trait_ref, generics, type_name)
    | Ref (_, t, k) ->
        Ref (Erased, ety_of_typ t, k)

  and egeneric_args_of_args (g : 'a region generic_args ) :
      egeneric_args=
    let { regions; types; const_generics; trait_refs } = g in
    assert (regions = []);
    let types = List.map ety_of_typ types in
    let trait_refs = List.map etrait_ref_of_trait_ref trait_refs in
    { regions = []; types; const_generics; trait_refs }

  and etrait_ref_of_trait_ref (tr : 'a region trait_ref) : etrait_ref=
    let ({ trait_id; generics; trait_decl_ref } : _) = tr in
    let trait_id = etrait_instance_of_trait_instance trait_id in
    let generics = egeneric_args_of_args generics in
    let trait_decl_ref = etrait_decl_ref_of_decl_erf trait_decl_ref in
    { trait_id; generics; trait_decl_ref }

  and etrait_decl_ref_of_decl_erf (tr : 'a region trait_decl_ref) :
      etrait_decl_ref =
    let ({ trait_decl_id; decl_generics } : _) = tr in
    let decl_generics =
      egeneric_args_of_args decl_generics
    in
    { trait_decl_id; decl_generics }

  and etrait_instance_of_trait_instance (id : 'a region trait_instance_id) : etrait_instance_id =
    match id with
    | Self -> Self
    | TraitImpl id -> TraitImpl id
    | BuiltinOrAuto id -> BuiltinOrAuto id
    | Clause id -> Clause id
    | ParentClause (id, decl_id, cid) ->
        let id = etrait_instance_of_trait_instance id in
        ParentClause (id, decl_id, cid)
    | ItemClause (id, decl_id, name, cid) ->
        let id = etrait_instance_of_trait_instance id in
        ItemClause (id, decl_id, name, cid)
    | TraitRef tr ->
        let tr = etrait_ref_of_trait_ref tr in
        TraitRef tr
    | UnknownTrait msg -> UnknownTrait msg
end

module K = Krml.Ast

module L = Logging


(** Environment *)

type env = {
  (* Lookup functions to resolve various id's into actual declarations. *)
  get_nth_function: C.FunDeclId.id -> C.fun_decl;
  get_nth_type: C.TypeDeclId.id -> C.type_decl;
  get_nth_global: C.GlobalDeclId.id -> C.global_decl;

  (* To compute DeBruijn indices *)
  binders: (C.var_id * K.typ * C.ety) list;
  type_binders: C.type_var_id list;

  (* For printing. *)
  formatter: Charon.PrintGAst.ast_formatter option;

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

let string_of_path_elem (p: Charon.Names.path_elem): string =
  match p with
  | Charon.Names.Ident s -> s
  | Disambiguator i -> Charon.Names.Disambiguator.to_string i

let mk_field_name f i =
  match f with
  | Some f -> f
  | None -> "f" ^ string_of_int i

(** Translation of types *)

let lid_of_name (_: env) (name: Charon.Names.name): K.lident =
  let prefix, name = Krml.KList.split_at_last name in
  List.map string_of_path_elem prefix, string_of_path_elem name

let width_of_integer_type (t: Charon.PrimitiveValues.integer_type): K.width =
  match t with
  | Charon.PrimitiveValues.Isize -> failwith "TODO: Isize"
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

let rec typ_of_ty (env: env) (ty: 'region Charon.Types.ty): K.typ =
  match ty with
  | C.Adt (C.AdtId id, { types = []; const_generics = []; _ }) ->
      TQualified (lid_of_type_decl_id env id)
  | C.Adt (C.AdtId id, { types = args; const_generics = generic_args; _ }) ->
      if List.length generic_args > 0 then
        failwith "TODO: Adt/generic_args";
      TApp (lid_of_type_decl_id env id, List.map (typ_of_ty env) args)
  | C.Adt (C.Tuple, { types = args; const_generics; _ }) ->
      assert (const_generics = []);
      if args = [] then
        TUnit
      else begin
        assert (List.length args > 1);
        TTuple (List.map (typ_of_ty env) args)
      end

  | C.TypeVar id ->
      K.TBound (lookup_typ env id)
  | C.Literal Bool ->
      K.TBool
  | C.Literal Char ->
      failwith "TODO: Char"
  | C.Never ->
      failwith "Impossible: Never"
  | C.Literal (Integer k) ->
      K.TInt (width_of_integer_type k)

  | C.Adt (Assumed Array, { types = [ t ]; const_generics = [ ConstGenericValue (Scalar n) ]; _ }) ->
      K.TArray (typ_of_ty env t, constant_of_scalar_value n)

  | C.Ref (_, Adt (Assumed Vec, { types = [ t ]; _ }), _) ->
      (* We compile vecs to fat pointers, which hold the pointer underneath -- no need for an
         extra reference here. *)
      Builtin.mk_vec (typ_of_ty env t)

  | C.Adt (Assumed Vec, { types = [ t ]; _ }) ->
      Builtin.mk_vec (typ_of_ty env t)

  | C.Ref (_, Adt (Assumed Slice, { types = [ t ]; _ }), _) ->
      (* We compile slices to fat pointers, which hold the pointer underneath -- no need for an
         extra reference here. *)
      Builtin.mk_slice (typ_of_ty env t)

  | C.Ref (_, Adt (Assumed Array, { types = [ t ]; _ }), _) ->
      (* We collapse Ref(Array) into a pointer type, leveraging C's implicit decay between array
         types and pointer types. *)
      K.TBuf (typ_of_ty env t, false)

  | C.Adt (Assumed Slice, _) ->
      (* Slice values cannot be materialized since their storage space cannot be computed at
         compile-time; we should never encounter this case. *)
      assert false

  | C.Adt (Assumed Range, { types = [ t ]; _ }) ->
      Builtin.mk_range (typ_of_ty env t)

  | C.Ref (_, t, _) ->
      (* Normal reference *)
      K.TBuf (typ_of_ty env t, false)

  | C.Adt (Assumed Box, { types = [ t ]; _ }) ->
      K.TBuf (typ_of_ty env t, false)

  | C.Adt (Assumed Option, { types = [ t ]; _ }) ->
      Builtin.mk_option (typ_of_ty env t)

  | C.Adt (Assumed f, { types = args; const_generics; _ }) ->
      List.iter (fun x -> print_endline (C.show_const_generic x)) const_generics;
      Krml.Warn.fatal_error "TODO: Adt/Assumed %s (%d) %d " (C.show_assumed_ty f) (List.length args)
        (List.length const_generics)

  | C.TraitType _ ->
      Krml.Warn.fatal_error "TODO: TraitTypes"

(* Helpers: expressions *)

let mk_slice_len (t: K.typ) (e: K.expr): K.expr =
  let open K in
  let t_len = Krml.DeBruijn.subst_t t 0 Builtin.slice_len.typ in
  let hd = with_type t_len
    (ETApp (with_type Builtin.slice_len.typ (EQualified Builtin.slice_len.name), [ t ]))
  in
  with_type (TInt SizeT) (EApp (hd, [ e ]))

let mk_array_to_slice (e: K.expr) (e_start: K.expr) (e_end: K.expr): K.expr =
  let t = Krml.Helpers.assert_tbuf_or_tarray e.typ in
  let t_new = Krml.DeBruijn.subst_t t 0 Builtin.array_to_slice.typ in
  let hd = K.with_type t_new
    K.(ETApp (with_type Builtin.array_to_slice.typ (EQualified Builtin.array_to_slice.name), [ t ]))
  in
  K.with_type (Builtin.mk_slice t) (K.EApp (hd, [ e; e_start; e_end ]))

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
    | C.Deref, Ref (_, (Adt (Assumed Array, { types = [ t ]; _ }) as ty), _) ->
        (* Array is passed by reference; when appearing in a place, it'll automatically decay in C *)
        K.with_type (TBuf (typ_of_ty env t, false)) e.K.node, ty

    | C.Deref, Ref (_, (Adt (Assumed (Slice | Vec), _) as t), _) ->
        e, t

    | C.Deref, Ref (_, ty, _) ->
        Krml.Helpers.(mk_deref (Krml.Helpers.assert_tbuf_or_tarray e.K.typ) e.K.node), ty

    | DerefBox, Adt (Assumed Box, { types = [ ty ]; _ }) ->
        Krml.Helpers.(mk_deref (Krml.Helpers.assert_tbuf_or_tarray e.K.typ) e.K.node), ty

    | Field (ProjAdt (typ_id, variant_id), field_id), C.Adt (_, { types; _ }) ->
        begin match variant_id with
        | None ->
            let { C.kind; _ } = env.get_nth_type typ_id in
            let fields = match kind with Struct fields -> fields | _ -> failwith "not a struct" in
            let field_name, field_ty =
              let field = List.nth fields (C.FieldId.to_int field_id) in
              match field.C.field_name with
              | Some field_name -> field_name, C.tsubst types (C.ety_of_typ field.C.field_ty)
              | None -> failwith "TODO: understand what empty field name means"
            in
            K.with_type (typ_of_ty env field_ty) (K.EField (e, field_name)),
            field_ty
        | Some variant_id ->
            let variant = find_nth_variant env typ_id variant_id in
            let field_id = C.FieldId.to_int field_id in
            let field = List.nth variant.fields field_id in
            let field_ty = C.tsubst types (C.ety_of_typ field.C.field_ty) in
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

    | Field (ProjOption v, _), C.Adt (_, { types; _ }) ->
        assert (v = C.option_some_id);
        let field_ty = Krml.KList.one types in
        (* The type of the x field of the Some constructor in option t is t
           itself. *)
        let field_t = typ_of_ty env field_ty in
        let b = Krml.Helpers.fresh_binder "x" field_t in
        K.with_type field_t K.(EMatch (Unchecked,
          e,
          [[ b ],
          with_type e.typ (PCons ("Some", [ with_type field_t (PBound 0) ])),
          with_type field_t (EBound 0)])),
        field_ty

    | Field (ProjTuple n, i), C.Adt (_, { types = tys; const_generics = cgs; _ }) ->
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
      | C.Adt (Assumed Array, { types = [ t ]; const_generics = [ ConstGenericValue (Scalar n) ]; _ }) ->
          mk_deep_copy p (typ_of_ty env t) (constant_of_scalar_value n)
      | _ ->
          p
      end

  | Move p ->
      fst (expression_of_place env p)
  | Constant ({ value = CLiteral (Scalar sv); _ }) ->
      expression_of_scalar_value sv
  | Constant ({ value = CLiteral (Bool b); _ }) ->
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
let maybe_addrof (ty: 'a C.ty) (e: K.expr) =
  (* ty is the *original* Rust type *)
  match ty with
  | Adt (Assumed (Array | Slice | Vec), _) ->
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
      maybe_addrof ty e

  | UnaryOp (Cast (_, dst), e) ->
      let dst = K.TInt (width_of_integer_type dst) in
      K.with_type dst (K.ECast (expression_of_operand env e, dst))
  | UnaryOp (op, o1) ->
      mk_op_app (op_of_unop op) (expression_of_operand env o1) []
  | BinaryOp (op, o1, o2) ->
      mk_op_app (op_of_binop op) (expression_of_operand env o1) [ expression_of_operand env o2 ]
  | Discriminant _ ->
      failwith "expression_of_rvalue Discriminant"
  | Aggregate (AggregatedTuple, ops) ->
      let ops = List.map (expression_of_operand env) ops in
      let ts = List.map (fun x -> x.K.typ) ops in
      if ops = [] then
        K.with_type TUnit K.EUnit
      else begin
        assert (List.length ops > 1);
        K.with_type (TTuple ts) (K.ETuple ops)
      end
  | Aggregate (AggregatedOption (v, t), x) ->
      let t = typ_of_ty env t in
      if v = C.option_none_id then
        K.with_type (Builtin.mk_option t) (K.ECons ("None", []))
      else
        let x = Krml.KList.one x in
        K.with_type (Builtin.mk_option t) (K.ECons ("Some", [ expression_of_operand env x ]))
  | Aggregate (AggregatedAdt (typ_id, variant_id, { types = typ_args; _ }), args) ->
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
  | Aggregate (AggregatedRange t, ops) ->
      if t <> C.Literal (Integer Usize) then
        failwith "TODO: polymorphic ranges";
      let start_index, end_index =
        match ops with
          | [ x; y ] ->
              expression_of_operand env x, expression_of_operand env y
          | _ ->
              failwith "too many arguments to range"
      in
      K.with_type (Builtin.mk_range (TInt SizeT))
        (K.EFlat [ Some "start", start_index; Some "end", end_index ])
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

let lookup_fun (env: env) (f: C.fun_id): K.lident * int * K.typ list * K.typ =
  let builtin_of_fun_id = function
    | C.SliceLen -> Builtin.slice_len
    | SliceIndexShared | SliceIndexMut -> Builtin.slice_index
    | ArrayToSliceShared | ArrayToSliceMut -> Builtin.array_to_slice
    | ArraySubsliceShared | ArraySubsliceMut -> Builtin.array_to_subslice
    | SliceSubsliceShared | SliceSubsliceMut -> Builtin.slice_subslice
    | VecPush -> Builtin.vec_push
    | VecNew -> Builtin.vec_new
    | VecLen -> Builtin.vec_len
    | VecIndexMut | VecIndex -> Builtin.vec_index
    | BoxNew -> Builtin.box_new
    | Replace -> Builtin.replace
    | f -> Krml.Warn.fatal_error "unknown assumed function: %s" (C.show_assumed_fun_id f)
  in
  match f with
  | C.Regular f ->
      let { C.name; signature = { generics = { types = type_params; _ }; inputs; output; _ }; _ } = env.get_nth_function f in
      let env = push_type_binders env type_params in
      lid_of_name env name, List.length type_params, List.map (typ_of_ty env) inputs, typ_of_ty env output

  | Assumed f ->
      let { Builtin.name; typ; n_type_args; _ } = builtin_of_fun_id f in
      let ret, args = Krml.Helpers.flatten_arrow typ in
      name, n_type_args, args, ret

(* Runs after the adjustment above *)
let args_of_const_generic_args (f: C.fun_id) (const_generic_args: C.const_generic list): K.expr list =
  match f, const_generic_args with
  | Assumed (ArrayToSliceShared | ArrayToSliceMut), [ ConstGenericValue (Scalar n) ] ->
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
      let p, ty = expression_of_place env p in
      begin match ty with
      (* doesn't do the right thing yet, need to understand why there are
         several drops per variable *)
      | C.Adt (Assumed Vec, _) when false ->
          (* p is a vec t *)
          let t = match p.typ with TApp ((["Eurydice"], "vec"), [ t ]) -> t | _ -> assert false in
          Krml.Helpers.(with_unit K.(EApp (
            with_type (Krml.DeBruijn.subst_tn [ t ] Builtin.vec_drop.typ) (ETApp (
              with_type Builtin.vec_drop.typ (EQualified Builtin.vec_drop.name),
              [ t ])),
            [ p ])))
      | _ ->
          Krml.Helpers.eunit
      end
  | Assert a ->
      expression_of_assertion env a

  | Call { func = FunId (Assumed ArrayRepeat); generics = { types = [ ty ]; const_generics = [ c ]; _ }; args = [ e ]; dest; _ } ->
      let e = expression_of_operand env e in
      let t = typ_of_ty env ty in
      let dest, _ = expression_of_place env dest in
      let n = match c with
        | ConstGenericValue (Scalar n) -> n
        | _ -> failwith "unexpected const generic for ArrayRepeat" in
      let c = constant_of_scalar_value n in
      let n = Z.to_int n.value in
      Krml.Helpers.with_unit K.(
        EAssign (dest, (with_type (TArray (t, c)) (EBufCreateL (Stack, List.init n (fun _ -> e))))))

  | Call { func = FunId (Assumed (ArrayIndexShared | ArrayIndexMut)); generics = { types = [ ty ]; _ }; args = [ e1; e2 ]; dest; _ } ->
      let e1 = expression_of_operand env e1 in
      let e2 = expression_of_operand env e2 in
      let t = typ_of_ty env ty in
      let dest, _ = expression_of_place env dest in
      Krml.Helpers.with_unit K.(EAssign (dest, maybe_addrof ty (with_type t (EBufRead (e1, e2)))))

  | Call { func; generics = { types = type_args; const_generics = const_generic_args; _ }; args; dest; _ } ->
      let func = match func with FunId func -> func | _ -> Krml.Warn.fatal_error "TODO: Call/not(FuncId)" in
      let dest, _ = expression_of_place env dest in
      let args = List.map (expression_of_operand env) args in
      let original_type_args = type_args in
      let type_args = List.map (typ_of_ty env) type_args in
      let args = args @ args_of_const_generic_args func const_generic_args in
      let args = if args = [] then [ Krml.Helpers.eunit ] else args in
      let name, n_type_params, inputs, output = lookup_fun env func in
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
        | C.Assumed (SliceIndexShared | SliceIndexMut), [ Adt (Assumed (Array | Slice | Vec), _) ] ->
            (* Will decay. See comment above maybe_addrof *)
            rhs
        | C.Assumed (SliceIndexShared | SliceIndexMut), _ ->
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
        | Adt (AdtId typ_id, _) ->
            let { C.kind; _ } = env.get_nth_type typ_id in
            let variants = match kind with Enum variants -> variants | _ -> assert false in
            fun v ->
              let v = C.VariantId.nth variants v in
              v.variant_name, List.length v.fields
        | Adt (Assumed Option, _) ->
            fun x ->
              if x = C.option_none_id then
                "None", 0
              else if x = C.option_some_id then
                "Some", 1
              else
                failwith "unknown variant id for option"
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

(* Top-level declaration formatters *)
type formatters = {
  mk_formatter: C.fun_decl -> Charon.PrintGAst.ast_formatter;
  type_ctx: C.type_decl C.TypeDeclId.Map.t;
  global_ctx: C.global_decl C.GlobalDeclId.Map.t;
  trait_ctx: C.trait_decl C.TraitDeclId.Map.t;
  trait_impl_ctx: C.trait_impl C.TraitImplId.Map.t;
}

let of_declaration_group (dg: 'id C.g_declaration_group) (f: 'id -> 'a): 'a list =
  (* We do not care about recursion as in C, everything is mutually recursive
     thanks to header inclusion. *)
  match dg with
  | NonRec id -> [ f id ]
  | Rec ids -> List.map f ids

let decls_of_declarations (env: env) (formatters: formatters) (d: C.declaration_group): K.decl option list =
  match d with
  | C.Type dg ->
      of_declaration_group dg (fun (id: C.TypeDeclId.id) ->
        let decl = env.get_nth_type id in
        let { C.name; def_id; kind; generics = { types = type_params; _ }; _ } = decl in
        L.log "AstOfLlbc" "Visiting type: %s\n%s" (Charon.Names.name_to_string name)
          (Charon.PrintLlbcAst.Crate.type_decl_to_string formatters.type_ctx formatters.global_ctx formatters.trait_ctx formatters.trait_impl_ctx decl);

        assert (def_id = id);
        let name = lid_of_name env name in

        match kind with
        | Opaque -> None
        | Struct fields ->
            let env = push_type_binders env type_params in
            let fields = List.map (fun { C.field_name; field_ty; _ } ->
              field_name, (typ_of_ty env field_ty, true)
            ) fields in
            Some (K.DType (name, [], List.length type_params, Flat fields))
        | Enum branches ->
            let env = push_type_binders env type_params in
            let branches = List.map (fun { C.variant_name; fields; _ } ->
              variant_name, List.mapi (fun i { C.field_name; field_ty; _ } ->
                mk_field_name field_name i,
                (typ_of_ty env field_ty, true)
              ) fields
            ) branches in
            Some (K.DType (name, [], List.length type_params, Variant branches))
      )
  | C.Fun dg ->
      of_declaration_group dg (fun (id: C.FunDeclId.id) ->
        let decl = env.get_nth_function id in
        let formatter = formatters.mk_formatter decl in
        let env = { env with formatter = Some formatter } in
        let { C.def_id; name; signature; body; is_global_decl_body; _ } = decl in
        L.log "AstOfLlbc" "Visiting function: %s\n%s" (Charon.Names.name_to_string name)
          (Charon.PrintLlbcAst.Ast.fun_decl_to_string formatter "  " "  " decl);

        assert (def_id = id);
        match body with
        | None ->
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
            Some (K.DExternal (None, [], List.length type_params, name, t, name_hints))
        | Some { arg_count; locals; body; _ } ->
            (* try *)
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
              Some (K.DFunction (None, [], List.length signature.C.generics.types, return_type, name, arg_binders, body))
            (* with _ -> None *)
      )
  | C.Global id ->
      let global = env.get_nth_global id in
      let { C.name; ty; body_id; _ } = global in
      let def = env.get_nth_function body_id in
        let formatter = formatters.mk_formatter def in
      L.log "AstOfLlbc" "Visiting global:%s\n%s" (Charon.Names.name_to_string name)
          (Charon.PrintLlbcAst.Ast.fun_decl_to_string formatter "  " "  " def);
      assert (def.signature.inputs = []);
      assert (def.signature.generics.types = []);
      assert (def.signature.generics.const_generics = []);
      let ty = typ_of_ty env ty in
      begin match def.body with
      | Some body ->
          let ret_var = Krml.KList.one body.locals in
          let body =
            with_locals env ty [ ret_var ] (fun env ->
              expression_of_raw_statement env ret_var.index body.body.content)
          in
          [ Some (K.DGlobal ([], lid_of_name env name, 0, ty, body)) ]
      | None ->
          [ Some (K.DExternal (None, [], 0, lid_of_name env name, ty, [])) ]
      end

  | C.TraitDecl _ ->
      failwith "TODO: C.TraitDecl"
  | C.TraitImpl _ ->
      failwith "TODO: C.TraitImpl"

let file_of_crate (crate: Charon.LlbcAst.crate): Krml.Ast.file =
  let { C.name; declarations; types; functions; globals; trait_impls; trait_decls } = crate in
  let get_nth_function = fun id -> C.FunDeclId.Map.find id functions in
  let get_nth_type = fun id -> C.TypeDeclId.Map.find id types in
  let get_nth_global = fun id -> C.GlobalDeclId.Map.find id globals in
  let formatters: formatters =
    let type_ctx = types in
    let global_ctx = globals in
    let trait_ctx = trait_decls in
    let trait_impl_ctx = trait_impls in
    let mk_formatter (decl: C.fun_decl) =
      Charon.PrintLlbcAst.Crate.decl_ctx_and_fun_decl_to_ast_formatter type_ctx functions global_ctx trait_ctx trait_impl_ctx decl
    in
    { mk_formatter; type_ctx; global_ctx; trait_ctx; trait_impl_ctx }
  in
  let env = {
    get_nth_function;
    get_nth_type;
    get_nth_global;
    binders = [];
    type_binders = [];
    formatter = None;
    crate_name = name;
  } in
  name, Krml.KList.filter_some (List.concat_map (decls_of_declarations env formatters) declarations)
