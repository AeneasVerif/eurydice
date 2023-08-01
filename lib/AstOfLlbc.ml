(* C for Charon *)
module C = struct
  include Charon.GAst
  include Charon.LlbcAst
  include Charon.Types
  include Charon.Expressions
  include Charon.PrimitiveValues
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
  | C.Adt (C.AdtId id, _, []) ->
      TQualified (lid_of_type_decl_id env id)
  | C.Adt (C.AdtId id, _, args) ->
      TApp (lid_of_type_decl_id env id, List.map (typ_of_ty env) args)
  | C.Adt (C.Tuple, _, args) ->
      if args = [] then
        TUnit
      else begin
        assert (List.length args > 1);
        TTuple (List.map (typ_of_ty env) args)
      end
  | C.Adt (C.Assumed _, _, _args) ->
      failwith "TODO: Adt/Assumed"

  | C.TypeVar id ->
      K.TBound (lookup_typ env id)
  | C.Bool ->
      K.TBool
  | C.Char ->
      failwith "TODO: Char"
  | C.Never ->
      failwith "Impossible: Never"
  | C.Integer k ->
      K.TInt (width_of_integer_type k)
  | C.Str ->
      failwith "TODO: Str"
  | C.Array (t, n) ->
      K.TArray (typ_of_ty env t, constant_of_scalar_value n)
  | C.Ref (_, C.Slice t, _) ->
      (* We compile slices to fat pointers, which hold the pointer underneath -- no need for an
         extra reference here. *)
      Builtin.mk_slice (typ_of_ty env t)
  | C.Ref (_, C.Array (t, _), _) ->
      (* We collapse Ref(Array) into a pointer type, leveraging C's implicit decay between array
         types and pointer types. *)
      K.TBuf (typ_of_ty env t, false)
  | C.Slice _ ->
      (* Slice values cannot be materialized since their storage space cannot be computed at
         compile-time; we should never encounter this case. *)
      assert false
  | C.Ref (_, t, _) ->
      (* Normal reference *)
      K.TBuf (typ_of_ty env t, false)

(* Helpers: expressions *)

let mk_slice_len (t: K.typ) (e: K.expr): K.expr =
  let open K in
  let t_len = Krml.DeBruijn.subst_t t 0 Builtin.slice_len_t in
  let hd = with_type t_len
    (ETApp (with_type Builtin.slice_len_t (EQualified Builtin.slice_len), [ t ]))
  in
  with_type (TInt SizeT) (EApp (hd, [ e ]))

let mk_slice_new (e: K.expr) (e_start: K.expr) (e_end: K.expr): K.expr =
  let t = Krml.Helpers.assert_tbuf_or_tarray e.typ in
  let t_new = Krml.DeBruijn.subst_t t 0 Builtin.slice_new_t in
  let hd = K.with_type t_new
    K.(ETApp (with_type Builtin.slice_new_t (EQualified Builtin.slice_new), [ t ]))
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
    | C.Deref, Ref (_, (Array (t, _) as ty), _) ->
        (* Array is passed by reference; when appearing in a place, it'll automatically decay in C *)
        K.with_type (TBuf (typ_of_ty env t, false)) e.K.node, ty
    | C.Deref, Ref (_, (Slice _ as t), _) ->
        e, t
    | C.Deref, Ref (_, ty, _) ->
        Krml.Helpers.(mk_deref (Krml.Helpers.assert_tbuf_or_tarray e.K.typ) e.K.node), ty
    | DerefBox, _ ->
        failwith "expression_of_place DerefBox"
    | Field (ProjAdt _, _), _ ->
        failwith "expression_of_place ProjAdt"
    | Field (ProjOption _, _), _ ->
        failwith "expression_of_place ProjOption"
    | Field (ProjTuple n, i), C.Adt (_, _, tys) ->
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
        K.with_type t_i (K.EMatch (e, [ binders, pattern, expr ])), List.nth tys i
    | Offset ofs_id, (Ref (_, ty, _) | Array (ty, _)) ->
        K.(with_type (typ_of_ty env ty) (EBufRead (e, expression_of_var_id env ofs_id))), ty
    | Offset ofs_id, (Slice ty) ->
        let slice_index = K.with_type Builtin.slice_index_t (K.EQualified Builtin.slice_index) in
        let t = typ_of_ty env ty in
        let slice_index = K.with_type (Krml.DeBruijn.subst_tn [ t ] Builtin.slice_index_t)
          (K.ETApp (slice_index, [ t ]))
        in
        K.(with_type t (EApp (slice_index, [ e; expression_of_var_id env ofs_id ]))), ty
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
      | C.Array (t, n) ->
          mk_deep_copy p (typ_of_ty env t) (constant_of_scalar_value n)
      | _ ->
          p
      end

  | Move p ->
      fst (expression_of_place env p)
  | Constant (_ty, Scalar sv) ->
      expression_of_scalar_value sv
  | Constant (_ty, Bool b) ->
      K.(with_type TBool (EBool b))
  | Constant (_ty, Char _) ->
      failwith "expression_of_operand Char"
  | Constant (_ty, String _) ->
      failwith "expression_of_operand String"

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

let find_nth_variant (env: env) (typ: C.type_decl_id) (var: C.variant_id) =
  match env.get_nth_type typ with
  | { kind = Enum variants; _ } ->
      Charon.Types.VariantId.nth variants var
  | _ ->
      failwith "impossible: type is not a variant"

let expression_of_rvalue (env: env) (p: C.rvalue): K.expr =
  match p with
  | Use op ->
      expression_of_operand env op
  | Ref (p, _) ->
      let e, ty = expression_of_place env p in
      (* Arrays and ref to arrays are compiled as pointers in C; we allow on implicit array decay to
         pass one for the other *)
      begin match ty with
      | Array _ | Slice _ ->
          e
      | _ ->
          K.(with_type (TBuf (e.typ, false)) (EAddrOf e))
      end

  | UnaryOp (Cast (_, _), _e) ->
      failwith "TODO: UnaryOp Cast"
  | UnaryOp (SliceNew l, e) ->
      let e = expression_of_operand env e in
      mk_slice_new e (Krml.Helpers.zero SizeT) (expression_of_scalar_value l)
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
  | Aggregate (AggregatedOption (_, _), _) ->
      failwith "expression_of_rvalue AggregatedOption"
  | Aggregate (AggregatedAdt (typ_id, variant_id, _, typ_args), args) ->
      let { C.name; _ } = env.get_nth_type typ_id in
      let typ_lid = lid_of_name env name in
      let typ_args = List.map (typ_of_ty env) typ_args in
      let t = if typ_args = [] then K.TQualified typ_lid else TApp (typ_lid, typ_args) in
      let args = List.map (expression_of_operand env) args in
      begin match variant_id with
      | Some variant_id ->
          let variant_id = (find_nth_variant env typ_id variant_id).variant_name in
          K.with_type t (K.ECons (variant_id, args))
      | None ->
          failwith "TODO: AggregatedAdt None"
      end
  | Aggregate (AggregatedRange t, ops) ->
      if t <> C.Integer Usize then
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
  | Aggregate (AggregatedArray t, ops) ->
      K.with_type (typ_of_ty env t) (K.EBufCreateL (Stack, List.map (expression_of_operand env) ops))
  | Global _ ->
      failwith "expression_of_rvalue Global"
  | Len p ->
      (* This will go away once proper treatment of lengths lands in Charon *)
      let e, ty = expression_of_place env p in
      match ty with
      | Array (_, n) ->
          expression_of_scalar_value n
      | Slice ty ->
          mk_slice_len (typ_of_ty env ty) e
      | _ ->
          Krml.Warn.fatal_error "Unknown Len overload: %a" Krml.PrintAst.Ops.ptyp e.typ

let expression_of_assertion (env: env) ({ cond; expected }: C.assertion): K.expr =
  let cond =
    if not expected then
      expression_of_operand env cond
    else
      Krml.Helpers.mk_not (expression_of_operand env cond)
  in
  K.(with_type TAny (
    EIfThenElse (cond,
      with_type TAny (EAbort (Some "assert failure")),
      Krml.Helpers.eunit)))

let lookup_fun (env: env) (f: C.fun_id): K.lident * int * K.typ list * K.typ =
  match f with
  | C.Regular f ->
      let { C.name; signature = { type_params; inputs; output; _ }; _ } = env.get_nth_function f in
      let env = push_type_binders env type_params in
      lid_of_name env name, List.length type_params, List.map (typ_of_ty env) inputs, typ_of_ty env output
  | Assumed Replace ->
      failwith "lookup_fun Replace"
  | Assumed BoxNew ->
      failwith "lookup_fun BoxNew"
  | Assumed BoxDeref ->
      failwith "lookup_fun BoxDeref"
  | Assumed BoxDerefMut ->
      failwith "lookup_fun BoxDerefMut"
  | Assumed BoxFree ->
      failwith "lookup_fun BoxFree"
  | Assumed VecNew ->
      failwith "lookup_fun VecNew"
  | Assumed VecPush ->
      failwith "lookup_fun VecPush"
  | Assumed VecInsert ->
      failwith "lookup_fun VecInsert"
  | Assumed VecLen ->
      failwith "lookup_fun VecLen"
  | Assumed VecIndex ->
      failwith "lookup_fun VecIndex"
  | Assumed VecIndexMut ->
      failwith "lookup_fun VecIndexMut"
  | Assumed ArraySlice
  | Assumed ArraySliceMut ->
      (* we intentionally give a type here that is too general... the idea is that krml's
         type-checker cannot represent dependencies, or type-level functions, so we can write
         neither:
           forall a n. (x: [a; n], range) -> slice a
         or
           forall a. (x: a, range) -> slice (decay a)
         so instead we do something that is too general, but will eventually work out:
           forall a b. (x: b*, range) -> slice b
         where all uses pick a = [ t; n ] and b = t.
         An additional subtletly is that the rvalue passed to the function decays automatically into
         a pointer type so really the only purpose of the first type argument is to retain the
         length that will eventually be emitted in the code-gen. *)
      let name = Builtin.slice_new_with_range in
      let ret, args = Krml.Helpers.flatten_arrow Builtin.slice_new_with_range_t in
      name, List.length args, args, ret

(* See explanation above *)
let adjust_type_args (f: C.fun_id) (type_args: K.typ list): K.typ list =
  match f with
  | Assumed (ArraySlice | ArraySliceMut) ->
      let t = match type_args with [ TArray (t, _) ] -> t | _ -> failwith "ill-typed slice_new_with_range" in
      type_args @ [ t ]
  | _ ->
      type_args

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
  | FakeRead _
  | Drop _ ->
      Krml.Helpers.eunit
  | Assert a ->
      expression_of_assertion env a
  | Call { func; type_args; args; dest; _ } ->
      let dest, _ = expression_of_place env dest in
      let args = List.map (expression_of_operand env) args in
      let type_args = List.map (typ_of_ty env) type_args in
      let type_args = adjust_type_args func type_args in
      let name, n_type_params, inputs, output = lookup_fun env func in
      assert (n_type_params = List.length type_args);
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
      Krml.Helpers.with_unit K.(EAssign (dest, with_type output (EApp (hd, args))))
  | Panic ->
      with_any (K.EAbort None)
  | Return ->
      let e = expression_of_var_id env ret_var in
      K.(with_type TUnit (EReturn e))
  | Break _ ->
      K.(with_type TUnit EBreak)
  | Continue _ ->
      K.(with_type TUnit EContinue)
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
  | Switch (Match (_, _, _)) ->
      failwith "expression_of_raw_statement Match"
  | Loop s ->
      K.(with_type TUnit (EWhile (Krml.Helpers.etrue,
        expression_of_raw_statement env ret_var s.content)))


(** Top-level declarations: orchestration *)

(* Top-level declaration formatters *)
type formatters = {
  mk_formatter: C.fun_decl -> Charon.PrintGAst.ast_formatter;
  type_ctx: C.type_decl C.TypeDeclId.Map.t
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
        let { C.name; def_id; kind; type_params; _ } = decl in
        L.log "AstOfLlbc" "Visiting type: %s\n%s" (Charon.Names.name_to_string name)
          (Charon.PrintLlbcAst.Crate.type_decl_to_string formatters.type_ctx decl);

        assert (def_id = id);
        let name = lid_of_name env name in

        match kind with
        | Opaque -> None
        | Struct _ -> failwith "TODO: C.Type Struct"
        | Enum branches ->
            let env = push_type_binders env type_params in
            let branches = List.map (fun { C.variant_name; fields; _ } ->
              variant_name, List.mapi (fun i { C.field_name; field_ty; _ } ->
                Option.value ~default:("f" ^ string_of_int i) field_name,
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
            let { C.type_params; inputs; output ; _ } = signature in
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
              let env = push_type_binders env signature.C.type_params in
              let name = lid_of_name env name in
              (* `locals` contains, in order: special return variable; function arguments;
                 local variables *)
              let args, locals = Krml.KList.split (arg_count + 1) locals in
              let return_var = List.hd args in
              let args = List.tl args in

              let return_type = typ_of_ty env return_var.var_ty in
              let arg_binders = List.map (fun (arg: C.var) ->
                let name = Option.value ~default:"_" arg.name in
                Krml.Helpers.fresh_binder name (typ_of_ty env arg.var_ty)
              ) args in
              let env = push_binders env args in
              let body =
                with_locals env return_type (return_var :: locals) (fun env ->
                  expression_of_raw_statement env return_var.index body.content)
              in
              Some (K.DFunction (None, [], List.length signature.C.type_params, return_type, name, arg_binders, body))
            (* with _ -> None *)
      )
  | C.Global _id ->
      failwith "TODO: C.Global"

let file_of_crate (crate: Charon.LlbcAst.crate): Krml.Ast.file =
  let { C.name; declarations; types; functions; globals } = crate in
  let get_nth (type id) (type b) (to_int: id -> int) (things: b list) (id: id) =
    (* TODO: if we're spending too much time here, use a hash table or something *)
    List.nth things (to_int id)
  in
  let get_nth_function = get_nth C.FunDeclId.to_int functions in
  let get_nth_type = get_nth C.TypeDeclId.to_int types in
  let get_nth_global = get_nth C.GlobalDeclId.to_int globals in
  let formatters: formatters =
    let type_ctx, f, g = Charon.LlbcAstUtils.compute_defs_maps crate in
    let mk_formatter (decl: C.fun_decl) =
      Charon.PrintLlbcAst.Crate.decl_ctx_and_fun_decl_to_ast_formatter type_ctx f g decl
    in
    { mk_formatter; type_ctx }
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
