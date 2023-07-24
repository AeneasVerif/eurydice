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
  binders: (C.var_id * K.typ) list;
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


(** Small helpers *)

let builtin_slice: K.lident = ["Eurydice"], "slice"

let builtin_slice_len: K.lident = ["Eurydice"], "slice_len"

let with_any = K.(with_type TAny)

let mk_slice (t: K.typ): K.typ =
  K.TApp (builtin_slice, [ t ])

let mk_slice_len (t: K.typ) (e: K.expr): K.expr =
  let hd = K.with_type (TArrow (mk_slice t, TInt SizeT))
    (K.ETApp (with_any (K.EQualified builtin_slice_len), [ t ]))
  in
  K.with_type (TInt SizeT) (K.EApp (hd, [ e ]))

let assert_slice (t: K.typ) =
  match t with
  | TApp (lid, [ t ]) when lid = builtin_slice ->
      t
  | _ ->
      Krml.Warn.fatal_error "Not a slice: %a" Krml.PrintAst.Ops.ptyp t

let string_of_path_elem (p: Charon.Names.path_elem): string =
  match p with
  | Charon.Names.Ident s -> s
  | Disambiguator i -> Charon.Names.Disambiguator.to_string i


(** Translation of types *)

let lid_of_name (env: env) (name: Charon.Names.name): K.lident =
  [ env.crate_name ], String.concat "_" (List.map string_of_path_elem name)

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
      TTuple (List.map (typ_of_ty env) args)
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
  | C.Slice t ->
      K.TApp (builtin_slice, [ typ_of_ty env t ])
  | C.Ref (_, t, _) ->
      K.TBuf (typ_of_ty env t, false)

(* Environment: expressions *)

let lookup env (v1: C.var_id) =
  let exception Found of int * K.typ in
  try
    List.iteri (fun i v2 ->
      if v1 = fst v2 then
        raise (Found (i, snd v2))
    ) env.binders;
    raise Not_found
  with Found (i, t) ->
    i, t

let push_binder env (t: C.var) =
  { env with binders = (t.index, typ_of_ty env t.var_ty) :: env.binders }

let push_binders env (ts: C.var list) =
  List.fold_left push_binder env ts


(** Translation of expressions (statements, operands, rvalues, places) *)

let binder_of_var (env: env) (l: C.var): K.binder =
  let name = Option.value ~default:"x" l.name in
  Krml.Helpers.fresh_binder name (typ_of_ty env l.var_ty)

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

let expression_of_place (env: env) (p: C.place): K.expr =
  let { C.var_id; projection } = p in
  let e = expression_of_var_id env var_id in
  List.fold_right (fun pe e ->
    match pe with
    | C.Deref | C.DerefBox ->
        Krml.Helpers.(mk_deref (assert_tbuf e.K.typ) e.K.node)
    | Field _ ->
        failwith "expression_of_place Field"
    | Offset ofs_id ->
        let t = Krml.Helpers.assert_tbuf e.typ in
        K.(with_type (TBuf (t, false)) (EBufSub (e, expression_of_var_id env ofs_id)))
  ) projection e

let expression_of_scalar_value ({ C.int_ty; _ } as sv) =
  let w = width_of_integer_type int_ty in
  K.(with_type (TInt w) (EConstant (constant_of_scalar_value sv)))

let expression_of_operand (env: env) (p: C.operand): K.expr =
  match p with
  | Copy p
  | Move p ->
      expression_of_place env p
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

let expression_of_rvalue (env: env) (p: C.rvalue): K.expr =
  match p with
  | Use op ->
      expression_of_operand env op
  | Ref (p, _) ->
      let p = expression_of_place env p in
      K.(with_type (TBuf (p.typ, false)) (EAddrOf p))
  | UnaryOp (Cast (_, _), _e) ->
      failwith "TODO: UnaryOp Cast"
  | UnaryOp (SliceNew _, _e) ->
      failwith "TODO: UnaryOp SliceNew"
  | UnaryOp (op, o1) ->
      mk_op_app (op_of_unop op) (expression_of_operand env o1) []
  | BinaryOp (op, o1, o2) ->
      mk_op_app (op_of_binop op) (expression_of_operand env o1) [ expression_of_operand env o2 ]
  | Discriminant _ ->
      failwith "expression_of_rvalue Discriminant"
  | Aggregate (AggregatedTuple, ops) ->
      let ops = List.map (expression_of_operand env) ops in
      let ts = List.map (fun x -> x.K.typ) ops in
      K.with_type (TTuple ts) (K.ETuple ops)
  | Aggregate (AggregatedOption (_, _), _) ->
      failwith "expression_of_rvalue AggregatedOption"
  | Aggregate (AggregatedAdt (_, _, _, _), _) ->
      failwith "expression_of_rvalue AggregatedAdt"
  | Aggregate (AggregatedRange _, _) ->
      failwith "expression_of_rvalue AggregatedRange"
  | Aggregate (AggregatedArray _, _) ->
      failwith "expression_of_rvalue AggregateArray"
  | Global _ ->
      failwith "expression_of_rvalue Global"
  | Len { var_id; projection } ->
      (* Length of an array, can be inferred from context *)
      if projection <> [ Deref ] then
        failwith "TODO: Len / non-deref case";
      let _, t = lookup env var_id in
      match t with
      | TArray (_, l) ->
          K.(with_type (TInt (fst l)) (EConstant l))
      | t ->
          Krml.Warn.fatal_error "Not an array: %a" Krml.PrintAst.Ops.ptyp t

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

let lookup_fun (env: env) (f: C.fun_id) =
  match f with
  | C.Regular f ->
      let { C.name; signature = { type_params; inputs; output; _ }; _ } = env.get_nth_function f in
      name, type_params, inputs, output
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
  | Assumed ArraySlice ->
      (* forall a. slice<a> slice_of_array(x: &[a]); *)
      failwith "lookup_fun ArraySlice"

let rec expression_of_raw_statement (env: env) (ret_var: C.var_id) (s: C.raw_statement): K.expr =
  match s with
  | Assign (p, rv) ->
      let p = expression_of_place env p in
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
      let dest = expression_of_place env dest in
      let args = List.map (expression_of_operand env) args in
      let type_args = List.map (typ_of_ty env) type_args in
      let name, type_params, inputs, output = lookup_fun env func in
      assert (List.length type_params = List.length type_args);
      let output, t =
        let env = push_type_binders env type_params in
        let output = typ_of_ty env output in
        let t = Krml.Helpers.fold_arrow (List.map (typ_of_ty env) inputs) output in
        Krml.DeBruijn.subst_tn type_args output, Krml.DeBruijn.subst_tn type_args t
      in
      let hd =
        let hd = with_any (EQualified (lid_of_name env name)) in
        if type_args <> [] then
          K.with_type t (K.ETApp (hd, type_args))
        else
          hd
      in
      Krml.Helpers.with_unit K.(EAssign (dest, with_type output (EApp (hd, args))))
  | Panic ->
      with_any (EAbort None)
  | Return ->
      let e = expression_of_var_id env ret_var in
      K.(with_type e.typ (EReturn e))
  | Break _ ->
      with_any EBreak
  | Continue _ ->
      with_any EContinue
  | Nop ->
      Krml.Helpers.eunit
  | Sequence (s1, s2) ->
      with_any (ESequence [
        expression_of_raw_statement env ret_var s1.content;
        expression_of_raw_statement env ret_var s2.content])
  | Switch (If (op, s1, s2)) ->
      with_any (EIfThenElse (expression_of_operand env op,
        expression_of_raw_statement env ret_var s1.content,
        expression_of_raw_statement env ret_var s2.content))
  | Switch (SwitchInt (_, _, _, _)) ->
      failwith "expression_of_raw_statement SwitchInt"
  | Switch (Match (_, _, _)) ->
      failwith "expression_of_raw_statement Match"
  | Loop s ->
      with_any (EWhile (Krml.Helpers.etrue,
        expression_of_raw_statement env ret_var s.content))


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
        let { C.name; def_id; kind; _ } = decl in
        L.log "AstOfLlbc" "Visting type: %s\n%s" (Charon.Names.name_to_string name)
          (Charon.PrintLlbcAst.Crate.type_decl_to_string formatters.type_ctx decl);

        assert (def_id = id);
        match kind with
        | Opaque -> None
        | Struct _ -> failwith "TODO: C.Type Struct"
        | Enum _ -> failwith "TODO: C.Type Enum"
      )
  | C.Fun dg ->
      of_declaration_group dg (fun (id: C.FunDeclId.id) ->
        let decl = env.get_nth_function id in
        let formatter = formatters.mk_formatter decl in
        let env = { env with formatter = Some formatter } in
        let { C.def_id; name; signature; body; is_global_decl_body; _ } = decl in
        L.log "AstOfLlbc" "Visting function: %s\n%s" (Charon.Names.name_to_string name)
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
