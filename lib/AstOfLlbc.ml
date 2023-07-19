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


(** Environments *)

type env = {
  (* Lookup functions to resolve various id's into actual declarations. *)
  get_nth_function: C.FunDeclId.id -> C.fun_decl;
  get_nth_type: C.TypeDeclId.id -> C.type_decl;
  get_nth_global: C.GlobalDeclId.id -> C.global_decl;

  (* Current DeBruijn index. *)
  binders: C.var_id list;
  type_binders: C.type_var_id list;

  (* For printing. *)
  formatter: Charon.PrintGAst.ast_formatter option;

  (* For picking pretty names *)
  crate_name: string;
}

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

let lookup env (v1: C.var_id) =
  let exception Found of int in
  try
    List.iteri (fun i v2 ->
      if v1 = v2 then
        raise (Found i)
    ) env.binders;
    raise Not_found
  with Found i ->
    i

let push_binder env (t: C.var) =
  { env with binders = t.index :: env.binders }

let push_binders env (ts: C.var list) =
  List.fold_left push_binder env ts


(** Small helpers *)

let builtin_slice: K.lident = ["Eurydice"], "slice"

let with_any = K.(with_type TAny)

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
  | C.Array (t, _n) ->
      K.TBuf (typ_of_ty env t, false)
  | C.Slice t ->
      K.TApp (builtin_slice, [ typ_of_ty env t ])
  | C.Ref (_, t, _) ->
      K.TBuf (typ_of_ty env t, false)


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

let expression_of_place (env: env) (p: C.place): K.expr =
  let { C.var_id; projection } = p in
  let e = with_any K.(EBound (lookup env var_id)) in
  List.fold_right (fun pe e ->
    match pe with
    | C.Deref | C.DerefBox ->
        Krml.Helpers.mk_deref K.TAny e.K.node
    | Field _ ->
        failwith "expression_of_place Field"
    | Offset ofs_id ->
        with_any K.(EBufSub (e, with_type (TInt SizeT) (EBound (lookup env ofs_id))))
  ) projection e

let constant_of_scalar_value { C.value; int_ty } =
  let w = width_of_integer_type int_ty in
  K.(with_type (TInt w) (EConstant (w, Z.to_string value)))

let expression_of_operand (env: env) (p: C.operand): K.expr =
  match p with
  | Copy p
  | Move p ->
      expression_of_place env p
  | Constant (_ty, Scalar sv) ->
      constant_of_scalar_value sv
  | Constant (_ty, Bool b) ->
      K.(with_type TBool (EBool b))
  | Constant (_ty, Char _) ->
      failwith "expression_of_operand Char"
  | Constant (_ty, String _) ->
      failwith "expression_of_operand String"

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

let expression_of_rvalue (env: env) (p: C.rvalue): K.expr =
  match p with
  | Use op ->
      expression_of_operand env op
  | Ref (p, _) ->
      with_any (EAddrOf (expression_of_place env p))
  | UnaryOp (_, _) ->
      failwith "expression_of_rvalue UnaryOp"
  | BinaryOp (op, e1, e2) ->
      with_any (EApp (with_any (EOp (op_of_binop op)), [
        expression_of_rvalue e1;
        expression_of_rvalue e2]))
  | Discriminant _ ->
      failwith "expression_of_rvalue Discriminant"
  | Aggregate (AggregatedTuple, ops) ->
      with_any (ETuple (List.map (expression_of_operand env) ops))
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
  | Len _ ->
      failwith "expression_of_rvalue Len"

let rec expression_of_raw_statement (env: env) (ret_var: C.var_id) (ret_typ: K.typ) (s: C.raw_statement): K.expr =
  match s with
  | Assign (p, rv) ->
      let p = expression_of_place env p in
      let rv = expression_of_rvalue env rv in
      K.with_unit (EAssign (p, rv))
  | FakeRead _ ->
      failwith "C.FakeRead"
  | SetDiscriminant (_, _) ->
      failwith "C.SetDiscriminant"
  | Drop _ ->
      Krml.Helpers.eunit
  | Assert _ ->
      failwith "C.Assert"
  | Call _ ->
      failwith "C.Call"
  | Panic ->
      with_any (EAbort None)
  | Return ->
      K.with_type ret_typ K.(EReturn (with_type ret_typ (EBound (lookup env ret_var))))
  | Break _ ->
      with_any EBreak
  | Continue _ ->
      with_any EContinue
  | Nop ->
      failwith "C.Nop"
  | Sequence (s1, s2) ->
      with_any (ESequence [
        expression_of_raw_statement env ret_var ret_typ s1.content;
        expression_of_raw_statement env ret_var ret_typ s2.content])
  | Switch (If (op, s1, s2)) ->
      with_any (EIfThenElse (expression_of_operand env op,
        expression_of_raw_statement env ret_var ret_typ s1.content,
        expression_of_raw_statement env ret_var ret_typ s2.content))
  | Switch (SwitchInt (_, _, _, _)) ->
      failwith "expression_of_raw_statement SwitchInt"
  | Switch (Match (_, _, _)) ->
      failwith "expression_of_raw_statement Match"
  | Loop s ->
      with_any (EWhile (Krml.Helpers.etrue,
        expression_of_raw_statement env ret_var ret_typ s.content))


(** Top-level declarations: orchestration *)

let of_declaration_group (dg: 'id C.g_declaration_group) (f: 'id -> 'a): 'a list =
  (* We do not care about recursion as in C, everything is mutually recursive
     thanks to header inclusion. *)
  match dg with
  | NonRec id -> [ f id ]
  | Rec ids -> List.map f ids

let decls_of_declarations (env: env) (formatter: C.fun_decl -> Charon.PrintGAst.ast_formatter) (d: C.declaration_group): K.decl list =
  match d with
  | C.Type dg ->
      of_declaration_group dg (fun (id: C.TypeDeclId.id) ->
        let _decl = env.get_nth_type id in
        failwith "TODO: C.Type"
      )
  | C.Fun dg ->
      of_declaration_group dg (fun (id: C.FunDeclId.id) ->
        let decl = env.get_nth_function id in
        let formatter = formatter decl in
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
            K.DExternal (None, [], List.length type_params, name, t, name_hints)
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
                let name = Option.get arg.name in
                Krml.Helpers.fresh_binder name (typ_of_ty env arg.var_ty)
              ) args in
              let env = push_binders env args in
              let body =
                with_locals env return_type (return_var :: locals) (fun env ->
                  expression_of_raw_statement env return_var.index return_type body.content)
              in
              K.DFunction (None, [], List.length signature.C.type_params, return_type, name, arg_binders, body)
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
  let formatter (decl: C.fun_decl) =
    let t, f, g = Charon.LlbcAstUtils.compute_defs_maps crate in
    Charon.PrintLlbcAst.Crate.decl_ctx_and_fun_decl_to_ast_formatter t f g decl
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
  name, List.concat_map (decls_of_declarations env formatter) declarations
