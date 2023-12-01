open Krml.Ast

(* All the transformations that need to happen in order for the program to type-check as valid Low*
   *)

let expr_of_constant (w, n) =
  with_type (TInt w) (EConstant (w, n))

let flatten_sequences files =
  (object
    inherit [_] map as super

    method! visit_ESequence env es =
      let rec flatten acc e =
        match e.node with
        | ESequence [ { node = EUnit; _ }; e2 ] ->
            flatten acc e2
        | ESequence [ e1; e2 ] ->
            flatten (e1 :: acc) e2
        | ESequence _ ->
            failwith "impossible: charon generates right-nested two-element sequences"
        | _ ->
            ESequence (List.rev_append acc [ super#visit_expr env e ])
      in
      flatten [] (with_type (snd env) (ESequence es))
  end)#visit_files () files

let expand_array_copies files =
  (object
    inherit [_] map as super

    method! visit_EAssign env lhs rhs =
      match lhs, rhs.node with
      | { node = _; typ = TArray (_, n) },
        EApp ({ node = EQualified lid; _ }, [ rhs ]) when lid = Builtin.array_copy ->
          (* Krml.Helpers.mk_copy_assignment (t, n) lhs.node rhs *)
          let zero = Krml.(Helpers.zero Constant.SizeT) in
          EBufBlit (rhs, zero, lhs, zero, expr_of_constant n)
      | _ ->
          super#visit_EAssign env lhs rhs

    method! visit_EApp env hd args =
      if hd.node = EQualified Builtin.array_copy then
        Krml.Warn.fatal_error "Uncaught array copy"
      else
        super#visit_EApp env hd args
  end)#visit_files () files

(* Rust is super lenient regarding the type of shift operators, we impose u32 -- see
   https://doc.rust-lang.org/std/ops/trait.Shl.html *)
let adjust_shifts files =
  (object
    inherit [_] map as super

    method! visit_EApp env e es =
      let open Krml in
      match e.node, es with
      | EOp ((BShiftL | BShiftR), _), [ e1; e2 ] ->
          begin match e2.node with
          | EConstant (_, s) ->
              let i = int_of_string s in
              assert (i >= 0);
              EApp (e, [ e1; Krml.Helpers.mk_uint32 i ])
          | _ ->
              EApp (e, [ e1; with_type (TInt Constant.UInt32) (ECast (e2, TInt Constant.UInt32)) ])
          end
      | _ ->
          super#visit_EApp env e es
  end)#visit_files () files

let precleanup files =
  let files = expand_array_copies files in
  let files = flatten_sequences files in
  let files = adjust_shifts files in
  files
