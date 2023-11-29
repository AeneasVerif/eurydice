open Krml.Ast

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
      | { node = EBound _; typ = TArray (_, n) },
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

let precleanup files =
  let files = expand_array_copies files in
  let files = flatten_sequences files in
  files
