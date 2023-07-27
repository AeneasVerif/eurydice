open Krml.Ast

let expr_of_constant (w, n) =
  with_type (TInt w) (EConstant (w, n))

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
