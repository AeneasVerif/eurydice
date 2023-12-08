open Krml.Ast
open Krml.DeBruijn

module H = Krml.Helpers

(* Target cleanups invoked from bin/main.ml *)

let remove_implicit_array_copies = object(self)
  inherit [ _ ] map as super
  method! visit_ELet ((), _ as env) b e1 e2 =
    (* let b: TArray (_, n) = e1 in e2 *)
    match b.typ with
    | TArray (_, n) when e1.node <> EAny ->
        let zero = Krml.(Helpers.zero Constant.SizeT) in
        (* let b = <uninitialized> in *)
        ELet (b, H.any, with_type e2.typ (
          ELet (H.sequence_binding (),
            (* let _ = blit e1 (a.k.a. src) b (a.k.a. dst) in *)
            H.with_unit (EBufBlit (lift 1 e1, zero, with_type b.typ (EBound 0), zero, PreCleanup.expr_of_constant n)),
            (* e2 *)
            lift 1 (self#visit_expr env e2))))
    | _ ->
        super#visit_ELet env b e1 e2

  method! visit_EAssign env lhs rhs =
    match lhs.typ with
    | TArray (_, n) ->
        let zero = Krml.(Helpers.zero Constant.SizeT) in
        EBufBlit (rhs, zero, lhs, zero, PreCleanup.expr_of_constant n)
    | _ ->
        super#visit_EAssign env lhs rhs
end

let remove_array_repeats = object(self)
  inherit [_] map as super

  method! visit_EApp env e es =
    let e = self#visit_expr_w () e in
    let es = List.map (self#visit_expr env) es in
    match e.node, es with
    | ETApp ({ node = EQualified lid; _ }, [ len ], [ _ ]), [ init ] when lid = Builtin.array_repeat.name ->
        let l = match len.node with EConstant (_, s) -> int_of_string s | _ -> failwith "impossible" in
        EBufCreateL (Stack, List.init l (fun _ -> init))
    | _ ->
        super#visit_EApp env e es
end

let remove_trivial_into = object(self)
  inherit [_] map as _super

  method! visit_EApp env e es =
    let e = self#visit_expr_w () e in
    let es = List.map (self#visit_expr env) es in
    match e.node, es with
    | ETApp ({ node = EQualified (["core"; "convert"; _ ], "into"); _ }, [], [ t1; t2 ]), [ e1 ] when t1 = t2 ->
        e1.node
    | _ ->
        EApp (e, es)
end
