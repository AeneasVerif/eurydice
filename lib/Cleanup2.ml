open Krml.Ast
open Krml.DeBruijn

module H = Krml.Helpers

(* Target cleanups invoked from bin/main.ml *)

let break_down_nested_arrays = object(self)
  inherit [_] map as super
  method! visit_ELet ((), _ as env) b e1 e2 =
    match e1.node with
    | EBufCreateL (Stack, es) when H.is_array (List.hd es).typ ->
        ELet (b, H.any, with_type e2.typ (ESequence (
          List.mapi (fun i e ->
            let b = with_type b.typ (EBound 0) in
            let i = with_type H.usize (EConstant (SizeT, string_of_int i)) in
            let b_i = with_type (H.assert_tbuf_or_tarray b.typ) (EBufRead (b, i)) in
            H.with_unit (EAssign (b_i, self#visit_expr env (Krml.DeBruijn.lift 1 e)))
          ) es @ [
            self#visit_expr env e2
          ])))
    | _ ->
        super#visit_ELet env b e1 e2
end


let remove_implicit_array_copies = object(self)

  inherit [ _ ] map as super

  method private remove_assign n lhs rhs e2 =
    match rhs.node with
    | EBufCreateL (Stack, es) ->
        (* let _ = lhs := bufcreatel e1, e2, ... lhs[0] := e1, lhs[1] := e2, ... *)
        assert (List.length es = int_of_string (snd n));
        let lift = Krml.DeBruijn.lift in
        let rec nest i es =
          match es with
          | [] -> lift i (self#visit_expr_w () e2)
          | e :: es ->
              let i_ = with_type H.usize (EConstant (SizeT, string_of_int i)) in
              let lhs_i = with_type (H.assert_tbuf_or_tarray lhs.typ) (EBufRead (lhs, i_)) in
              with_type e2.typ (ELet (H.sequence_binding (),
                H.with_unit (EAssign (lift i lhs_i, lift i e)),
                nest (i + 1) es))
        in
        (nest 0 es).node
    | _ ->
        let zero = Krml.(Helpers.zero Constant.SizeT) in
        EBufBlit (rhs, zero, lhs, zero, PreCleanup.expr_of_constant n)

  method! visit_ELet ((), _ as env) b e1 e2 =
    let is_suitable_initializer = function EAny | EBufCreate _ | EBufCreateL _ -> true | _ -> false in
    match b.typ, e1.node with
    (* COPY: let b: TArray (_, n) = e1 in e2 *)
    | TArray (_, n), _ when not (is_suitable_initializer e1.node) ->
        let zero = Krml.(Helpers.zero Constant.SizeT) in
        (* let b = <uninitialized> in *)
        ELet (b, H.any, with_type e2.typ (
          ELet (H.sequence_binding (),
            (* let _ = blit e1 (a.k.a. src) b (a.k.a. dst) in *)
            H.with_unit (EBufBlit (lift 1 e1, zero, with_type b.typ (EBound 0), zero, PreCleanup.expr_of_constant n)),
            (* e2 *)
            lift 1 (self#visit_expr env e2))))

    (* COPY: let _ = lhs := rhs with lhs.typ == TArray _ ... *)
    | _, EAssign (lhs, rhs) when H.is_array lhs.typ ->
        let n = match lhs.typ with TArray (_, n) -> n | _ -> failwith "impossible" in
        (* Fixpoint here for multi-dimensional arrays. *)
        (self#visit_expr env (with_type e2.typ (self#remove_assign n lhs rhs e2))).node
    | _ ->
        super#visit_ELet env b e1 e2

  method! visit_EAssign env lhs rhs =
    match lhs.typ with
    | TArray (_, n) ->
        (* Fixpoint here for multi-dimensional arrays. *)
        (self#visit_expr env (H.with_unit (self#remove_assign n lhs rhs H.eunit))).node
    | _ ->
        super#visit_EAssign env lhs rhs
end

let remove_array_repeats = object(self)
  inherit [_] map as super

  method! visit_EApp env e es =
    match e.node, es with
    | ETApp ({ node = EQualified lid; _ }, [ len ], [ _ ]), [ init ] when lid = Builtin.array_repeat.name ->
        let l = match len.node with EConstant (_, s) -> int_of_string s | _ -> failwith "impossible" in
        EBufCreateL (Stack, List.init l (fun _ -> init))
    | _ ->
        super#visit_EApp env e es

  method! visit_ELet (((), _) as env) b e1 e2 =
    match e1.node with
    | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ len ], [ _ ]); _ }, [ init ]) when lid = Builtin.array_repeat.name ->
        (* let b = [ init; len ] *)
        let module H = Krml.Helpers in
        let len = self#visit_expr env len in
        let init = self#visit_expr env init in
        (* let b; *)
        ELet (b, H.any,
        (* let _ = *)
        with_type e2.typ (ELet (H.sequence_binding (),
          (* for *)
          H.with_unit (EFor (Krml.Helpers.fresh_binder ~mut:true "i" H.usize, H.zero_usize (* i = 0 *),
            H.mk_lt_usize (Krml.DeBruijn.lift 2 len) (* i < len *),
            H.mk_incr_usize (* i++ *),
            let i = with_type H.usize (EBound 0) in
            let b = with_type b.typ (EBound 1) in
            let b_i = with_type (H.assert_tbuf_or_tarray b.typ) (EBufRead (b, i)) in
            (* b[i] := init *)
            H.with_unit (EAssign (b_i, Krml.DeBruijn.lift 2 init)))),
        (* e2 *)
        Krml.DeBruijn.lift 1 (self#visit_expr env e2))))

    | _ ->
        super#visit_ELet env b e1 e2
end

let rewrite_slice_to_array = object(_self)
  inherit [_] map as super

  method! visit_expr ((), _ as env) e =
    match e.node with
    | EApp ({ node = ETApp ({ node = EQualified lid; _ }, _, ts); _ }, es) when lid = Builtin.slice_to_array.name ->
        let src = Krml.KList.one es in
        (* src = slice ..., dst = array ... *)
        let result_t = e.typ in
        let slice_to_array2 = with_type Builtin.slice_to_array2.typ (EQualified Builtin.slice_to_array2.name) in
        let slice_to_array2 = with_type (subst_tn ts Builtin.slice_to_array2.typ) (ETApp (slice_to_array2, [], ts)) in
        (* let dst = *)
        with_type result_t (ELet (H.fresh_binder "dst" result_t, H.any,
        (* let _ = *)
        with_type result_t (ELet (H.sequence_binding (),
          (* slice_to_array (&dst, src) *)
          H.with_unit (EApp (slice_to_array2, [
            with_type (TBuf (result_t, false)) (EAddrOf (with_type result_t (EBound 0)));
            lift 1 src
          ])),
          (* dst *)
          with_type result_t (EBound 1)))))
    | _ ->
        super#visit_expr env e

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

let remove_trivial_ite = object(self)
  inherit [_] map as super

  method! visit_EIfThenElse ((), _ as env) e1 e2 e3 =
    match e1.node with
    | EApp ({ node = EOp (Eq, _); _ }, [
      { node = EConstant (w1, c1); _ };
      { node = EConstant (w2, c2); _ };
    ]) when w1 = w2 ->
      if int_of_string c1 = int_of_string c2 then
        (self#visit_expr env e2).node
      else
        (self#visit_expr env e3).node
    | _ ->
        super#visit_EIfThenElse env e1 e2 e3
end
