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

let expr_of_cg n_cgs n i_cg =
  EBound (n - n_cgs + i_cg)

let expr_of_array_length (n_cgs, i) = function
  | TArray (_, n) -> PreCleanup.expr_of_constant n
  | TCgArray (_, n) -> with_type (TInt SizeT) (expr_of_cg n_cgs i n)
  | _ -> failwith "impossible"

let remove_implicit_array_copies = object(self)

  inherit Krml.DeBruijn.map_counting_cg as super

  method private remove_assign env n lhs rhs e2 =
    match rhs.node with
    | EBufCreateL (Stack, es) ->
        (* let _ = lhs := bufcreatel e1, e2, ... lhs[0] := e1, lhs[1] := e2, ... *)
        (* assert (List.length es = int_of_string (snd n)); *)
        let lift = Krml.DeBruijn.lift in
        let rec nest i es =
          match es with
          | [] -> lift i (self#visit_expr_w env e2)
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
        (* let _ = *)
        ELet (H.sequence_binding (),
          H.with_unit (EBufBlit (rhs, zero, lhs, zero, n)),
          lift 1 (self#visit_expr_w env e2))

  method! visit_ELet env b e1 e2 =
    let is_suitable_initializer = function EAny | EBufCreate _ | EBufCreateL _ -> true | _ -> false in
    match b.typ, e1.node with
    (* COPY: let b: TArray (_, n) = e1 in e2 *)
    | (TArray _ | TCgArray _), _ when not (is_suitable_initializer e1.node) ->
        let n = expr_of_array_length (fst env) b.typ in
        let zero = Krml.(Helpers.zero Constant.SizeT) in
        (* let b = <uninitialized> in *)
        ELet (b, H.any, with_type e2.typ (
          (* -- crossing first binder *)
          let env = self#extend (fst env) b in
          (* let _ = blit e1 (a.k.a. src) b (a.k.a. dst) in *)
          ELet (H.sequence_binding (),
            H.with_unit (EBufBlit (lift 1 e1, zero, with_type b.typ (EBound 0), zero, n)),
            (* -- crossing second binder, not #extending since this is going to be lifted by one *)
            (* e2 *)
            lift 1 (self#visit_expr_w env e2))))

    (* COPY: let _ = lhs := rhs with lhs.typ == TArray _ ... *)
    | _, EAssign (lhs, rhs) when H.is_array lhs.typ ->
        let n = expr_of_array_length (fst env) lhs.typ in
        (* Fixpoint here for multi-dimensional arrays. *)
        (* -- not #extending since we operate on e2 where b has already been
           substituted away *)
        (self#visit_expr env (with_type e2.typ (self#remove_assign (fst env) n lhs rhs (subst H.eunit 0 e2)))).node
    | _ ->
        super#visit_ELet env b e1 e2

  method! visit_EAssign env lhs rhs =
    match lhs.typ with
    | TArray _ | TCgArray _ ->
        let n = expr_of_array_length (fst env) lhs.typ in
        (* Fixpoint here for multi-dimensional arrays. *)
        (self#visit_expr env (H.with_unit (self#remove_assign (fst env) n lhs rhs H.eunit))).node

    | _ ->
        super#visit_EAssign env lhs rhs

  method! visit_DFunction _ cc flags n_cgs n t name bs e =
    super#visit_DFunction (n_cgs, 0) cc flags n_cgs n t name bs e
end

let remove_array_repeats = object(self)
  inherit [_] map as super

  method! visit_EApp env e es =
    match e.node, es with
    | ETApp ({ node = EQualified lid; _ }, [ len ], [ _ ]), [ init ] when lid = Builtin.array_repeat.name ->
        let init = self#visit_expr env init in
        begin match len.node with
        | EConstant (_, s) ->
            let l = int_of_string s in
            EBufCreateL (Stack, List.init l (fun _ -> init))
        | _ ->
            EBufCreate (Stack, init, len)
        end
    | _ ->
        super#visit_EApp env e es

  method! visit_ELet (((), _) as env) b e1 e2 =
    let rec all_repeats e =
      match e.node with
      | EConstant _ ->
          true
      | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ _ ], [ _ ]); _ }, [ init ]) when lid = Builtin.array_repeat.name ->
          all_repeats init
      | _ ->
          false
    in
    match e1.node with
    | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ len ], [ _ ]); _ }, [ init ]) when lid = Builtin.array_repeat.name ->
        if all_repeats e1 then
          (* Further code-gen can handle nested ebufcreatel's by using nested
             static initializer lists, possiblye shortening to { 0 } if
             applicable. *)
          super#visit_ELet env b e1 e2
        else
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

let remove_array_from_fn = object
  inherit [_] map as super

  val mutable defs = Hashtbl.create 41

  method! visit_DFunction _ cc flags n_cgs n t name bs e =
    (* assert (n_cgs = 0 && n = 0); *)
    match bs with
    | [{ typ = TInt SizeT; _ }] ->
        Hashtbl.add defs name e
    | _ ->
        ()
    ; ;
    super#visit_DFunction () cc flags n_cgs n t name bs e

  method! visit_EApp env e es =
    match e.node with
    | ETApp ({ node = EQualified (["core"; "array"], "from_fn"); _ },
      [ len ],
      [ t_elements; TArrow (t_index, t_elements') ]) ->
        assert (t_elements' = t_elements);
        assert (t_index = TInt SizeT);
        assert (List.length es = 2);
        let closure = Krml.Helpers.assert_elid (List.nth es 0).node in
        assert (Hashtbl.mem defs closure);
        let dst = List.nth es 1 in
        EFor (Krml.Helpers.fresh_binder ~mut:true "i" H.usize, H.zero_usize (* i = 0 *),
          H.mk_lt_usize (Krml.DeBruijn.lift 1 len) (* i < len *),
          H.mk_incr_usize (* i++ *),
          let i = with_type H.usize (EBound 0) in
          Krml.Helpers.with_unit (EBufWrite (Krml.DeBruijn.lift 1 dst, i, Hashtbl.find defs closure)))
    | _ ->
        super#visit_EApp env e es
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
    | EBool true ->
        (self#visit_expr env e2).node
    | EBool false ->
        (self#visit_expr env e3).node
    | _ ->
        super#visit_EIfThenElse env e1 e2 e3

  method! visit_ESwitch env scrut branches =
    let const_eq (w1, s1) (w2, s2) = w1 = w2 && int_of_string s1 = int_of_string s2 in
    let fits s (w': K.width) =
      let s = Z.of_string s in
      match w' with
      | UInt8 -> Z.leq s (Z.of_string "0xff")
      | UInt16 -> Z.leq s (Z.of_string "0xffff")
      | UInt32 -> Z.leq s (Z.of_string "0xffffffff")
      | UInt64 -> Z.leq s (Z.of_string "0xffffffffffffffff")
      | _ -> false (* conservative decision *)
    in
    let normalize = function
      | ECast ({ node = EConstant (_, s); _ }, TInt w') when fits s w' ->
          EConstant (w', s)
      | c ->
          c
    in
    match normalize scrut.node with
    | EConstant c ->
        begin match List.find_opt (function (SConstant c', _) -> const_eq c c' | _ -> false) branches with
        | Some (_, b) ->
            (self#visit_expr env b).node
        | None ->
            begin match List.find_opt (fun (sv, _) -> sv = SWild) branches with
            | Some (_, b) ->
                (self#visit_expr env b).node
            | None ->
                assert (snd env = TUnit);
                EUnit
            end
        end
    | _ ->
        super#visit_ESwitch env scrut branches
end

let contains_array t = object(_self)
  inherit [_] reduce as _super

  method zero = false
  method plus = (||)

  method! visit_TBuf _ _ _ =
    false

  method! visit_TArray _ _ _ =
    true

  method! visit_TCgArray _ _ _ =
    true
end#visit_expr_w () t

let remove_literals = object(_self)
  inherit [_] map as super_map
  inherit! Krml.Structs.remove_literals as super_krml

  method! visit_ELet env b e1 e2 =
    if contains_array e1 then
      super_krml#visit_ELet env b e1 e2
    else
      super_map#visit_ELet env b e1 e2

  method! visit_EFlat ((), t as env) fields =
    if contains_array (with_type t (EFlat fields)) then
      super_krml#visit_EFlat env fields
    else
      super_map#visit_EFlat env fields
end

let build_macros (macros: Krml.Idents.LidSet.t ref) = object(_self)
  inherit [_] map as super

  method! visit_DGlobal env flags name n t body =
    if Krml.Helpers.is_bufcreate body then
      super#visit_DGlobal env flags name n t body
    else begin
      macros := Krml.Idents.LidSet.(union !macros (singleton name));
      DGlobal (flags @ [ Macro ], name, n, t, body)
    end
end

let build_macros files =
  let map = ref Krml.Idents.LidSet.empty in
  let files = (build_macros map)#visit_files () files in
  files, !map

(* In some cases, we wish to disable const-generic monomorphization to e.g.
   avoid code-size bloat. For the moment, this is a global switch, but in the
   future, one could conceivably use an attribute, or a pass a list of functions
   that should opt-out of monomorphization via the command-line. *)
let disable_cg_monomorphization = object(self)
  inherit [_] map as super

  method! visit_EApp env e es =
    match e.node with
    | ETApp ({ node = EQualified (("Eurydice"|"core") :: _,_); _ }, _, _) ->
        super#visit_EApp env e es
    | ETApp (e, es', ts')  ->
        let e = self#visit_expr_w () e in
        let es = List.map (self#visit_expr_w ()) es in
        let es' = List.map (self#visit_expr_w ()) es' in
        EApp (with_type e.typ (ETApp (e, [], ts')), es' @ es)
    | _ ->
        super#visit_EApp env e es

  method! visit_ETApp ((), t) e es ts =
    let e = self#visit_expr_w () e in
    let es = List.map (self#visit_expr_w ()) es in
    match e.node with
    | EQualified (("Eurydice"|"core") :: _,_) ->
        ETApp (e, es, ts)
    | _ ->
        EApp (with_type t (ETApp (e, [], ts)), es)
end


(* The previous phase left n_cgs in place over DFunctions so that
   remove_implicit_array_copies could convert length arguments of TCgArray's to
   runtime expressions, in order to implement blit operations to materialize
   array copies. We now remove those, and manually decay TCgArrays into TBufs,
   relying on Checker to validate this pass. *)
let erase_and_decay_cgs = object(self)
  inherit Krml.DeBruijn.map_counting_cg as super

  method! visit_ELet env b e1 e2 =
    match b.typ, e1.node with
    | TCgArray (t, _), EAny ->
        let n = expr_of_array_length (fst env) b.typ in
        let typ = TBuf (t, false) in
        ELet ({ b with typ }, with_type typ (EBufCreate (Stack, H.any, n)),
          self#visit_expr_w (self#extend (fst env) { b with typ }) e2)
    | _ ->
        super#visit_ELet env b e1 e2

  method! visit_TCgArray env t _ =
    super#visit_TBuf env t false

  method! visit_DFunction _ cc flags n_cgs n t name bs e =
    super#visit_DFunction (n_cgs, 0) cc flags 0 n t name bs e

end

