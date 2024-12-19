open Krml.Ast
open Krml.DeBruijn
module H = Krml.Helpers
module L = Logging
open Krml.PrintAst.Ops

(* Target cleanups invoked from bin/main.ml *)

let break_down_nested_arrays =
  object (self)
    inherit [_] map as super

    method! visit_ELet (((), _) as env) b e1 e2 =
      match e1.node with
      | EBufCreateL (Stack, es) when H.is_array (List.hd es).typ ->
          ELet
            ( b,
              H.any,
              with_type e2.typ
                (ESequence
                   (List.mapi
                      (fun i e ->
                        let b = with_type b.typ (EBound 0) in
                        let i = with_type H.usize (EConstant (SizeT, string_of_int i)) in
                        let b_i = with_type (H.assert_tbuf_or_tarray b.typ) (EBufRead (b, i)) in
                        H.with_unit (EAssign (b_i, self#visit_expr env (Krml.DeBruijn.lift 1 e))))
                      es
                   @ [ self#visit_expr env e2 ])) )
      | _ -> super#visit_ELet env b e1 e2
  end

let remove_implicit_array_copies =
  object (self)
    inherit [_] map as super

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
                with_type e2.typ
                  (ELet
                     ( H.sequence_binding (),
                       H.with_unit (EAssign (lift i lhs_i, lift i e)),
                       nest (i + 1) es ))
          in
          (nest 0 es).node
      | _ ->
          let zero = Krml.(Helpers.zero Constant.SizeT) in
          (* let _ = *)
          ELet
            ( H.sequence_binding (),
              H.with_unit (EBufBlit (rhs, zero, lhs, zero, PreCleanup.expr_of_constant n)),
              lift 1 (self#visit_expr_w () e2) )

    method! visit_ELet (((), _) as env) b e1 e2 =
      let is_suitable_initializer = function
        | EAny | EBufCreate _ | EBufCreateL _ -> true
        | _ -> false
      in
      match b.typ, e1.node with
      (* COPY: let b: TArray (_, n) = e1 in e2 *)
      | TArray (_, n), _ when not (is_suitable_initializer e1.node) ->
          let zero = Krml.(Helpers.zero Constant.SizeT) in
          (* let b = <uninitialized> in *)
          ELet
            ( b,
              H.any,
              with_type e2.typ
                (ELet
                   ( H.sequence_binding (),
                     (* let _ = blit e1 (a.k.a. src) b (a.k.a. dst) in *)
                     H.with_unit
                       (EBufBlit
                          ( lift 1 e1,
                            zero,
                            with_type b.typ (EBound 0),
                            zero,
                            PreCleanup.expr_of_constant n )),
                     (* e2 *)
                     lift 1 (self#visit_expr env e2) )) )
      (* COPY: let _ = lhs := rhs with lhs.typ == TArray _ ... *)
      | _, EAssign (lhs, rhs) when H.is_array lhs.typ ->
          let n =
            match lhs.typ with
            | TArray (_, n) -> n
            | _ -> failwith "impossible"
          in
          (* Fixpoint here for multi-dimensional arrays. *)
          (self#visit_expr env
             (with_type e2.typ (self#remove_assign n lhs rhs (subst H.eunit 0 e2))))
            .node
      | _ -> super#visit_ELet env b e1 e2

    method! visit_EAssign env lhs rhs =
      match lhs.typ with
      | TArray (_, n) ->
          (* Fixpoint here for multi-dimensional arrays. *)
          (self#visit_expr env (H.with_unit (self#remove_assign n lhs rhs H.eunit))).node
      | _ -> super#visit_EAssign env lhs rhs
  end

let remove_array_temporaries =
  object (self)
    inherit [_] map as _super

    method! visit_ELet (((), _) as env) b e1 e2 =
      (* let x: TArray (t, n) = any;
         blit (src, 0, dst, 0, n); // same length
         x
         ~~>
         src
      *)
      match snd !(b.node.mark), b.typ, e1.node, e2.node with
      | ( AtMost 2,
          TArray (_, l),
          EAny,
          ESequence
            [
              {
                node =
                  EBufBlit
                    ( src,
                      { node = EConstant (_, "0"); _ },
                      { node = EBound 0; _ },
                      { node = EConstant (_, "0"); _ },
                      { node = EConstant l'; _ } );
                _;
              };
              { node = EBound 0; _ };
            ] )
        when l = l' -> (subst H.eunit 0 src).node
      | _ -> ELet (b, self#visit_expr env e1, self#visit_expr env e2)
  end

let remove_array_repeats =
  object (self)
    inherit [_] map as super

    method! visit_EApp env e es =
      match e.node, es with
      | ETApp ({ node = EQualified lid; _ }, [ len ], _, [ _ ]), [ init ]
        when lid = Builtin.array_repeat.name ->
          let l =
            match len.node with
            | EConstant (_, s) -> int_of_string s
            | _ -> failwith "impossible"
          in
          let init = self#visit_expr env init in
          EBufCreateL (Stack, List.init l (fun _ -> init))
      | _ -> super#visit_EApp env e es

    method! visit_ELet (((), _) as env) b e1 e2 =
      let rec all_repeats e =
        match e.node with
        | EConstant _ -> true
        | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ _ ], _, [ _ ]); _ }, [ init ])
          when lid = Builtin.array_repeat.name -> all_repeats init
        | _ -> false
      in
      match e1.node with
      | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ len ], _, [ _ ]); _ }, [ init ])
        when lid = Builtin.array_repeat.name ->
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
            ELet
              ( b,
                H.any,
                (* let _ = *)
                with_type e2.typ
                  (ELet
                     ( H.sequence_binding (),
                       (* for *)
                       H.with_unit
                         (EFor
                            ( Krml.Helpers.fresh_binder ~mut:true "i" H.usize,
                              H.zero_usize (* i = 0 *),
                              H.mk_lt_usize (Krml.DeBruijn.lift 2 len) (* i < len *),
                              H.mk_incr_usize (* i++ *),
                              let i = with_type H.usize (EBound 0) in
                              let b = with_type b.typ (EBound 1) in
                              let b_i =
                                with_type (H.assert_tbuf_or_tarray b.typ) (EBufRead (b, i))
                              in
                              (* b[i] := init *)
                              H.with_unit (EAssign (b_i, Krml.DeBruijn.lift 2 init)) )),
                       (* e2 *)
                       Krml.DeBruijn.lift 1 (self#visit_expr env e2) )) )
      | _ -> super#visit_ELet env b e1 e2
  end

let remove_array_from_fn files =
  let defs =
    Krml.Helpers.build_map files (fun tbl d ->
        match d with
        | DFunction (_, _, _, _, _, name, _, body) -> Hashtbl.add tbl name body
        | _ -> ())
  in
  begin
    object
      inherit [_] map as super

      method! visit_DFunction _ cc flags n_cgs n t name bs e =
        assert (n_cgs = 0 && n = 0);
        Hashtbl.add defs name e;
        super#visit_DFunction () cc flags n_cgs n t name bs e

      method! visit_EApp env e es =
        match e.node with
        | ETApp
            ( { node = EQualified ([ "core"; "array" ], "from_fn"); _ },
              [ len ],
              _,
              [ t_elements; TArrow (t_index, TArrow (t_elements', TUnit)) ] ) ->
            (* Same as below, but catching the case where the type of elements is an
               array and has undergone outparam optimization (i.e. the closure,
               instead of having type size_t -> t_element, has type size_t -> t_element ->
               unit *)
            L.log "Cleanup2" "%a %a" ptyp t_elements ptyp t_elements';
            assert (t_elements' = t_elements);
            assert (t_index = TInt SizeT);
            assert (List.length es = 2);
            let closure_lid, state =
              match (List.hd es).node with
              | EQualified _ -> List.hd es, []
              | EApp (({ node = EQualified lid; _ } as hd), [ e_state ]) ->
                  L.log "Cleanup2" "closure=%a" pexpr
                    (Krml.DeBruijn.subst e_state 0 (Hashtbl.find defs lid));
                  hd, [ e_state ]
              | _ ->
                  L.log "Cleanup2" "closure=%a" pexpr (List.hd es);
                  failwith "unexpected closure shape"
            in
            (* First argument = closure, second argument = destination. Note that
               the closure may itself be an application of the closure to the state
               (but not always... is this unit argument elimination kicking in? not
               sure). *)
            let dst = List.nth es 1 in
            let lift1 = Krml.DeBruijn.lift 1 in
            EFor
              ( Krml.Helpers.fresh_binder ~mut:true "i" H.usize,
                H.zero_usize (* i = 0 *),
                H.mk_lt_usize (Krml.DeBruijn.lift 1 len) (* i < len *),
                H.mk_incr_usize (* i++ *),
                let i = with_type H.usize (EBound 0) in
                Krml.Helpers.with_unit
                  (EApp
                     ( closure_lid,
                       List.map lift1 state @ [ i; with_type t_elements (EBufRead (lift1 dst, i)) ]
                     )) )
        | ETApp
            ( { node = EQualified ([ "core"; "array" ], "from_fn"); _ },
              [ len ],
              _,
              [ t_elements; TArrow (t_index, t_elements') ] ) ->
            (* Not sure why this one inlines the body, but not above. *)
            L.log "Cleanup2" "%a %a" ptyp t_elements ptyp t_elements';
            assert (t_elements' = t_elements);
            assert (t_index = TInt SizeT);
            assert (List.length es = 2);
            let closure =
              match (List.hd es).node with
              | EQualified lid -> Hashtbl.find defs lid
              | EApp ({ node = EQualified lid; _ }, [ e_state ]) ->
                  L.log "Cleanup2" "closure=%a" pexpr
                    (Krml.DeBruijn.subst e_state 0 (Hashtbl.find defs lid));
                  Krml.DeBruijn.subst e_state 1 (Hashtbl.find defs lid)
              | _ ->
                  L.log "Cleanup2" "closure=%a" pexpr (List.hd es);
                  failwith "unexpected closure shape"
            in
            let dst = List.nth es 1 in
            EFor
              ( Krml.Helpers.fresh_binder ~mut:true "i" H.usize,
                H.zero_usize (* i = 0 *),
                H.mk_lt_usize (Krml.DeBruijn.lift 1 len) (* i < len *),
                H.mk_incr_usize (* i++ *),
                let i = with_type H.usize (EBound 0) in
                Krml.Helpers.with_unit (EBufWrite (Krml.DeBruijn.lift 1 dst, i, closure)) )
        | ETApp ({ node = EQualified ("core" :: "array" :: _, "map"); _ }, [ len ], _, ts) ->
            let t_src, t_dst =
              match ts with
              | [ t_src; t_closure; t_dst ] ->
                  assert (t_closure = TArrow (t_src, t_dst));
                  L.log "Cleanup2" "found array map from %a to %a" ptyp t_src ptyp t_dst;
                  t_src, t_dst
              | _ ->
                  failwith "TODO: unknown map closure shape; is it an array outparam? (see above)"
            in
            let e_src, e_closure, e_dst =
              match es with
              | [ e_src; e_closure; e_dst ] -> e_src, e_closure, e_dst
              | _ -> failwith "unknown shape of arguments to array map"
            in
            let closure_lid, state =
              match e_closure.node with
              | EQualified _ -> e_closure, []
              | EApp (({ node = EQualified lid; _ } as hd), [ e_state ]) ->
                  L.log "Cleanup2" "map closure=%a" pexpr
                    (Krml.DeBruijn.subst e_state 0 (Hashtbl.find defs lid));
                  hd, [ e_state ]
              | _ ->
                  L.log "Cleanup2" "map closure=%a" pexpr (List.hd es);
                  failwith "unexpected map closure shape"
            in
            let lift1 = Krml.DeBruijn.lift 1 in
            EFor
              ( Krml.Helpers.fresh_binder ~mut:true "i" H.usize,
                H.zero_usize (* i = 0 *),
                H.mk_lt_usize (Krml.DeBruijn.lift 1 len) (* i < len *),
                H.mk_incr_usize (* i++ *),
                let i = with_type H.usize (EBound 0) in
                let e_src_i = with_type t_src (EBufRead (lift1 e_src, i)) in
                Krml.Helpers.with_unit
                  (EBufWrite
                     ( lift1 e_dst,
                       i,
                       with_type t_dst (EApp (closure_lid, List.map lift1 state @ [ e_src_i ])) ))
              )
        | _ -> super#visit_EApp env e es
    end
  end
    #visit_files
    () files

let rewrite_slice_to_array =
  object (_self)
    inherit [_] map as super

    method! visit_expr (((), _) as env) e =
      match e.node with
      | EApp ({ node = ETApp ({ node = EQualified lid; _ }, _, _, ts); _ }, es)
        when lid = Builtin.slice_to_array.name ->
          let src = Krml.KList.one es in
          (* src = slice ..., dst = array ... *)
          let result_t = e.typ in
          let slice_to_array2 = Builtin.(expr_of_builtin slice_to_array2) in
          let slice_to_array2 =
            with_type
              (Krml.MonomorphizationState.resolve (subst_tn ts Builtin.slice_to_array2.typ))
              (ETApp (slice_to_array2, [], [], ts))
          in
          (* let dst = *)
          with_type result_t
            (ELet
               ( H.fresh_binder "dst" result_t,
                 H.any,
                 (* let _ = *)
                 with_type result_t
                   (ELet
                      ( H.sequence_binding (),
                        (* slice_to_array (&dst, src) *)
                        H.with_unit
                          (EApp
                             ( slice_to_array2,
                               [
                                 with_type
                                   (TBuf (result_t, false))
                                   (EAddrOf (with_type result_t (EBound 0)));
                                 lift 1 src;
                               ] )),
                        (* dst *)
                        with_type result_t (EBound 1) )) ))
      | _ -> super#visit_expr env e
  end

let remove_trivial_into =
  object (self)
    inherit [_] map as _super

    method! visit_EApp env e es =
      let e = self#visit_expr_w () e in
      let es = List.map (self#visit_expr env) es in
      match e.node, es with
      | ( ETApp ({ node = EQualified ([ "core"; "convert"; _ ], "into"); _ }, [], _, [ t1; t2 ]),
          [ e1 ] )
        when t1 = t2 -> e1.node
      | ( ETApp
            ( { node = EQualified ([ "core"; "convert"; _ ], "into"); _ },
              [],
              _,
              [ TInt _; (TInt _ as t2) ] ),
          [ e1 ] ) -> ECast (e1, t2)
      | _ -> EApp (e, es)
  end

let remove_trivial_ite =
  object (self)
    inherit [_] map as super

    method! visit_EIfThenElse (((), _) as env) e1 e2 e3 =
      match e1.node with
      | EApp
          ( { node = EOp (Eq, _); _ },
            [ { node = EConstant (w1, c1); _ }; { node = EConstant (w2, c2); _ } ] )
        when w1 = w2 ->
          if int_of_string c1 = int_of_string c2 then
            (self#visit_expr env e2).node
          else
            (self#visit_expr env e3).node
      | EBool true -> (self#visit_expr env e2).node
      | EBool false -> (self#visit_expr env e3).node
      | _ -> super#visit_EIfThenElse env e1 e2 e3

    method! visit_ESwitch env scrut branches =
      let const_eq (w1, s1) (w2, s2) = w1 = w2 && int_of_string s1 = int_of_string s2 in
      let fits s (w' : K.width) =
        let s = Z.of_string s in
        match w' with
        | UInt8 -> Z.leq s (Z.of_string "0xff")
        | UInt16 -> Z.leq s (Z.of_string "0xffff")
        | UInt32 -> Z.leq s (Z.of_string "0xffffffff")
        | UInt64 -> Z.leq s (Z.of_string "0xffffffffffffffff")
        | _ -> false (* conservative decision *)
      in
      let normalize = function
        | ECast ({ node = EConstant (_, s); _ }, TInt w') when fits s w' -> EConstant (w', s)
        | c -> c
      in
      match normalize scrut.node with
      | EConstant c -> begin
          match
            List.find_opt
              (function
                | SConstant c', _ -> const_eq c c'
                | _ -> false)
              branches
          with
          | Some (_, b) -> (self#visit_expr env b).node
          | None -> begin
              match List.find_opt (fun (sv, _) -> sv = SWild) branches with
              | Some (_, b) -> (self#visit_expr env b).node
              | None ->
                  assert (snd env = TUnit);
                  EUnit
            end
        end
      | _ -> super#visit_ESwitch env scrut branches
  end

let contains_array t =
  begin
    object (_self)
      inherit [_] reduce as _super
      method zero = false
      method plus = ( || )
      method! visit_TBuf _ _ _ = false
      method! visit_TArray _ _ _ = true
      method! visit_TCgArray _ _ _ = true
    end
  end
    #visit_expr_w
    () t

let remove_literals =
  object (_self)
    inherit [_] map as super_map
    inherit! Krml.Structs.remove_literals as super_krml

    method! visit_ELet env b e1 e2 =
      if contains_array e1 then
        super_krml#visit_ELet env b e1 e2
      else
        super_map#visit_ELet env b e1 e2

    method! visit_EFlat (((), t) as env) fields =
      if contains_array (with_type t (EFlat fields)) then
        super_krml#visit_EFlat env fields
      else
        super_map#visit_EFlat env fields
  end

let build_macros (macros : Krml.Idents.LidSet.t ref) =
  object (_self)
    inherit [_] map as super

    method! visit_DGlobal env flags name n t body =
      if Krml.Helpers.is_bufcreate body then
        super#visit_DGlobal env flags name n t body
      else begin
        (macros := Krml.Idents.LidSet.(union !macros (singleton name)));
        DGlobal (flags @ [ Macro ], name, n, t, body)
      end
  end

let build_macros files =
  let map = ref Krml.Idents.LidSet.empty in
  let files = (build_macros map)#visit_files () files in
  files, !map

let resugar_loops =
  object(self)
  inherit [_] map as super

  method! visit_ELet ((), _ as env) b e1 e2 =
    match e1.node, e2.node with
    (* Terminal position *)
    |
    (* let iter = core::iter::traits::collect<t>({ start = e_start; end = e_end }) in *)
    EApp ({ node = ETApp (
      { node = EQualified (["core"; "iter"; "traits"; "collect"; _], "into_iter"); _ },
      [],
      _,
      [ TApp ((["core"; "ops"; "range"], "Range"), _t')  ]
    ); _ }, [
      { node = EFlat [ Some "start", e_start; Some "end", e_end ]; _ }
    ]),
    (* while (true) *)
    EWhile ({ node = EBool true; _ }, {
      (* let next = core::iter::range::next<t>(&(&iter[0])) in *)
      node = ELet (_, {
        node = EApp ({
          node = ETApp ({
            node = EQualified (["core";"iter";"range";_], "next"); _
          }, [], [], [ t' ]);
          _
        }, [{
          node = EAddrOf({
            node = EBufRead ({
              node = EAddrOf ({
                node = EBound 0;
                _
              }); _
            }, {
              node = EConstant (_, "0"); _
            }); _
          }
          ); _ }]); _ },
        (* match next with None -> break | Some _ -> e_body *)
        {
          node = EMatch (Unchecked, { node = EBound 0; _ }, [
            [], { node = PCons ("None", _); _ }, { node = EBreak; _ };
            [], { node = PCons ("Some", _); _ }, e_body;
          ]); _
        }

    ); _ }) ->
      let open Krml.Helpers in
      let w = match t' with TInt w -> w | _ -> assert false in
      let e_some_i = with_type (Builtin.mk_option t') (ECons ("Some", [with_type t' (EBound 0)])) in
      EFor (fresh_binder ~mut:true "i" t',
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        mk_incr w,
        self#visit_expr env (Krml.DeBruijn.subst e_some_i 0 e_body))

    (* Non-terminal position *)
    |
    (* let iter = core::iter::traits::collect<t>({ start = e_start; end = e_end }) in *)
    EApp ({ node = ETApp (
      { node = EQualified (["core"; "iter"; "traits"; "collect"; _], "into_iter"); _ },
      [],
      _,
      [ TApp ((["core"; "ops"; "range"], "Range"), _t')  ]
    ); _ }, [
      { node = EFlat [ Some "start", e_start; Some "end", e_end ]; _ }
    ]),
    (* while (true) *)
    ESequence ({ node = EWhile ({ node = EBool true; _ }, {
      (* let next = core::iter::range::next<t>(&(&iter[0])) in *)
      node = ELet (_, {
        node = EApp ({
          node = ETApp ({
            node = EQualified (["core";"iter";"range";_], "next"); _
          }, [], [], [ t' ]);
          _
        }, [{
          node = EAddrOf({
            node = EBufRead ({
              node = EAddrOf ({
                node = EBound 0;
                _
              }); _
            }, {
              node = EConstant (_, "0"); _
            }); _
          }
          ); _ }]); _ },
        (* match next with None -> break | Some _ -> e_body *)
        {
          node = EMatch (Unchecked, { node = EBound 0; _ }, [
            [], { node = PCons ("None", _); _ }, { node = EBreak; _ };
            [], { node = PCons ("Some", _); _ }, e_body;
          ]); _
        }

    ); _ }); _ }::rest)
    (*; ... *) ->
      let open Krml.Helpers in
      let w = match t' with TInt w -> w | _ -> assert false in
      let e_some_i = with_type (Builtin.mk_option t') (ECons ("Some", [with_type t' (EBound 0)])) in
      ESequence (with_type TUnit (EFor (fresh_binder ~mut:true "i" t',
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        mk_incr w,
        self#visit_expr env (Krml.DeBruijn.subst e_some_i 0 e_body))
      ) :: List.map (fun e -> self#visit_expr env (Krml.DeBruijn.subst eunit 0 e)) rest)
    | _ ->
      super#visit_ELet env b e1 e2


end
[@ocamlformat "disable"]

let improve_names files =
  let renamed = Hashtbl.create 41 in
  let allocated = Hashtbl.create 41 in
  (object (_self)
     inherit [_] iter

     method! visit_DFunction _ _ _ _ _ _ ((m, n) as lid) _ _ =
       let trait_impl, m = List.partition (fun s -> s.[0] = '{') m in
       match trait_impl with
       | [ trait_impl ] ->
           let hash = Hashtbl.hash trait_impl in
           let n = Printf.sprintf "%s_%02x" n (hash land 0xFF) in
           Krml.Monomorphization.maybe_debug_hash hash (lazy PPrint.(string "trait impl:" ^/^ string trait_impl));
           let n = Krml.Idents.mk_fresh n (fun n -> Hashtbl.mem allocated (m, n)) in
           Hashtbl.add renamed lid ((m, n), trait_impl);
           Hashtbl.add allocated (m, n) ()
       | _ -> ()
  end)
    #visit_files
    () files;
  (* Hashtbl.iter (fun k (v, _) -> *)
  (*   Krml.KPrint.bprintf "%a --> %a\n" plid k plid v *)
  (* ) renamed; *)
  (* TODO: are there other global maps like this whose lids need to be
     updated??? *)
  Krml.Options.static_header :=
    List.map
      (function
        | Krml.Bundle.Lid lid when Hashtbl.mem renamed lid ->
            Krml.Bundle.Lid (fst (Hashtbl.find renamed lid))
        | x -> x)
      !Krml.Options.static_header;
  (object (self)
     inherit [_] map

     method! visit_DFunction env cc flags n_cgs n t lid bs e =
       match Hashtbl.find_opt renamed lid with
       | Some (lid, trait_impl) ->
           let comment = Krml.KPrint.bsprintf "This function found in impl %s" trait_impl in
           DFunction (cc, flags @ [ Comment comment ], n_cgs, n, t, lid, bs, self#visit_expr_w env e)
       | None -> DFunction (cc, flags, n_cgs, n, t, lid, bs, self#visit_expr_w env e)

     method! visit_EQualified _ lid =
       EQualified
         (match Hashtbl.find_opt renamed lid with
         | Some (lid, _) -> lid
         | None -> lid)
  end)
    #visit_files
    () files

let recognize_asserts =
  object (_self)
    inherit [_] map as super

    method! visit_EIfThenElse (((), _) as env) e1 e2 e3 =
      match e1.typ, e2.node, e3.node with
      | TBool, EUnit, EAbort (_, msg) ->
          (* if not (e1) then abort msg else ()  -->  static_assert(e1, msg) *)
          EApp
            ( Builtin.static_assert_ref,
              [ e1; with_type Krml.Checker.c_string (EString (Option.value ~default:"" msg)) ] )
      | _ -> super#visit_EIfThenElse env e1 e2 e3
  end

(* Reconstructing for-loops from while nodes introduced by c_for!. *)

class iter_counting = object
  (* The environment [i] has type [int]. *)
  inherit [_] iter
  (* The environment [i] keeps track of how many binders have been
     entered. It is incremented at each binder. *)
  method! extend i (_: binder) =
    i + 1
end

(* De Bruijn index i is found in expression e *)
let found i e =
  let exception Found in
  let find = object
    inherit iter_counting

    method! visit_EBound (i, _) j =
      if i = j then
        raise Found

  end in
  try find#visit_expr_w i e; false
  with Found -> true

let smallest = object
  inherit [_] reduce
  method zero = max_int
  method plus x y = min x y
  method visit_EBound _ i =
    i
end

let rec find_terminal_incr i e =
  assert (e.typ = TUnit);
  let ( let* ) = Option.bind in
  let hoist e = Krml.DeBruijn.subst_n e (List.init i (fun _ -> Krml.Helpers.eunit)) in
  match e.node with
  | ELet (b, e1, e2) ->
      let* e2, e_incr = find_terminal_incr (i + 1) e2 in
      Some ({ e with node = ELet (b, e1, e2) }, e_incr)
  | ESequence es ->
      let es, e_incr = Krml.KList.split_at_last es in
      let nearest = smallest#visit_expr_w () e_incr in
      if nearest < i then
        None
      else
        Some ({ e with node = ESequence es }, hoist e_incr)
  | _ ->
      let nearest = smallest#visit_expr_w () e in
      if nearest < i then
        None
      else
        Some (Krml.Helpers.eunit, hoist e)

let reconstruct_for_loops =
  object (self)
    inherit [_] map as super

    method! visit_ELet (((), _) as env) b e1 e2 =

      match e1.node, e1.typ, e2.node with
      (* t x = e1; while (true) { if (e_cond) { ...;  e_incr } else { break; } *)
      | _, _, EWhile ({ node = EBool true; _ }, {
        node = EIfThenElse (e_cond, e_then, { node = EBreak; _ }); _
      }) ->
        begin match find_terminal_incr 0 e_then with
        | Some (e_then, e_incr) ->
            let e_then = self#visit_expr env e_then in
            EFor (b, e1, e_cond, e_incr, e_then)
        | None ->
            super#visit_ELet env b e1 e2
        end

      (* t x = e_init; while (true) { if (e_cond) { ...;  e_incr } else { break; }; ... *)
      | _, _, ELet (_, {
          node = EWhile ({ node = EBool true; _ }, {
            node = EIfThenElse (e_cond, e_then, { node = EBreak; _ }); _
          }); _ }, e2) when not (found 1 e2)
      ->
        begin match find_terminal_incr 0 e_then with
        | Some (e_then, e_incr) ->
            let e_then = self#visit_expr env e_then in
            let e2 = self#visit_expr env e2 in
            ELet (Krml.Helpers.sequence_binding (), with_type TUnit (
              EFor (b, e1, e_cond, e_incr, e_then)
            ), Krml.(DeBruijn.subst Helpers.eunit 0 e2))
        | None ->
            super#visit_ELet env b e1 e2
        end

      | _ -> super#visit_ELet env b e1 e2

    method! visit_EWhile env e1 e2 =
      match e1.node, e2.node with
      | EBool true, EIfThenElse (e_cond, e_then, { node = EBreak; _ }) ->
          EWhile (e_cond, self#visit_expr env e_then)
      | _ ->
          super#visit_EWhile env e1 e2
  end

