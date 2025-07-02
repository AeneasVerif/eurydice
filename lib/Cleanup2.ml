open Krml.Ast
open Krml.DeBruijn
module H = Krml.Helpers
module L = Logging
open Krml.PrintAst.Ops

(* Target cleanups invoked from bin/main.ml *)

(* A general phase that removes assignments at array types. Note that all array repeat expressions
   are desugared before this. We try to reduce all cases to the assignment case (e1 := e2). *)
let remove_implicit_array_copies =
  object (self)
    inherit [_] map as super

    (* Desugar `lhs := rhs in e2`, because `rhs` has an array type and `not (is_suitable_initializer rhs)`. *)
    method private remove_assign n lhs rhs e2 =
      (* Krml.KPrint.bprintf "remove_assign %a := %a\n" pexpr lhs pexpr rhs; *)
      let is_array = function
        | TArray _ -> true
        | _ -> false
      in

      (* What are we trying to assign? *)
      match rhs.node with
      | EBufCreateL (Stack, es) ->
          if List.for_all (( = ) (List.hd es)) es && not (is_array (List.hd es).typ) then
            (* We assign a list of elements that are all identical -- optimize *)
            let lift = Krml.DeBruijn.lift 1 in
            ELet
              ( H.sequence_binding (),
                H.with_unit (EBufFill (lhs, List.hd es, Krml.Helpers.mk_uint32 (List.length es))),
                lift e2 )
          else begin
            (* lhs := bufcreatel e1, e2, ... ~~> lhs[0] := e1, lhs[1] := e2, ...
               we possibly recurse if the type of elements is an array *)
            assert (List.length es = int_of_string (snd n));
            let lift = Krml.DeBruijn.lift in
            let rec nest lifting_index array_index es =
              match es with
              | [] -> lift lifting_index (self#visit_expr_w () e2)
              | e :: es -> (
                  let array_index_ =
                    with_type H.usize (EConstant (SizeT, string_of_int array_index))
                  in
                  let lhs_i =
                    with_type (H.assert_tbuf_or_tarray lhs.typ) (EBufRead (lhs, array_index_))
                  in
                  match e.typ with
                  | TArray (_, n) ->
                      with_type e2.typ
                        (self#remove_assign n (lift lifting_index lhs_i) (lift lifting_index e)
                           (nest lifting_index (array_index + 1) es))
                  | _ ->
                      with_type e2.typ
                        (ELet
                           ( H.sequence_binding (),
                             H.with_unit (EAssign (lift lifting_index lhs_i, lift lifting_index e)),
                             nest (lifting_index + 1) (array_index + 1) es )))
            in
            (nest 0 0 es).node
          end
      | _ ->
          (* Something else, e.g. a variable -- generate a memcpy *)
          let zero = Krml.(Helpers.zero Constant.SizeT) in
          let rhs = self#visit_expr_w () rhs in
          let lhs = self#visit_expr_w () lhs in
          ELet
            ( H.sequence_binding (),
              H.with_unit (EBufBlit (rhs, zero, lhs, zero, PreCleanup.expr_of_constant n)),
              lift 1 (self#visit_expr_w () e2) )

    method! visit_ELet (((), _) as env) b e1 e2 =
      let rec subarrays_only_literals e =
        (* Underneath an initializer list *)
        match e.typ with
        | TArray _ ->
            (* In the case of nested arrays *)
            begin
              match e.node with
              | EBufCreateL (_, es) ->
                  (* We only allow sub-initializer lists *)
                  List.for_all subarrays_only_literals es
              | _ ->
                  (* Anything else (e.g. variable) is not copy-assignment in C *)
                  false
            end
        | _ ->
            (* If this is not a nested array, then anything goes *)
            true
      in
      let is_suitable_initializer = function
        | EAny | EBufCreate _ -> true
        | EBufCreateL (_, es) -> List.for_all subarrays_only_literals es
        | _ -> false
      in
      match b.typ, e1.node with
      (* INVALID INITIALIZATION: let b = e1 in e2 -- explode into assignments, recursively *)
      | TArray (_, n), _ when not (is_suitable_initializer e1.node) ->
          (* let b = <uninitialized> in *)
          ELet
            ( b,
              H.any,
              (* b := *)
              with_type e2.typ
                (self#remove_assign n (with_type b.typ (EBound 0)) (Krml.DeBruijn.lift 1 e1)
                   (* e2 *)
                   (self#visit_expr env e2)) )
      (* COPY: let _ = lhs := rhs with lhs.typ == TArray _ ... *)
      | _, EAssign (lhs, rhs) when H.is_array lhs.typ ->
          let n =
            match lhs.typ with
            | TArray (_, n) -> n
            | _ -> failwith "impossible"
          in
          (* Fixpoint here for multi-dimensional arrays. *)
          self#remove_assign n lhs rhs (subst H.eunit 0 e2)
      | _ -> super#visit_ELet env b e1 e2

    method! visit_EAssign env lhs rhs =
      (* COPY: lhs := rhs with lhs.typ == TArray _ ... *)
      match lhs.typ with
      | TArray (_, n) -> self#remove_assign n lhs rhs H.eunit
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

(* We remove array repeat expressions.

   Such expressions might occur in any position; we rewrite everything into a
   single form let x = [e; n] with a let-binding. (The later hoist phase is
   going to do this anyhow, so we might as well do it now for simplicity.)

   - If the array repeat is made up of zeroes, or array repeats made of zeroes,
     then we generate (complete) initializer lists that the subsequent code-gen
     in CStarToC will be able to emit as = { 0 } (note that this works because
     we ensure such expressions are let-bound.) We could expand and generate
     EBufCreateL nodes for *any* array repeat whose bounds are statically known,
     but that's generally a bad idea.
   - If the array repeat is of a /simple form/ (i.e., e is a scalar value), then
     we use the BufCreate node, to be emitted later on (also in CStarToC) as a
     zero-initializer, a memset, or a for-loop.
   - Barring that, we use a for-loop and recurse.

   This happens BEFORE remove_implicit_array_copies above. *)
let remove_array_repeats =
  object (self)
    inherit [_] map as super

    method! visit_EApp env e es =
      (* This is the case where the declaration is not in let-binding position. This happens with
         e.g. `fn init() -> ... { [0; 32] }`. *)
      match e.node, es with
      | ETApp ({ node = EQualified lid; _ }, [ _ ], _, [ _ ]), [ _ ]
        when lid = Builtin.array_repeat.name ->
          (self#visit_expr env
             (with_type (snd env)
                (ELet
                   ( H.fresh_binder "repeat_expression" (snd env),
                     with_type (snd env) (EApp (e, es)),
                     with_type (snd env) (EBound 0) ))))
            .node
      | _ -> super#visit_EApp env e es

    method private assert_length len =
      match len.node with
      | EConstant (_, s) -> int_of_string s
      | _ -> failwith "impossible"

    (* This function recursively expands nested repeat expressions as initializer lists
       (EBufCreateL) as long as the innermost initial value is a zero, otherwise, it throws
       Not_found. For instance:
       - [[0; 2]; 2] --> { {0, 0}, {0, 0} }.
       - [[1; 2]; 2] --> error -- better code quality with a BufCreate expression which will give
         rise to a for-loop initializer

       We override this behavior when we're already underneath an EBufCreateL -- here, we've already
       committed to an initializer list (and Charon will suitably "fold" repeat expressions
       automatically for us), so we might as well expand.
    *)
    method private expand_repeat under_bufcreate e =
      match e.node with
      | EApp
          ( { node = ETApp ({ node = EQualified lid; _ }, [ len ], _, [ _ ]); _ },
            [ ({ node = EConstant (_, "0"); _ } as init) ] )
        when lid = Builtin.array_repeat.name ->
          (* [0; n] -> ok *)
          with_type e.typ @@ EBufCreateL (Stack, List.init (self#assert_length len) (fun _ -> init))
      | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ len ], _, [ _ ]); _ }, [ init ])
        when lid = Builtin.array_repeat.name ->
          (* [e; n] -> ok if e ok -- n is a constant here Rust has no VLAs *)
          let init = self#expand_repeat under_bufcreate init in
          with_type e.typ @@ EBufCreateL (Stack, List.init (self#assert_length len) (fun _ -> init))
      | _ ->
          if under_bufcreate then
            e
          else
            raise Not_found

    method! visit_DGlobal env flags name n t e1 =
      match e1.node with
      | EBufCreateL (l, es) ->
          DGlobal
            ( flags,
              name,
              n,
              t,
              with_type e1.typ (EBufCreateL (l, List.map (self#expand_repeat true) es)) )
      | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ len ], _, [ _ ]); _ }, [ init ])
        when lid = Builtin.array_repeat.name -> begin
          try
            (* Case 1. *)
            DGlobal (flags, name, n, t, (self#expand_repeat false) e1)
          with Not_found -> (
            match init.node with
            | EConstant _ ->
                (* Case 2. *)
                DGlobal
                  ( flags,
                    name,
                    n,
                    t,
                    with_type e1.typ
                      (EBufCreate (Stack, init, Krml.Helpers.mk_sizet (self#assert_length len))) )
            | _ -> super#visit_DGlobal env flags name n t e1)
        end
      | _ -> super#visit_DGlobal env flags name n t e1

    method! visit_ELet (((), _) as env) b e1 e2 =
      match e1.node with
      (* Nothing special here for EBufCreateL otherwise it breaks the invariant expected by
         remove_implicit_array_copies *)
      | EApp ({ node = ETApp ({ node = EQualified lid; _ }, [ len ], _, [ _ ]); _ }, [ init ])
        when lid = Builtin.array_repeat.name -> begin
          try
            (* Case 1. *)
            let r = ELet (b, (self#expand_repeat false) e1, self#visit_expr env e2) in
            r
          with Not_found -> (
            match init.node with
            | EConstant _ ->
                (* Case 2. *)
                let e1 =
                  with_type e1.typ
                    (EBufCreate (Stack, init, Krml.Helpers.mk_sizet (self#assert_length len)))
                in
                ELet (b, e1, self#visit_expr env e2)
            | _ ->
                (* Case 3. *)

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
                           Krml.DeBruijn.lift 1 (self#visit_expr env e2) )) ))
        end
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
              [ call_mut; _call_once ],
              [ t_elements; _t_captured_state ] ) ->
            (* Same as below, but catching the case where the type of elements is an
               array and has undergone outparam optimization (i.e. the closure,
               instead of having type size_t -> t_element, has type size_t -> t_element ->
               unit *)
            L.log "Cleanup2" "%a %a" ptyp t_elements ptyp t_elements;
            (* First argument = closure, second argument = destination. Note that
               the closure may itself be an application of the closure to the state
               (but not always... is this unit argument elimination kicking in? not
               sure). *)
            let state, dst = match es with [ x; y ] -> x, y | _ -> assert false in
            let lift1 = Krml.DeBruijn.lift 1 in
            let t_dst = H.assert_tbuf_or_tarray dst.typ in
            EFor
              ( Krml.Helpers.fresh_binder ~mut:true "i" H.usize,
                H.zero_usize (* i: size_t = 0 *),
                H.mk_lt_usize (Krml.DeBruijn.lift 1 len) (* i < len *),
                H.mk_incr_usize (* i++ *),
                let i = with_type H.usize (EBound 0) in
                Krml.Helpers.with_unit (
                  if H.is_array t_dst then
                    EApp (call_mut, [
                      with_type (TBuf (state.typ, false)) (EAddrOf (lift1 state));
                      with_type (TInt SizeT) (EBound 0);
                      with_type t_dst (EBufRead (lift1 dst, i))
                    ])
                  else
                    EBufWrite (Krml.DeBruijn.lift 1 dst, i,
                      with_type t_dst (EApp (call_mut, [
                        with_type (TBuf (state.typ, false)) (EAddrOf (lift1 state));
                        with_type (TInt SizeT) (EBound 0)
                      ]))
                    )))

        | ETApp ({ node = EQualified ("core" :: "array" :: _, "map"); _ }, [ len ], [ call_mut; _call_once], ts) ->
            let t_src, t_dst =
              match ts with
              | [ t_src; t_state; t_dst ] ->
                  assert (t_state = TUnit);
                  L.log "Cleanup2" "found array map from %a to %a" ptyp t_src ptyp t_dst;
                  t_src, t_dst
              | _ ->
                  failwith "TODO: unknown map closure shape; is it an array outparam? (see above)"
            in
            let e_src, e_state, e_dst =
              match es with
              | [ e_src; e_state; e_dst ] -> e_src, e_state, e_dst
              | _ -> failwith "unknown shape of arguments to array map"
            in
            let lift1 = Krml.DeBruijn.lift 1 in
            let e_state = with_type (TBuf (e_state.typ, false)) (EAddrOf (lift1 e_state)) in
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
                       with_type t_dst (EApp (call_mut, [ lift1 e_state; e_src_i ])) ))
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

(* This is not a good criterion -- way too conservative! *)
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

let remove_literals tbl =
  object (_self)
    inherit [_] map as super_map
    inherit! Krml.Structs.remove_literals tbl as super_krml

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

    method! visit_DGlobal _env flags name n t body =
      (* No point: can't have let-bindings in globals *)
      DGlobal (flags, name, n, t, body)
  end

let remove_literals files =
  (remove_literals (Krml.Structs.build_remove_literals_map files))#visit_files () files

let build_macros (macros : Krml.Idents.LidSet.t ref) =
  object (_self)
    inherit [_] map as super

    method! visit_DGlobal env flags name n t body =
      if List.exists (fun x -> x = Krml.Common.Macro) flags then
        macros := Krml.Idents.LidSet.(union !macros (singleton name));
      super#visit_DGlobal env flags name n t body
  end

let build_macros files =
  let map = ref Krml.Idents.LidSet.empty in
  let files = (build_macros map)#visit_files () files in
  files, !map

let resugar_loops =
  object(self)
  inherit [_] map as super

  method! visit_expr ((), _ as env) e =
    (* Non-terminal position (step-by for-loop) *)
    match e with
    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter<
          core::iter::adapters::step_by::StepBy<core::ops::range::Range<?..>>,
          ?..
        >(core::iter::range::?::step_by<?..>(
          { start: ?e_start, end: ?e_end },
          ?e_increment
        ));
      while true {
        let x = core::iter::adapters::step_by::?::next<?, ?t1>(&iter);
        match x {
          None -> break,
          Some ? -> ?e_body
        }
      };
      ?rest..
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      let e_some_i = with_type (Builtin.mk_option t1) (ECons ("Some", [with_type t1 (EBound 0)])) in
      with_type e.typ @@ ESequence (with_type TUnit (EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        (* XXX seems like the increment is always size_t here ?! *)
        mk_incr_e w (with_type t1 (ECast (e_increment, t1))),
        self#visit_expr env (Krml.DeBruijn.subst e_some_i 0 e_body))) ::
        List.map (fun e -> self#visit_expr env (Krml.DeBruijn.subst eunit 0 e)) rest)

    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter<
          core::iter::adapters::step_by::StepBy<core::ops::range::Range<?..>>,
          ?..
        >(core::iter::range::?::step_by<?..>(
          { start: ?e_start, end: ?e_end },
          ?e_increment
        ));
      while true {
        match (core::iter::adapters::step_by::?::next<?, ?t1>(&iter)) {
          None -> break,
          Some ? -> ?e_body
        }
      };
      ?rest..
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      with_type e.typ @@ ESequence (with_type TUnit (EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        (* XXX seems like the increment is always size_t here ?! *)
        mk_incr_e w (with_type t1 (ECast (e_increment, t1))),
        self#visit_expr env e_body)) ::
        List.map (fun e -> self#visit_expr env (Krml.DeBruijn.subst eunit 0 e)) rest)

    (* Terminal position (step-by for-loop) *)
    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter<
          core::iter::adapters::step_by::StepBy<core::ops::range::Range<?..>>,
          ?..
        >(core::iter::range::?::step_by<?..>(
          { start: ?e_start, end: ?e_end },
          ?e_increment
        ));
      while true {
        let x = core::iter::adapters::step_by::?::next<?, ?t1>(&iter);
        match x {
          None -> break,
          Some ? -> ?e_body
        }
      }
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      let e_some_i = with_type (Builtin.mk_option t1) (ECons ("Some", [with_type t1 (EBound 0)])) in
      with_type e.typ @@ EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        (* XXX seems like the increment is always size_t here ?! *)
        mk_incr_e w (with_type t1 (ECast (e_increment, t1))),
        self#visit_expr env (Krml.DeBruijn.subst e_some_i 0 e_body))

    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter<
          core::iter::adapters::step_by::StepBy<core::ops::range::Range<?..>>,
          ?..
        >(core::iter::range::?::step_by<?..>(
          { start: ?e_start, end: ?e_end },
          ?e_increment
        ));
      while true {
        match (core::iter::adapters::step_by::?::next<?, ?t1>(&iter)) {
          None -> break,
          Some ? -> ?e_body
        }
      }
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      with_type e.typ @@ EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        (* XXX seems like the increment is always size_t here ?! *)
        mk_incr_e w (with_type t1 (ECast (e_increment, t1))),
        self#visit_expr env e_body)

    (* Terminal position (regular range for-loop) *)
    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter
          <core::ops::range::Range<?..>, ?..>
          ({ start: ?e_start, end: ?e_end });
      while true {
        let x = core::iter::range::?::next<?t1>(&iter);
        match x {
          None -> break,
          Some ? -> ?e_body
        }
      }
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      let e_some_i = with_type (Builtin.mk_option t1) (ECons ("Some", [with_type t1 (EBound 0)])) in
      with_type e.typ @@ EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        mk_incr w,
        self#visit_expr env (Krml.DeBruijn.subst e_some_i 0 e_body))

    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter
          <core::ops::range::Range<?..>, ?..>
          ({ start: ?e_start, end: ?e_end });
      while true {
        match (core::iter::range::?::next<?t1>(&iter)) {
          None -> break,
          Some ? -> ?e_body
        }
      }
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      with_type e.typ @@ EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        mk_incr w,
        self#visit_expr env e_body)

    (* Non-terminal position (regular range for-loop) *)
    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter
          <core::ops::range::Range<?>, ?..>
          ({ start: ?e_start, end: ?e_end });
      while true {
        let x = core::iter::range::?::next<?t1>(&iter);
        match x {
          None -> break,
          Some ? -> ?e_body
        }
      };
      ?rest..
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      let e_some_i = with_type (Builtin.mk_option t1) (ECons ("Some", [with_type t1 (EBound 0)])) in
      with_type e.typ @@ ESequence (with_type TUnit (EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        mk_incr w,
        self#visit_expr env (Krml.DeBruijn.subst e_some_i 0 e_body))
      ) :: List.map (fun e -> self#visit_expr env (Krml.DeBruijn.subst eunit 0 e)) rest)

    | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter
          <core::ops::range::Range<?>, ?..>
          ({ start: ?e_start, end: ?e_end });
      while true {
        match (core::iter::range::?::next<?t1>(&iter)) {
          None -> break,
          Some ? -> ?e_body
        }
      };
      ?rest..
    |}] ->
      let open Krml.Helpers in
      let w = match t1 with TInt w -> w | _ -> assert false in
      with_type e.typ @@ ESequence (with_type TUnit (EFor (fresh_binder ~mut:true "i" t1,
        e_start,
        mk_lt w (Krml.DeBruijn.lift 1 e_end),
        mk_incr w,
        self#visit_expr env e_body)
      ) :: List.map (fun e -> self#visit_expr env (Krml.DeBruijn.subst eunit 0 e)) rest)

    | _ ->
      super#visit_expr env e


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
           Krml.Monomorphization.maybe_debug_hash hash
             (lazy PPrint.(string "trait impl:" ^/^ string trait_impl));
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
          (* if e1 then () else abort msg -->  static_assert(e1, msg) *)
          EApp
            ( Builtin.static_assert_ref,
              [ e1; with_type Krml.Checker.c_string (EString (Option.value ~default:"" msg)) ] )
      | TBool, EAbort (_, msg), EUnit ->
          (* if not (e1) then abort msg else ()  -->  static_assert(e1, msg) *)
          EApp
            ( Builtin.static_assert_ref,
              [
                Krml.Helpers.mk_not e1;
                with_type Krml.Checker.c_string (EString (Option.value ~default:"" msg));
              ] )
      | _ -> super#visit_EIfThenElse env e1 e2 e3
  end

(* Reconstructing for-loops from while nodes introduced by c_for!. *)

class iter_counting =
  object
    (* The environment [i] has type [int]. *)
    inherit [_] iter

    (* The environment [i] keeps track of how many binders have been
       entered. It is incremented at each binder. *)
    method! extend i (_ : binder) = i + 1
  end

(* De Bruijn index i is found in expression e *)
let found i e =
  let exception Found in
  let find =
    object
      inherit iter_counting

      method! visit_EBound (i, _) j =
        if i = j then
          raise Found
    end
  in
  try
    find#visit_expr_w i e;
    false
  with Found -> true

let smallest =
  object
    inherit [_] reduce
    method zero = max_int
    method plus x y = min x y
    method visit_EBound _ i = i
  end

let rec find_terminal_incr i e =
  if e.typ <> TUnit && e.typ <> TAny then
    Krml.Warn.failwith "e_then has type: %a\n" ptyp e.typ;
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
  let no_control_flow (e : expr) =
    match e.node with
    | EWhile _ | EFor _ | ELet _ | EFun _ | EIfThenElse _ | ESequence _ | EMatch _ | ESwitch _ ->
        false
    | _ -> true
  in

  object (self)
    inherit [_] map as super

    method! visit_ELet (((), _) as env) b e1 e2 =
      match e1.node, e1.typ, e2.node with
      (* t x = e1; while (true) { if (e_cond) { ...;  e_incr } else { break; } *)
      | ( _,
          _,
          EWhile
            ( { node = EBool true; _ },
              { node = EIfThenElse (e_cond, e_then, { node = EBreak; _ }); _ } ) ) -> begin
          match find_terminal_incr 0 e_then with
          | Some (e_then, e_incr) when no_control_flow e_incr ->
              let e_then = self#visit_expr env e_then in
              EFor (b, e1, e_cond, e_incr, e_then)
          | _ -> super#visit_ELet env b e1 e2
        end
      (* let t x = e1 in
         let _ = while (true) { if (e_cond) { e_then; e_incr } else { break; } in
         e2
         ~~~>
         let _ = for (t x = e1; e_cond; e_incr) { e_then } in
         e2 *)
      | ( _,
          _,
          ELet
            ( _,
              {
                node =
                  EWhile
                    ( { node = EBool true; _ },
                      { node = EIfThenElse (e_cond, e_then, { node = EBreak; _ }); _ } );
                _;
              },
              e2' ) )
        when not (found 1 e2') -> begin
          match find_terminal_incr 0 e_then with
          | Some (e_then, e_incr) when no_control_flow e_incr ->
              let e_then = self#visit_expr env e_then in
              let e2 = self#visit_expr env e2' in
              let shift1 = Krml.(DeBruijn.subst Helpers.eunit 0) in
              ELet
                ( Krml.Helpers.sequence_binding (),
                  with_type TUnit (EFor (b, e1, e_cond, e_incr, e_then)),
                  shift1 e2 )
          | _ -> super#visit_ELet env b e1 e2
        end
      | _ -> super#visit_ELet env b e1 e2

    method! visit_EWhile env e1 e2 =
      (* while (true) { if (e_cond) { e_then } else { break } } ~~>
         while (e_cond) { e_then } *)
      match e1.node, e2.node with
      | EBool true, EIfThenElse (e_cond, e_then, { node = EBreak; _ }) ->
          EWhile (e_cond, self#visit_expr env e_then)
      | _ -> super#visit_EWhile env e1 e2

    (*     method! visit_DFunction _ cc flags n_cgs n t name bs e = *)
    (*       Krml.KPrint.bprintf "for-loop reconstruction: visiting %a\n" plid name; *)
    (*       super#visit_DFunction () cc flags n_cgs n t name bs e *)
  end

let remove_assign_return =
  object (self)
    inherit [_] map as super

    method! visit_ESequence (((), _) as env) es =
      match List.rev es with
      | { node = EReturn { node = EBound i; _ }; typ = t; _ }
        :: { node = EAssign ({ node = EBound i'; _ }, e); _ }
        :: es
        when i = i' ->
          ESequence (List.rev (with_type t (EReturn e) :: List.map (self#visit_expr env) es))
      | { node = EBound i; _ } :: { node = EAssign ({ node = EBound i'; _ }, e); _ } :: es
        when i = i' -> ESequence (List.rev (e :: List.map (self#visit_expr env) es))
      | _ -> super#visit_ESequence env es
  end

let bonus_cleanups =
  let open Krml in
  object (self)
    inherit [_] map as super
    method! extend env b = b.node.name :: env

    method! visit_lident _ lid =
      match lid with
      | [ "core"; "slice"; "{@Slice<T>}" ], "len" -> [ "Eurydice" ], "slice_len"
      | [ "core"; "slice"; "{@Slice<T>}" ], "copy_from_slice" -> [ "Eurydice" ], "slice_copy"
      | [ "core"; "slice"; "{@Slice<T>}" ], "split_at" -> [ "Eurydice" ], "slice_split_at"
      | [ "core"; "slice"; "{@Slice<T>}" ], "split_at_mut" -> [ "Eurydice" ], "slice_split_at_mut"
      | _ -> lid

    method! visit_ELet ((bs, _) as env) b e1 e2 =
      match e1.node, e1.typ, e2.node with
      (* let x; x := e; return x  -->  x*)
      | ( EAny,
          _,
          ESequence [ { node = EAssign ({ node = EBound 0; _ }, e3); _ }; { node = EBound 0; _ } ] )
        -> (DeBruijn.subst Helpers.eunit 0 e3).node
      (* let uu; memcpy(uu, ..., src, ...); e2  -->  let copy_of_src; ... *)
      | ( EAny,
          TArray (_, (_, n)),
          ESequence
            [
              {
                node =
                  EBufBlit
                    ( { node = EBound src; _ },
                      { node = EConstant (_, "0"); _ },
                      { node = EBound 0; _ },
                      { node = EConstant (_, "0"); _ },
                      { node = EConstant (_, n'); _ } );
                _;
              };
              _;
            ] )
        when n = n' && Krml.Helpers.is_uu b.node.name ->
          super#visit_ELet env
            {
              b with
              node = { b.node with name = "copy_of_" ^ List.nth bs (src - 1) };
              meta = [ CommentBefore "Passing arrays by value in Rust generates a copy in C" ];
            }
            e1 e2
      (* let uu = f(e); y = uu; e2  -->  let y = f(e); e2 *)
      | ( EApp ({ node = EQualified _; _ }, es),
          _,
          ESequence [ { node = EAssign (e2, { node = EBound 0; _ }); _ }; e3 ] )
        when Helpers.is_uu b.node.name && List.for_all Helpers.is_readonly_c_expression es ->
          ESequence
            [
              with_type TUnit (EAssign (DeBruijn.subst Helpers.eunit 0 e2, e1));
              self#visit_expr env (DeBruijn.subst Helpers.eunit 0 e3);
            ]
      | _ -> super#visit_ELet env b e1 e2
  end

(* This is a potentially tricky phase because if it's too aggressive, it'll
   generate a copy -- for instance, f(&x[3]) is not the same as let tmp = x[3];
   f(&tmp). Such cases might be hidden behind macros! (Like
   Eurydice_slice_index.) *)
let check_addrof =
  object (self)
    inherit [_] map

    method! visit_EAddrOf ((), t) e =
      (* see https://en.cppreference.com/w/c/language/operator_member_access *)
      match e.node with
      | EQualified _ (* case 1 *) | EBufRead _ (* case 4 *) -> EAddrOf (self#visit_expr_w () e)
      | EApp ({ node = EQualified lid; _ }, _)
      | EApp ({ node = ETApp ({ node = EQualified lid; _ }, _, _, _); _ }, _)
        when lid = Builtin.slice_index.name
             || Krml.KString.starts_with (snd lid) "op_Bang_Star__" (* case 4, case 3 *) ->
          EAddrOf e
      | _ ->
          if Krml.Structs.will_be_lvalue e then
            EAddrOf e
          else
            let b = Krml.Helpers.fresh_binder ~mut:true "lvalue" e.typ in
            let b =
              {
                b with
                Krml.Ast.meta = [ CommentBefore "original Rust expression is not an lvalue in C" ];
              }
            in
            (* Recursively do for the internal expression *)
            let e = self#visit_expr_w () e in
            ELet (b, e, with_type t (EAddrOf (with_type e.typ (EBound 0))))
  end

(* Aeneas requires hoisting loop bodies into separate functions. *)
let is_inline_loop lid = Krml.KString.exists (snd lid) "inner_loop"

let return_becomes_break =
  object
    inherit [_] Krml.Ast.map as super
    method! visit_EReturn _ _ = EBreak
    method! visit_EFor _ _ _ _ = failwith "nested loop in a loop body"

    method! visit_EApp env e es =
      match e.node with
      | EQualified lid when is_inline_loop lid -> failwith "nested loop in a loop body"
      | _ -> super#visit_EApp env e es
  end

let inline_loops =
  object
    inherit [_] Krml.Ast.map

    method! visit_DFunction () cc flags n_cgs n t name binders body =
      if is_inline_loop name then
        DFunction
          ( cc,
            [ Krml.Common.MustInline; MustDisappear ] @ flags,
            n_cgs,
            n,
            t,
            name,
            binders,
            return_becomes_break#visit_expr_w () body )
      else
        DFunction (cc, flags, n_cgs, n, t, name, binders, body)
  end

(** A better version of hoist (than [Krml.Simplify.hoist]), also work for [DGlobal]. *)
let hoist = let open Krml in object(self)
  inherit [_] map as super

  method hoist_top_level loc name binders expr =
    let loc = Loc.(InTop name :: loc) in
    (* TODO: no nested let-bindings in top-level value declarations either *)
    let binders, expr = open_binders binders expr in
    let expr = Krml.Simplify.hoist_stmt loc expr in
    close_binders binders expr

  method! visit_file loc file =
    super#visit_file Loc.(File (fst file) :: loc) file

  method! visit_DFunction loc cc flags n_cgs n ret name binders expr =
    let expr = self#hoist_top_level loc name binders expr in
    DFunction (cc, flags, n_cgs, n, ret, name, binders, expr)

  method! visit_DGlobal loc flags name n ret expr =
    let expr = self#hoist_top_level loc name [] expr in
    DGlobal (flags, name, n, ret, expr)
end

(** Also fix for [DGlobal] as [hoist] above *)
let fixup_hoist = object
  inherit [_] map

  method! visit_DFunction _ cc flags n_cgs n ret name binders expr =
    DFunction (cc, flags, n_cgs, n, ret, name, binders, Krml.Simplify.fixup_return_pos expr)

  method! visit_DGlobal () flags name n ret expr =
    DGlobal (flags, name, n, ret, Krml.Simplify.fixup_return_pos expr)
end

(** For any [DGlobal], if the expression has any locals remaining
    We should let them also be globals, so to make the overall expr valid.

    I.e., we make:
    [T VAL = let v1 : T1 = e1 in
             let v2 : T2 = e2 in
             ...
             let vN : TN = eN in
             e;]
    become:
    [T1 VAL_local_1 = e1;
     T2 VAL_local_2 = e2[v1/VAL_local_1];
     ...
     TN VAL_local_N = eN[v1/VAL_local_1; v2/VAL_local_2; ...; vN-1/VAL_local_(N-1)];
     T VAL = e;]

     Notably, the locals should be renamed to avoid potential naming conflicts.
*)
let globalise_global_locals files =
  let mapper = function
  | DGlobal (flags, name, n_cgs, ty, expr) ->
    let rec decompose_expr id infoAcc expr =
      match expr.node with
      | ELet (_, e1, e2) ->
        let name =
          let lst,name = name in
          lst, name ^ "$local$" ^ string_of_int id
        in
        (* Replace the variable with the new globalised name. *)
        let e2 = subst Krml.Ast.(with_type e1.typ (EQualified name)) 0 e2 in
        decompose_expr (id + 1) ((name, e1) :: infoAcc) e2
      | _ -> List.rev infoAcc, expr
    in
    let info, expr = decompose_expr 0 [] expr in
    (* Make the new globals private if possible -- with exception that
       if it is a non-private macro, then the involved new globals should not be private *)
    let module NameSet = Krml.Idents.LidSet in
    let no_priv_names =
      if List.mem Krml.Common.Macro flags && 
         not (List.mem Krml.Common.Private flags) then
        object
          inherit [_] reduce
          method private zero = NameSet.empty
          method private plus = NameSet.union
          method! visit_EQualified _ name = NameSet.singleton name
        end#visit_expr_w () expr
      else NameSet.empty
    in
    let make_decl (name, expr) =
      let flags =
        if NameSet.mem name no_priv_names then []
        else [ Krml.Common.Private ] in
      DGlobal (flags, name, n_cgs, expr.typ, expr)
    in
    List.map make_decl info @
    [DGlobal (flags, name, n_cgs, ty, expr)]
  | decl -> [decl]
  in
  List.map (fun (name, decls) -> (name, List.concat_map mapper decls)) files

let fixup_monomorphization_map map =
  let replace = object(self)
    inherit [_] Krml.Ast.map
    method! visit_TQualified () lid =
      match Hashtbl.find_opt map lid with
      | Some (Krml.DataTypes.Eliminate t) ->
          self#visit_typ () t
      | _ ->
          TQualified lid
  end in
  Seq.iter (fun ((lid, ts, cgs), v) ->
    let ts = List.map (replace#visit_typ ()) ts in
    Hashtbl.add Krml.MonomorphizationState.state (lid, ts, cgs) v
  ) (Hashtbl.to_seq Krml.MonomorphizationState.state)


