open Krml.Ast
open Krml.DeBruijn
module H = Krml.Helpers
module AtomMap = Map.Make (Krml.Atom)
module AtomSet = Set.Make (Krml.Atom)

let set_of_map_keys m = AtomSet.of_list (List.map fst (AtomMap.bindings m))

let count_atoms =
  object
    inherit [_] reduce
    method private zero = AtomSet.empty
    method private plus = AtomSet.union
    method! visit_EOpen _ _ a = AtomSet.singleton a
  end

type remove_env = (string * typ * node_meta list * meta list) AtomMap.t

let pmeta buf ({ meta; _ } : 'a with_type) =
  List.iter
    (function
      | CommentBefore s | CommentAfter s ->
          Buffer.add_string buf s;
          Buffer.add_char buf '\n')
    meta

let mk typ meta node = { node; typ; meta }
let is_sequence = Krml.Simplify.is_sequence

let already_clean = function
  | [ "core"; "slice"; _ ], "swap" -> true
  | [ "alloc"; "vec"; _ ], "try_with_capacity" -> true
  | _ -> false

let remove_assignments =
  object (self)
    inherit [_] map

    method private peel_lets (to_close : remove_env) e =
      match e.node with
      | ELet (b, e1, e2) ->
          (if not (e1.node = EAny || e1.node = EUnit) then
             Krml.(Warn.fatal_error "Initializer of let-binding is %a" PrintAst.Ops.pexpr e1));
          (* Krml.(KPrint.bprintf "peeling %s\n" b.node.name); *)
          let b, e2 = open_binder b e2 in
          (* Krml.KPrint.bprintf "peel: let-binding meta %a\n" pmeta b; *)
          let to_close =
            AtomMap.add b.node.atom (b.node.name, b.typ, b.meta, b.node.meta) to_close
          in
          self#peel_lets to_close e2
      | _ ->
          let e = Krml.Simplify.sequence_to_let#visit_expr_w () e in
          (* Krml.(KPrint.bprintf "after peeling:\n%a" PrintAst.Ops.ppexpr e); *)
          self#visit_expr_w to_close e

    method! visit_DFunction (to_close : remove_env) cc flags n_cgs n t name bs e =
      (* Krml.(KPrint.bprintf "visiting %a\n" PrintAst.Ops.plid name); *)
      assert (AtomMap.is_empty to_close);
      DFunction
        ( cc,
          flags,
          n_cgs,
          n,
          t,
          name,
          bs,
          if already_clean name then
            e
          else
            self#peel_lets to_close e )

    method! visit_DGlobal (to_close : remove_env) flags n t name e =
      assert (AtomMap.is_empty to_close);
      DGlobal (flags, n, t, name, self#peel_lets to_close e)

    method! visit_ELet ((not_yet_closed : remove_env), t) b e1 e2 =
      (* If [not_yet_closed] represents the set of bindings that have yet to be
         closed (i.e. for which we have yet to insert a let-binding, as close as
         possible to the first use-site), and [candidates] represents the atoms
         that we know for sure must be closed right now, then [close_now_over]
         inserts suitable let-bindings for the candidates that have not yet been
         closed, then calls the continuation with the remaining subset of
         not_yet_closed. *)
      let close_now_over (not_yet_closed : remove_env) candidates mk_node =
        let to_close_now = AtomSet.inter candidates (set_of_map_keys not_yet_closed) in
        let bs = List.of_seq (AtomSet.to_seq to_close_now) in
        let bs =
          List.map
            (fun atom ->
              let name, typ, meta, binder_meta = AtomMap.find atom not_yet_closed in
              ( {
                  node = { atom; name; mut = true; mark = ref Krml.Mark.default; meta = binder_meta };
                  typ;
                  meta;
                },
                if typ = TUnit then
                  Krml.Helpers.eunit
                else
                  Krml.Helpers.any ))
            bs
        in
        (* For the subexpressions, we now need to insert declarations for those variables that we're
           not handling now. *)
        let not_yet_closed =
          AtomMap.filter (fun a _ -> not (AtomSet.mem a to_close_now)) not_yet_closed
        in
        let node = mk_node not_yet_closed in
        (* XXX why is the with_type necessary here?? *)
        (Krml.Helpers.nest bs t (with_type t node)).node
      in

      let count e = count_atoms#visit_expr_w () e in

      let ( ++ ) = AtomSet.union in

      (* Called when hitting a node in terminal position: either fall back into
         the general case if it's a let-node; special treatment if it's
         control-flow (match, if, while); otherwise, just close everything now and
         move on (wildcard case). *)
      let rec recurse_or_close (not_yet_closed : remove_env) e0 =
        let t = e0.typ in
        match e0.node with
        | ELet _ ->
            (* let node: restart logic and jump back to match below *)
            self#visit_expr_w not_yet_closed e0
        | EIfThenElse (e, e', e'') ->
            mk t e0.meta
            @@ close_now_over not_yet_closed
                 ((* We must now bind: *)
                  count e
                ++
                (* whichever variables were in the condition *)
                AtomSet.empty)
                 (* unlike below, we are in terminal position, so we do not need to
                    close immediately variables that appear in both branches -- we can simply declare them
                    twice in each branch! is this a better code-gen choice? yes, absolutely -- owing to
                    the structure of MIR, NOT doing this generates awful code *)
                 (fun not_yet_closed ->
                   EIfThenElse
                     ( self#visit_expr_w not_yet_closed e,
                       recurse_or_close not_yet_closed e',
                       recurse_or_close not_yet_closed e'' ))
        | EWhile (e, e') ->
            mk t e0.meta
            @@ close_now_over not_yet_closed (count e) (fun not_yet_closed ->
                   EWhile (self#visit_expr_w not_yet_closed e, recurse_or_close not_yet_closed e'))
        | ESwitch (e, branches) ->
            mk t e0.meta
            @@ close_now_over not_yet_closed
                 ((* We must now bind: *)
                  count e
                ++
                (* i.e., whichever variables were in the condition *)
                AtomSet.empty)
                 (* see above *)
                 (fun not_yet_closed ->
                   ESwitch
                     ( self#visit_expr_w not_yet_closed e,
                       List.map (fun (p, e) -> p, recurse_or_close not_yet_closed e) branches ))
        | EMatch (c, e, branches) ->
            mk t e0.meta
            @@ close_now_over not_yet_closed
                 ((* We must now bind: *)
                  count e
                ++
                (* i.e., whichever variables were in the condition *)
                AtomSet.empty)
                 (* see above *)
                 (fun not_yet_closed ->
                   EMatch
                     ( c,
                       self#visit_expr_w not_yet_closed e,
                       List.map
                         (fun (bs, p, e) -> bs, p, recurse_or_close not_yet_closed e)
                         branches ))
        | _ ->
            (* There are opportunities for finesse here, for instance, if we reach
               an assignment in terminal position, *and* the variable has yet to
               be closed, it means that the assignment is useless since no one
               else will be using the variable after that. *)
            mk t e0.meta
            @@ close_now_over not_yet_closed (count e0) (fun not_yet_closed ->
                   (* not_yet_closed should be empty at this stage *)
                   (self#visit_expr_w not_yet_closed e0).node)
      in

      match e1.node with
      | EAssign ({ node = EOpen (_, atom); _ }, e_rhs) when AtomMap.mem atom not_yet_closed ->
          close_now_over not_yet_closed (count e_rhs) (fun not_yet_closed ->
              (* Combined "close now" (above) + let-binding insertion in lieu of the assignment *)
              assert (is_sequence b.node.meta);
              let e2 = snd (open_binder b e2) in
              let name, typ, meta, binder_meta = AtomMap.find atom not_yet_closed in
              let b =
                {
                  node =
                    { atom; name; mut = true; mark = ref Krml.Mark.default; meta = binder_meta };
                  typ;
                  meta = meta @ e1.meta;
                }
              in
              let not_yet_closed = AtomMap.remove atom not_yet_closed in
              (* Krml.(KPrint.bprintf "rebuilt: %a\n" PrintAst.Ops.pexpr (with_type TUnit (ELet (b, e_rhs, e2)))); *)
              let e2 = self#visit_expr_w not_yet_closed (close_binder b e2) in
              ELet (b, e_rhs, e2))
      | EIfThenElse (e, e', e'') ->
          assert (is_sequence b.node.meta);
          close_now_over not_yet_closed
            ((* We must now bind: *)
             count e
            ++
            (* whichever variables were in the condition *)
            AtomSet.inter (count e') (count e'')
            ++
            (* variables that appear in both branches *)
            AtomSet.inter (count e' ++ count e'') (count e2))
            (* variables in either branch *and* used later *)
            (fun not_yet_closed ->
              ELet
                ( b,
                  mk e1.typ e1.meta
                    (EIfThenElse
                       ( self#visit_expr_w not_yet_closed e,
                         recurse_or_close not_yet_closed e',
                         recurse_or_close not_yet_closed e'' )),
                  recurse_or_close not_yet_closed e2 ))
      | EWhile (e, e') ->
          assert (is_sequence b.node.meta);
          close_now_over not_yet_closed
            (* We must be here variables that are declared in the condition, and variables that
               appear both in the loop body and its continuation. *)
            (count e ++ AtomSet.inter (count e') (count e2))
            (fun not_yet_closed ->
              ELet
                ( b,
                  mk e1.typ e1.meta
                    (EWhile (self#visit_expr_w not_yet_closed e, recurse_or_close not_yet_closed e')),
                  recurse_or_close not_yet_closed e2 ))
      | ESwitch (e, branches) ->
          assert (is_sequence b.node.meta);
          close_now_over not_yet_closed
            ((* We must now bind: *)
             count e
            ++
            (* i.e., whichever variables were in the condition *)
            Krml.KList.reduce AtomSet.inter (List.map (fun (_p, e) -> count e) branches)
            ++
            (* i.e., variables that appear in all branches -- note that
                  switches don't bind variables in their branches so it's simpler
                  than the match below*)
            AtomSet.inter
              (Krml.KList.reduce ( ++ ) (List.map (fun (_, e) -> count e) branches))
              (count e2))
            (* i.e., variables in either one of the branches *and* used later *)
            (fun not_yet_closed ->
              ELet
                ( b,
                  mk e1.typ e1.meta
                    (ESwitch
                       ( self#visit_expr_w not_yet_closed e,
                         List.map (fun (p, e) -> p, recurse_or_close not_yet_closed e) branches )),
                  recurse_or_close not_yet_closed e2 ))
      | EMatch (c, e, branches) ->
          assert (is_sequence b.node.meta);
          close_now_over not_yet_closed
            ((* We must now bind: *)
             count e
            ++
            (* i.e., whichever variables were in the condition *)
            Krml.KList.reduce AtomSet.inter (List.map (fun (_bs, _p, e) -> count e) branches)
            ++
            (* i.e., variables that appear in all branches -- note that we
                  don't open _bs meaning that we don't collect bound variables in this branch *)
            AtomSet.inter
              (Krml.KList.reduce ( ++ ) (List.map (fun (_, _, e) -> count e) branches))
              (count e2))
            (* i.e., variables in either one of the branches *and* used later *)
            (fun not_yet_closed ->
              ELet
                ( b,
                  mk e1.typ e1.meta
                    (EMatch
                       ( c,
                         self#visit_expr_w not_yet_closed e,
                         List.map
                           (fun (bs, p, e) -> bs, p, recurse_or_close not_yet_closed e)
                           branches )),
                  recurse_or_close not_yet_closed e2 ))
      | _ ->
          (* The open variables in e1 for which we have not yet inserted a declaration need to be closed now *)
          close_now_over not_yet_closed (count e1) (fun not_yet_closed ->
              ELet (b, self#visit_expr_w not_yet_closed e1, recurse_or_close not_yet_closed e2))
  end

let remove_units =
  object (self)
    inherit [_] map as super

    method! visit_EAssign env e1 e2 =
      if e1.typ = TUnit && Krml.Helpers.is_readonly_c_expression e2 then
        EUnit
      else if e1.typ = TUnit && Krml.Helpers.is_readonly_c_expression e1 then
        (* Unit nodes have been initialized at declaration-time by
           remove_assignments above. So, e1 := e2 can safely become e2. *)
        (self#visit_expr env e2).node
      else
        super#visit_EAssign env e1 e2
  end

let remove_terminal_returns =
  object (self)
    inherit [_] map

    method! visit_DFunction env cc flags n_cgs n t name bs e =
      DFunction
        ( cc,
          flags,
          n_cgs,
          n,
          t,
          name,
          bs,
          if already_clean name then
            e
          else
            self#visit_expr_w env e )

    method! visit_ELet (terminal, _) b e1 e2 =
      ELet (b, self#visit_expr_w false e1, self#visit_expr_w terminal e2)

    method! visit_EWhile _ e1 e2 = EWhile (self#visit_expr_w false e1, self#visit_expr_w false e2)

    method! visit_EFor _ b e1 e2 e3 e4 =
      EFor
        ( b,
          self#visit_expr_w false e1,
          self#visit_expr_w false e2,
          self#visit_expr_w false e3,
          self#visit_expr_w false e4 )

    method! visit_EReturn (terminal, _) e =
      if terminal then
        (self#visit_expr_w terminal e).node
      else
        EReturn (self#visit_expr_w terminal e)

    method! visit_ESequence _ _ = assert false

    method! visit_EIfThenElse (terminal, _) e1 e2 e3 =
      EIfThenElse
        (self#visit_expr_w false e1, self#visit_expr_w terminal e2, self#visit_expr_w terminal e3)

    method! visit_EMatch (terminal, _) f e branches =
      EMatch
        ( f,
          self#visit_expr_w false e,
          List.map (fun (b, p, e) -> b, p, self#visit_expr_w terminal e) branches )
  end

let remove_terminal_continues =
  object (self)
    inherit [_] map

    method! visit_DFunction env cc flags n_cgs n t name bs e =
      DFunction
        ( cc,
          flags,
          n_cgs,
          n,
          t,
          name,
          bs,
          if already_clean name then
            e
          else
            self#visit_expr_w env e )

    method! visit_ELet (terminal, _) b e1 e2 =
      ELet (b, self#visit_expr_w false e1, self#visit_expr_w terminal e2)

    method! visit_EWhile _ e1 e2 = EWhile (self#visit_expr_w false e1, self#visit_expr_w true e2)

    method! visit_EFor _ b e1 e2 e3 e4 =
      EFor
        ( b,
          self#visit_expr_w false e1,
          self#visit_expr_w false e2,
          self#visit_expr_w false e3,
          self#visit_expr_w true e4 )

    method! visit_EContinue (terminal, t) =
      if terminal then begin
        assert (t = TUnit);
        EUnit
      end
      else
        EContinue

    method! visit_ESequence _ _ = assert false

    method! visit_EIfThenElse (terminal, _) e1 e2 e3 =
      EIfThenElse
        (self#visit_expr_w false e1, self#visit_expr_w terminal e2, self#visit_expr_w terminal e3)

    method! visit_EMatch (terminal, _) f e branches =
      EMatch
        ( f,
          self#visit_expr_w false e,
          List.map (fun (b, p, e) -> b, p, self#visit_expr_w terminal e) branches )
  end

let unsigned_overflow_is_ok_in_c =
  object
    inherit [_] map as super

    method! visit_EApp env e es =
      let is_u = function
        | "u8" | "u16" | "u32" | "u64" | "usize" -> true
        | _ -> false
      in
      let as_w = function
        | "u8" -> K.UInt8
        | "u16" -> UInt16
        | "u32" -> UInt32
        | "u64" -> UInt64
        | "usize" -> SizeT
        | _ -> failwith "not an unsigned crate name"
      in
      match e.node with
      | EQualified ([ "core"; "num"; t; _ ], "wrapping_add") when is_u t ->
          EApp (Krml.Helpers.mk_op Add (as_w t), es)
      | _ -> super#visit_EApp env e es
  end

(* PPrint.(Krml.Print.(print (Krml.PrintAst.print_files files ^^ hardline))); *)

let remove_slice_eq = object
  inherit [_] map as super

  method! visit_expr _ e =
    match e with
    | [%cremepat {| core::cmp::impls::?impl::eq(#?eq..)<?t,?u>(?s1, ?s2) |}] ->
        begin match impl with
        | "{core::cmp::PartialEq<&0 mut (B)> for &1 mut (A)}"
        | "{core::cmp::PartialEq<&0 (B)> for &1 (A)}" ->
            assert (t = u);
            begin match flatten_tapp t with
            | lid, [ t ], [] when lid = Builtin.derefed_slice ->
                let rec is_flat = function
                  | TArray (t, _) -> is_flat t
                  | TInt _ | TBool | TUnit -> true
                  | _ -> false
                in
                if not (is_flat t) then
                  failwith "TODO: slice eq at non-flat types";
                with_type TBool (EApp (
                  Builtin.(expr_of_builtin_t slice_eq [ t ]),
                  [ s1; s2 ]))
            | _ ->
                let deref e = with_type (H.assert_tbuf e.typ) (EBufRead (e, H.zero_usize)) in
                with_type TBool (EApp (List.hd eq, [ deref s1; deref s2]))
            end
        | _ -> failwith "unknown eq impl in core::cmp::impls"
        end
    | _ -> super#visit_expr ((), e.typ) e
end

let cleanup files =
  let files = remove_units#visit_files () files in
  let files = remove_assignments#visit_files AtomMap.empty files in
  let files = unsigned_overflow_is_ok_in_c#visit_files () files in
  let files = Krml.Simplify.optimize_lets files in
  let files = remove_terminal_returns#visit_files true files in
  let files = remove_terminal_continues#visit_files false files in
  let files = Krml.Simplify.let_to_sequence#visit_files () files in
  let files = remove_slice_eq#visit_files () files in
  files
