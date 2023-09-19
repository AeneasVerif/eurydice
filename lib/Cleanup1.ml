open Krml.Ast
open Krml.DeBruijn

module AtomMap = Map.Make(Krml.Atom)
module AtomSet = Set.Make(Krml.Atom)

let set_of_map_keys m =
  AtomSet.of_list (List.map fst (AtomMap.bindings m))

let count_atoms = object
  inherit [_] reduce

  method private zero = AtomSet.empty
  method private plus = AtomSet.union

  method! visit_EOpen _ _ a =
    AtomSet.singleton a
end

let remove_assignments = object(self)
  inherit [_] map

  method private peel_lets to_close e =
    match e.node with
    | ELet (b, e1, e2) ->
        assert (e1.node = EAny || e1.node = EUnit);
        let b, e2 = open_binder b e2 in
        let to_close = AtomMap.add b.node.atom (b.node.name, b.typ) to_close in
        self#peel_lets to_close e2
    | _ ->
        let e = Krml.Simplify.sequence_to_let#visit_expr_w () e in
        self#visit_expr_w to_close e

  method! visit_DFunction to_close cc flags n t name bs e =
    assert (AtomMap.is_empty to_close);
    DFunction (cc, flags, n, t, name, bs, self#peel_lets to_close e)

  method! visit_DGlobal to_close flags n t name e =
    assert (AtomMap.is_empty to_close);
    DGlobal (flags, n, t, name, self#peel_lets to_close e)

  method! visit_ELet (not_yet_closed, t) b e1 e2 =
    let close_now_over not_yet_closed candidates mk_node =
      let to_close_now = AtomSet.inter candidates (set_of_map_keys not_yet_closed) in
      let bs = List.of_seq (AtomSet.to_seq to_close_now) in
      let bs = List.map (fun atom ->
        let name, typ = AtomMap.find atom not_yet_closed in
        { node = { atom; name; mut = true; mark = ref 0; meta = None }; typ },
        if typ = TUnit then Krml.Helpers.eunit else Krml.Helpers.any
      ) bs in
      (* For the subexpressions, we now need to insert declarations for those variables that we're
         not handling now. *)
      let not_yet_closed = AtomMap.filter (fun a _ -> not (AtomSet.mem a to_close_now)) not_yet_closed in
      let node = mk_node not_yet_closed in
      (Krml.Helpers.nest bs t (with_type t node)).node
    in

    let count e = count_atoms#visit_expr_w () e in

    let recurse_or_close not_yet_closed e =
      match e.node with
      | ELet _ ->
          self#visit_expr_w not_yet_closed e
      | _ ->
          with_type e.typ (close_now_over not_yet_closed (count e) (fun not_yet_closed ->
            (self#visit_expr_w not_yet_closed e).node))
    in

    let (++) = AtomSet.union in

    match e1.node with
    | EAssign ({ node = EOpen (_, atom); _ }, e1) when AtomMap.mem atom not_yet_closed ->
        close_now_over not_yet_closed (count e1) (fun not_yet_closed ->
          (* Combined "close now" (above) + let-binding insertion in lieu of the assignment *)
          assert (b.node.meta = Some MetaSequence);
          let e2 = snd (open_binder b e2) in
          let name, typ = AtomMap.find atom not_yet_closed in
          let b = { node = { atom; name; mut = true; mark = ref 0; meta = None }; typ } in
          let not_yet_closed = AtomMap.remove atom not_yet_closed in
          let e2 = self#visit_expr_w not_yet_closed (close_binder b e2) in
          ELet (b, e1, e2))
    | EIfThenElse (e, e', e'') ->
        assert (b.node.meta = Some MetaSequence);
        close_now_over not_yet_closed (
          (* We must now bind: *)
            (count e) ++ (* whichever variables were in the condition *)
            (AtomSet.inter (count e') (count e'')) ++ (* variables that appear in both branches *)
            (AtomSet.inter (count e' ++ count e'') (count e2))) (* variables in either branch *and* used later *)
          (fun not_yet_closed ->
            ELet (b, with_type e1.typ (EIfThenElse (self#visit_expr_w not_yet_closed e,
              recurse_or_close not_yet_closed e',
              recurse_or_close not_yet_closed e'')),
              recurse_or_close not_yet_closed e2))
    | EWhile (e, e') ->
        assert (b.node.meta = Some MetaSequence);
        close_now_over not_yet_closed (count e ++ AtomSet.inter (count e') (count e2)) (fun not_yet_closed ->
          ELet (b,
            with_type TUnit (EWhile (self#visit_expr_w not_yet_closed e, recurse_or_close not_yet_closed e')),
            recurse_or_close not_yet_closed e2))
    | _ ->
        (* The open variables in e1 for which we have not yet inserted a declaration need to be closed now *)
        close_now_over not_yet_closed (count e1) (fun not_yet_closed ->
          ELet (b, self#visit_expr_w not_yet_closed e1, recurse_or_close not_yet_closed e2))
end

let remove_units = object
  inherit [_] map as super

  method! visit_EAssign env e1 e2 =
    if e1.typ = TUnit && Krml.Helpers.is_readonly_c_expression e2 then
      EUnit
    else
      super#visit_EAssign env e1 e2
end

let remove_terminal_returns = object(self)
  inherit [_] map

  method! visit_ELet (terminal, _) b e1 e2 =
    ELet (b, self#visit_expr_w false e1, self#visit_expr_w terminal e2)

  method! visit_EWhile _ e1 e2 =
    EWhile (self#visit_expr_w false e1, self#visit_expr_w false e2)

  method! visit_EFor _ b e1 e2 e3 e4 =
    EFor (b, self#visit_expr_w false e1, self#visit_expr_w false e2, self#visit_expr_w false e3,
      self#visit_expr_w false e4)

  method! visit_EReturn (terminal, _) e =
    if terminal then
      (self#visit_expr_w terminal e).node
    else
      EReturn (self#visit_expr_w terminal e)

  method! visit_ESequence _ _ =
    assert false
end

(* PPrint.(Krml.Print.(print (Krml.PrintAst.print_files files ^^ hardline))); *)

let cleanup files =
  let files = remove_units#visit_files () files in
  let files = remove_assignments#visit_files AtomMap.empty files in
  let files = Krml.Simplify.count_and_remove_locals#visit_files [] files in
  let files = Krml.Simplify.remove_uu#visit_files () files in
  let files = remove_terminal_returns#visit_files true files in
  let files = Krml.Simplify.let_to_sequence#visit_files () files in
  files
