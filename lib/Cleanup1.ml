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

  method! visit_DFunction to_close cc flags n t name bs e =
    let rec peel_lets to_close e =
      match e.node with
      | ELet (b, e1, e2) ->
          assert (e1.node = EAny);
          let b, e2 = open_binder b e2 in
          let to_close = AtomMap.add b.node.atom (b.node.name, b.typ) to_close in
          peel_lets to_close e2
      | _ ->
          let e = Krml.Simplify.sequence_to_let#visit_expr_w () e in
          self#visit_expr_w to_close e
    in
    DFunction (cc, flags, n, t, name, bs, peel_lets to_close e)

  method! visit_ELet (to_close, t) b e1 e2 =
    let close_now open_in_e1 =
      let to_close_now = AtomSet.inter open_in_e1 (set_of_map_keys to_close) in
      let bs = List.of_seq (AtomSet.to_seq to_close_now) in
      let bs = List.map (fun atom ->
        let name, typ = AtomMap.find atom to_close in
        { node = { atom; name; mut = true; mark = ref 0; meta = None }; typ }, Krml.Helpers.any
      ) bs in
      (* For the subexpressions, we now need to insert declarations for those variables that we're
         not handling now. *)
      let to_close = AtomMap.filter (fun a _ -> not (AtomSet.mem a to_close_now)) to_close in
      let node = ELet (b, self#visit_expr_w to_close e1, self#visit_expr_w to_close e2) in
      (Krml.Helpers.nest bs t (with_type t node)).node
    in

    let count e = count_atoms#visit_expr_w () e in

    match e1.node with
    | EAssign ({ node = EOpen (_, atom); _ }, e1) when AtomMap.mem atom to_close ->
        (* Combined "close now" (above) + let-binding insertion *)
        assert (b.node.meta = Some MetaSequence);
        let e2 = snd (open_binder b e2) in
        let name, typ = AtomMap.find atom to_close in
        let b = { node = { atom; name; mut = true; mark = ref 0; meta = None }; typ } in
        let to_close = AtomMap.remove atom to_close in
        let e2 = self#visit_expr_w to_close (close_binder b e2) in
        ELet (b, e1, e2)
    | EIfThenElse (e1, e2, e3) ->
        assert (b.node.meta = Some MetaSequence);
        close_now (AtomSet.union (count e1) (AtomSet.inter (count e2) (count e3)))
    | EWhile (e1, _) ->
        assert (b.node.meta = Some MetaSequence);
        close_now (count e1)
    | _ ->
        (* The open variables in e1 for which we have not yet inserted a declaration need to be closed now *)
        close_now (count e1)
end

(* PPrint.(Krml.Print.(print (Krml.PrintAst.print_files files ^^ hardline))); *)

let cleanup files =
  let files = remove_assignments#visit_files AtomMap.empty files in
  let files = Krml.Simplify.count_and_remove_locals#visit_files [] files in
  let files = Krml.Simplify.remove_uu#visit_files () files in
  let files = Krml.Simplify.let_to_sequence#visit_files () files in
  files
