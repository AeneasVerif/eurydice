(* Compilation of const generics at runtime. *)


open Krml.Ast

module H = Krml.Helpers

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


(* But for types, we *do* need to monomorphize them still. For that, we
   symbolically traverse the function-call and type declaration graph and, for
   each cg-monomorphization found, record it and insert it into the files. *)
let enumerate_cg_monomorphizations files =
  let map = H.build_map files (fun map -> function
    | DFunction (cc, flags, n_cgs, n, t, name, b, body) ->
        if n > 0 || n_cgs > 0 then
          Hashtbl.add map name (DFunction (cc, flags, n_cgs, n, t, name, b, body))
    | _ ->
        ()
  ) in

  (* We are concerned with applications of data types (so, TCgApps) to
     arguments that contain const generic arguments. We want to enumerate, based
     on a whole-program traversal, all possible values of const generics for any
     given concrete type application whose arguments contains const generic
     variables. Examples of such forms include `uint8_t[$3] * uint8_t[$4]` (the
     application of the tuple constructor to two TCgArrays), or
     `MlKemPrivateKey<$0>` (the application of a hand-written data type
     constructor to the first const generic argument). *)
  let known_concrete_arguments = Hashtbl.create 41 in
  let record t ts cgs =
    assert (List.for_all (function CgConst _ -> true | CgVar _ -> false) cgs);
    let known = Hashtbl.mem Krml.MonomorphizationState.state (t, ts, cgs) in
    if known then
      let open Krml in
      let open PrintAst.Ops in
      KPrint.bprintf "KNOWN: %a: %a ++ %a\n" plid t ptyps ts pcgs cgs;
    if not (Hashtbl.mem Krml.MonomorphizationState.state (t, ts, cgs)) &&
      match List.hd (fst t) with "Eurydice" | "core" -> false | _ -> true
    then
      let _ = Krml.(
        let open PrintAst.Ops in
        KPrint.bprintf "RUNTIME_MONOMORPHIZATION: %a: %a ++ %a\n" plid t ptyps ts pcgs cgs
      ) in
      if Hashtbl.mem known_concrete_arguments t then
        let existing = Hashtbl.find known_concrete_arguments t in
        if not (List.mem (ts, cgs) existing) then
          Hashtbl.replace known_concrete_arguments t ((ts, cgs) :: existing)
      else
        Hashtbl.add known_concrete_arguments t [ ts, cgs ]
  in

  let seen = Hashtbl.create 41 in

  (* To enumerate those, we visit each function and global recursively, keeping
     a mapping from cg db index to closed term. *)
  object(self)
    inherit [_] iter as super

    (* Main traversal logic. We only descend into functions that are type- and
       cg-monomorphic, because ultimately, what makes it into C code-generation
       is whatever is reachable from those functions. *)
    method! visit_decl env decl =
      match decl with
      | DFunction (_, _, n_cgs, n, _, _, _, _) ->
          if n_cgs = 0 && n = 0 then
            let _ = Krml.(KPrint.bprintf "Visiting %a\n" PrintAst.Ops.plid (lid_of_decl decl)) in
            super#visit_decl env decl
      | DGlobal (_, _, n, _, _) ->
          if n = 0 then
            let _ = Krml.(KPrint.bprintf "Visiting %a\n" PrintAst.Ops.plid (lid_of_decl decl)) in
            super#visit_decl env decl
      | _ ->
          ()

    (* Completely substitute types and cgs in the callee's body. About just as
       expensive as monomorphization. *)
    method! visit_EApp _ e es =
      (* This is an EApp not an ETApp because the earlier phase just removed
         those! *)
      match match e.node with
        | EQualified lid -> Some (lid, es, [])
        | ETApp ({ node = EQualified lid; _ }, cgs, ts) -> Some (lid, cgs @ es, ts)
        | _ -> None
      with
      | Some (lid, es, ts) ->
          if Hashtbl.mem map lid then
            begin match Hashtbl.find map lid with
            | DFunction (_, _, n_cgs, n, _ret, _, binders, body) ->
                let cgs, _ = Krml.KList.split n_cgs es in
                if not (Hashtbl.mem seen (lid, (cgs, ts))) then begin
                  Krml.KPrint.bprintf "Visiting (recursive): %a\n"
                    Krml.PrintAst.Ops.pexpr (with_type TUnit (ETApp (e, cgs, ts)));
                  assert (n_cgs = List.length cgs && n = List.length ts);
                  Hashtbl.add seen (lid, (cgs, ts)) ();
                  let body = Krml.DeBruijn.(subst_cen (List.length binders - n_cgs) cgs (subst_ten ts body)) in
                  (* Krml.KPrint.bprintf "After subst: %a\n" Krml.PrintAst.Ops.pexpr body; *)
                  self#visit_expr_w () body

(*                   let ret = Krml.DeBruijn.subst_ctn 0 (1* all closed terms *1) cgs (Krml.DeBruijn.subst_tn ts ret) in *)
(*                   Krml.KPrint.bprintf "After subst: %a\n" Krml.PrintAst.Ops.ptyp ret; *)
(*                   self#visit_typ () ret *)
                end
            | _ ->
                ()
            end
      | None ->
          ()

    (* This is where the action happens. Note that I chose NOT to carry explicit
       substitutions, so we don't know what the thing originally looked like --
       we can't tell here the difference between, e.g.,
       `uint8_t[$0] * uint8_t[$0]` applied to `32`, and
       `('a * 'b)` applied to `uint8_t[32]` and `uint8_t[32]`. *)
    method! visit_TCgApp _ t cg =
      let lid, ts, cgs = flatten_tapp (TCgApp (t, cg)) in
      record lid ts cgs

    method! visit_TApp _ t ts =
      let lid, ts, cgs = flatten_tapp (TApp (t, ts)) in
      record lid ts cgs

    method! visit_TTuple _ ts =
      record tuple_lid ts []

  end#visit_files () files;

  (* We run after the first data type monomorphization -- all of the type
     applications we found here will be missing once we execute our cg-generic
     functions at runtime, and we need to request monomorphizations of those
     too. *)
  known_concrete_arguments

let debug missing =
  Krml.KPrint.bprintf "DEBUG RUNTIME MONOMORPHIZATION:\n";
  Hashtbl.iter (fun lid args ->
    let open Krml in
    let open PrintAst.Ops in
    KPrint.bprintf "%a:\n" plid lid;
    List.iter (fun (ts, cgs) ->
      KPrint.bprintf "  %a ++ %a\n" ptyps ts pcgs cgs
    ) args
  ) missing


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
        let n = Cleanup2.expr_of_array_length (fst env) b.typ in
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

