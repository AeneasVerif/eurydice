(* Compilation of const generics at runtime. *)


open Krml.Ast
open Krml.PrintAst.Ops

module H = Krml.Helpers

(* In some cases, we wish to disable const-generic monomorphization to e.g.
   avoid code-size bloat. For the moment, this is a global switch, but in the
   future, one could conceivably use an attribute, or a pass a list of functions
   that should opt-out of monomorphization via the command-line.

   The first step consists in turning monomorphizing calls (ETApps) into regular
   function calls, so as to avoid those functions getting cg-specialized. *)
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


(* But for types, we *do* need to monomorphize them still. Base types like
   uint8_t[$0] (using $ to signify "cg var") can be suitably handled all
   throughout and stack-allocated with VLAs. But for data type declarations
   (which, of course, abound), there is no such mechanism, so we need to do
   something.

   The strategy is to traverse, symbolically, the function-call graph, and
   for each cg-monomorphization found, record it. Concretely, if there is a call
   to f::<32>() and the body of f mentions `[u32; N] * [u32; N]`, we record
   `[u32; 32] * [u32; 32]` as an extra monomorphization that *will* be needed
   to support the runtime execution of that cg code. *)
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
    let builtin = match List.hd (fst t) with "Eurydice" | "core" -> true | _ -> false in
    if known then
      Krml.(KPrint.bprintf "KNOWN: %a: %a ++ %a\n" plid t ptyps ts pcgs cgs)
    else if not builtin then begin
      Krml.(KPrint.bprintf "RUNTIME_MONOMORPHIZATION: %a: %a ++ %a\n" plid t ptyps ts pcgs cgs);
      if Hashtbl.mem known_concrete_arguments t then begin
        let existing = Hashtbl.find known_concrete_arguments t in
        if not (List.mem (ts, cgs) existing) then
          Hashtbl.replace known_concrete_arguments t ((ts, cgs) :: existing)
      end else
        Hashtbl.add known_concrete_arguments t [ ts, cgs ]
    end
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
            let _ = Krml.(KPrint.bprintf "Visiting %a\n" plid (lid_of_decl decl)) in
            super#visit_decl env decl
      | DGlobal (_, _, n, _, _) ->
          if n = 0 then
            let _ = Krml.(KPrint.bprintf "Visiting %a\n" plid (lid_of_decl decl)) in
            super#visit_decl env decl
      | _ ->
          ()

    (* Completely substitute types and cgs in the callee's body. Not sure how
       much more expensive this is than monomorphization. *)
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
                    pexpr (with_type TUnit (ETApp (e, cgs, ts)));
                  assert (n_cgs = List.length cgs && n = List.length ts);
                  Hashtbl.add seen (lid, (cgs, ts)) ();
                  let body = Krml.DeBruijn.(subst_cen (List.length binders - n_cgs) cgs (subst_ten ts body)) in
                  (* Krml.KPrint.bprintf "After subst: %a\n" Krml.PrintAst.Ops.pexpr body; *)
                  self#visit_expr_w () body

                  (* These should be visited as part of the body... we are
                     talking about parameters and return expressions, unless
                     some parameters are unused...? *)
                  (* let ret = Krml.DeBruijn.subst_ctn 0 (* all closed terms *) cgs (Krml.DeBruijn.subst_tn ts ret) in
                  Krml.KPrint.bprintf "After subst: %a\n" Krml.PrintAst.Ops.ptyp ret;
                  self#visit_typ () ret *)
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

(* It helps to compare this with what's already caught by regular
   monomorphization. *)
let debug missing =
  Krml.KPrint.bprintf "DEBUG RUNTIME MONOMORPHIZATION:\n";
  Hashtbl.iter (fun lid args ->
    let open Krml in
    KPrint.bprintf "%a:\n" plid lid;
    List.iter (fun (ts, cgs) ->
      KPrint.bprintf "  %a ++ %a\n" ptyps ts pcgs cgs
    ) args
  ) missing

(* So once we've captured the missing instances of monomorphization that aren't
   otherwise reached with regular, non-cg functions, we gather them in a
   separate file to be fed separately to data type monomorphization. *)
let build_missing_decls missing =
  let i = ref 0 in
  "", Hashtbl.fold (fun lid args acc ->
    List.fold_left (fun acc (ts, cgs) ->
      let abbrev_lid = [], "special" ^ string_of_int !i in
      incr i;
      DType (abbrev_lid, [ Krml.Common.AutoGenerated ], 0, 0, Abbrev (fold_tapp (lid, ts, cgs))) :: acc
    ) acc args
  ) missing []

(* Now these things have been monomorphized, and have a corresponding lid
   allocated and recorded in MonomorphizationState. This is where it gets
   interesting... because I didn't carry explicit substitutions, I now have to
   do something equivalent where for each syntactic form of type that *still*
   has const generics in it, I try to figure out which monomorphized types could
   possibly, at run-time, be one of the corresponding candidates. *)
let replace =
  let rec matches_t t1 t2 =
    let r =
      match t1, t2 with
      | TTuple ts1, TTuple ts2 ->
          List.for_all2 matches_t ts1 ts2
      | TApp (t1, ts1), TApp (t2, ts2) ->
          t1 = t2 && List.for_all2 matches_t ts1 ts2
      | TCgArray (t1, cg1), TCgArray (t2, cg2) ->
          matches_t t1 t2 && cg1 = cg2
      | TArray (t1, _), TCgArray (t2, _) ->
          (* TODO: compute mgus and check they coincide *)
          matches_t t1 t2
      | TCgApp (t1, cg1), TCgApp (t2, cg2) ->
          matches_t t1 t2 && matches_cg cg1 cg2
      | TApp _, TCgApp _ ->
          let t1, ts1, cgs1 = flatten_tapp t1 in
          let t2, ts2, cgs2 = flatten_tapp t2 in
          t1 = t2 && List.for_all2 matches_t ts1 ts2 && List.for_all2 matches_cg cgs1 cgs2
      | t1, t2 ->
          t1 = t2
    in
    Krml.(KPrint.bprintf "  MATCH: %a | %a ? %b\n" ptyp t1 ptyp t2 r);
    r

  and matches_cg cg1 cg2 =
    match cg1, cg2 with
    | _, CgVar _ -> true
    | CgConst c1, CgConst c2 -> c1 = c2
    | _ -> false
  in

  let replace t ts cgs =
    let builtin = match List.hd (fst t) with "Eurydice" | "core" -> true | _ -> false in
    if not builtin then begin
      Krml.(KPrint.bprintf "REPLACE: %a: %a ++ %a\n" plid t ptyps ts pcgs cgs);
      (* let candidates = Hashtbl.find missing t in *)
      (* Pull ALL the candidates from monomorphization *)
      let candidates = Hashtbl.fold (fun (lid, ts, cgs) (_, mono_lid) acc ->
        if lid = t then (ts, cgs, mono_lid) :: acc else acc
      ) Krml.MonomorphizationState.state [] in
      (* Keep those that match *)
      let candidates = List.filter (fun (ts', cgs', _) ->
        List.for_all2 matches_t ts' ts && List.for_all2 matches_cg cgs' cgs
      ) candidates in
      match candidates with
      | [] -> fold_tapp (t, ts, cgs)
      | [ ts', cgs', lid ] ->
          Krml.(KPrint.bprintf "  FOUND: %a: %a ++ %a\n" plid t ptyps ts' pcgs cgs');
          Krml.(KPrint.bprintf "  AKA: %a\n" plid lid);
          TQualified lid
      | _ ->
          List.iter (fun (ts, cgs, _) ->
            Krml.(KPrint.bprintf "  CANDIDATE: %a: %a ++ %a\n" plid t ptyps ts pcgs cgs)
          ) candidates;
          failwith "TODO: multiple candidates, need to be subtler"
    end else
      fold_tapp (t, ts, cgs)
  in
  object(_self)
    inherit [_] map

    method! visit_TCgApp () t cg =
      let lid, ts, cgs = flatten_tapp (TCgApp (t, cg)) in
      replace lid ts cgs

    method! visit_TApp _ t ts =
      let lid, ts, cgs = flatten_tapp (TApp (t, ts)) in
      replace lid ts cgs

    method! visit_TTuple _ ts =
      replace tuple_lid ts []
  end

(* This happens much later. The previous phases left n_cgs in place over
   DFunctions so that remove_implicit_array_copies could convert length
   arguments of TCgArray's to runtime expressions, in order to implement blit
   operations to materialize array copies. We now remove those, and manually
   decay TCgArrays into TBufs, relying on Checker to validate this pass. *)
let decay_cgs = object(self)
  inherit Krml.DeBruijn.map_counting_cg as super

  method! visit_ELet env b e1 e2 =
    match b.typ, e1.node with
    | TCgArray _, EAny ->
        let rec convert t0 =
          match t0 with
          | TCgArray (t, _) ->
              (* Krml.KPrint.bprintf "n_cgs=%d, n=%d, i_cg=%d\n" (fst (fst env)) (snd (fst env)) i_cg; *)
              let n = Cleanup2.expr_of_array_length (fst env) t0 in
              let t, init = convert t in
              let typ = TBuf (t, false) in
              typ, with_type typ (EBufCreate (Stack, init, n))
          | TArray (_, _) ->
              let n = Cleanup2.expr_of_array_length (fst env) t0 in
              t0, with_type t0 (EBufCreate (Stack, H.any, n))
          | _ ->
              t0, H.any
        in
        let typ, init = convert b.typ in
        ELet ({ b with typ }, init,
          self#visit_expr_w (self#extend (fst env) { b with typ }) e2)
    | _ ->
        super#visit_ELet env b e1 e2

  method! visit_TCgArray env t _ =
    super#visit_TBuf env t false

  method! visit_DFunction _ cc flags n_cgs n t name bs e =
    (* Setting up environment properly *)
    super#visit_DFunction (n_cgs, 0) cc flags n_cgs n t name bs e
end


(* This happens much later. The previous phases left n_cgs in place over
   DFunctions so that remove_implicit_array_copies could convert length
   arguments of TCgArray's to runtime expressions, in order to implement blit
   operations to materialize array copies. We now remove those, and manually
   decay TCgArrays into TBufs, relying on Checker to validate this pass. *)
let erase_cgs = object(_self)
  inherit Krml.DeBruijn.map_counting_cg as super

  method! visit_DFunction _ cc flags n_cgs n t name bs e =
    (* Discarding n_cgs *)
    super#visit_DFunction (n_cgs, 0) cc flags 0 n t name bs e

end

