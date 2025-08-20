open Krml.Ast

module H = Krml.Helpers

(* All the transformations that need to happen in order for the program to type-check as valid Low*
   *)

let expr_of_constant (w, n) = with_type (TInt w) (EConstant (w, n))

let expand_array_copies files =
  begin
    object
      inherit [_] map as super

      method! visit_EAssign env lhs rhs =
        match rhs.node with
        | EApp ({ node = EQualified lid; _ }, [ src; len ]) when lid = Builtin.array_copy ->
            (* Krml.Helpers.mk_copy_assignment (t, n) lhs.node rhs *)
            let zero = H.zero_usize in
            EBufBlit (src, zero, lhs, zero, len)
        | _ -> super#visit_EAssign env lhs rhs

      method! visit_EApp env hd args =
        match hd, args with
        | { node = EQualified lid; _ }, [ src; len ] when lid = Builtin.array_copy ->
            ELet (H.fresh_binder "array_copy" src.typ,
              H.any,
              with_type src.typ (ESequence [
                with_type TUnit (
                  EBufBlit (Krml.DeBruijn.lift 1 src, H.zero_usize, with_type src.typ (EBound 0), H.zero_usize, Krml.DeBruijn.lift 1 len));
                with_type src.typ (EBound 0)]))
        | _ ->
            super#visit_EApp env hd args
    end
  end
    #visit_files
    () files

let remove_array_eq = object
  inherit Krml.DeBruijn.map_counting_cg as super

  method! visit_expr ((n_cgs, n_binders) as env, _) e =
    match e with
    | [%cremepat {| core::array::equality::?impl::eq[#?n](#?..)<?t,?u>(?a1, ?a2) |}] ->
        let rec is_flat = function
          | TCgApp (TApp (lid, [ t ]), _) -> lid = Builtin.arr && is_flat t
          | TInt _ | TBool | TUnit -> true
          | _ -> false
        in
        assert (t = u);
        if is_flat t then
          let diff = n_binders - n_cgs in
          match impl with
          | "{core::cmp::PartialEq<@Array<U, N>> for @Array<T, N>}" ->
              with_type TBool (EApp (
                Builtin.(expr_of_builtin_t ~cgs:(diff, [n]) array_eq [ t ]),
                [ a1; a2 ]))
          | "{core::cmp::PartialEq<&0 (@Slice<U>)> for @Array<T, N>}" ->
              with_type TBool (EApp (
                Builtin.(expr_of_builtin_t ~cgs:(diff, [n]) array_eq_slice [ t ]),
                [ a1; a2 ]))
          | _ ->
              failwith ("unknown array eq impl: " ^ impl)
        else
          failwith "TODO: non-byteeq array comparison"
    | _ -> super#visit_expr (env, e.typ) e

   method! visit_DFunction _ cc flags n_cgs n t lid bs e =
     super#visit_DFunction (n_cgs, 0) cc flags n_cgs n t lid bs e
end


(** Comes from [drop_unused] in Inlining.ml, we use it to remove the builtin function defined
    using abstract syntax when they are not used. Otherwise they may use some undefined types and
    fail the check *)

let builtin_func_lids =
  [
    [ "Eurydice" ], "array_to_subslice_to";
    [ "Eurydice" ], "array_to_subslice_from";
    [ "Eurydice" ], "array_to_slice";
  ]

let drop_unused_builtin files =
  let open Krml in
  let open Krml.Common in
  let seen = Hashtbl.create 41 in

  let body_of_lid = Helpers.build_map files (fun map d -> Hashtbl.add map (lid_of_decl d) d) in

  let visitor = object (self)
    inherit [_] iter as super
    method! visit_EQualified (before, _) lid =
      self#discover before lid
    method! visit_TQualified before lid =
      self#discover before lid
    method! visit_TApp before lid ts =
      self#discover before lid;
      List.iter (self#visit_typ before) ts
    method private discover before lid =
      if not (Hashtbl.mem seen lid) then begin
        Hashtbl.add seen lid ();
        if Hashtbl.mem body_of_lid lid then
          ignore (super#visit_decl (lid :: before) (Hashtbl.find body_of_lid lid));
      end
    method! visit_decl _ d =
      let flags = flags_of_decl d in
      let lid = lid_of_decl d in
      if not (List.exists ((=) Private) flags) && not (Drop.lid lid) then begin
        Hashtbl.add seen lid ();
        super#visit_decl [lid] d
      end
   end in
  visitor#visit_files [] files;
  Hashtbl.add seen (["LowStar"; "Ignore"], "ignore") ();
  filter_decls (fun d ->
    let flags = flags_of_decl d in
    let lid = lid_of_decl d in
    if (List.exists ((=) Private) flags || Drop.lid lid) && not (Hashtbl.mem seen lid)
       && List.mem lid builtin_func_lids then
      None
    else
      Some d
  ) files



let precleanup files =
  let files = expand_array_copies files in
  let files = remove_array_eq#visit_files (0, 0) files in
  let files = drop_unused_builtin files in
  files

let merge files =
  let open Krml.Idents in
  let open Krml.PrintAst.Ops in
  let merge_decl lid d1 d2 =
    match d1, d2 with
    | Some d1, None | None, Some d1 -> Some d1
    | None, None -> assert false
    | Some d1, Some d2 -> (
        let is_external = function
          | DExternal _ -> true
          | _ -> false
        in
        let check_equal () =
          if d1 <> d2 then begin
            Krml.KPrint.bprintf "%a is:\n%a\n\nVS\n\n%a\n" plid lid pdecl d1 pdecl d2;
            failwith "can't reconcile these two definitions"
          end
        in
        match d1, d2 with
        | DExternal _, d2 | d2, DExternal _ ->
            if is_external d2 then
              check_equal ();
            Some d2
        | _ ->
            check_equal ();
            Some d1)
  in
  let decl_map decls = LidMap.of_seq (List.to_seq (List.map (fun d -> lid_of_decl d, d) decls)) in
  let merge_decls decls1 decls2 = LidMap.merge merge_decl decls1 decls2 in
  let concat_filenames f1 f2 =
    if f1 = "" then
      f2
    else
      f1 ^ "_" ^ f2
  in
  let merge_files (f1, decls1) (f2, decls2) =
    concat_filenames f1 f2, merge_decls decls1 (decl_map decls2)
  in
  let f, decls = List.fold_left merge_files ("", LidMap.empty) files in
  let decls = List.map snd (List.of_seq (LidMap.to_seq decls)) in
  let decls = Bundles.topological_sort decls in
  f, decls
