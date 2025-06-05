open Krml.Ast

(* All the transformations that need to happen in order for the program to type-check as valid Low*
   *)

let expr_of_constant (w, n) = with_type (TInt w) (EConstant (w, n))

let flatten_sequences files =
  begin
    object
      inherit [_] map as super

      method! visit_ESequence env es =
        let rec flatten acc e =
          match e.node with
          | ESequence [ { node = EUnit; _ }; e2 ] -> flatten acc e2
          | ESequence [ e1; e2 ] -> flatten (e1 :: acc) e2
          | ESequence _ ->
              failwith "impossible: charon generates right-nested two-element sequences"
          | _ -> ESequence (List.rev_append acc [ super#visit_expr env e ])
        in
        flatten [] (with_type (snd env) (ESequence es))
    end
  end
    #visit_files
    () files

let expand_array_copies files =
  begin
    object
      inherit [_] map as super

      method! visit_EAssign env lhs rhs =
        match rhs.node with
        | EApp ({ node = EQualified lid; _ }, [ src; len ]) when lid = Builtin.array_copy ->
            (* Krml.Helpers.mk_copy_assignment (t, n) lhs.node rhs *)
            let zero = Krml.(Helpers.zero Constant.SizeT) in
            EBufBlit (src, zero, lhs, zero, len)
        | _ -> super#visit_EAssign env lhs rhs

      method! visit_EApp env hd args =
        if hd.node = EQualified Builtin.array_copy then
          Krml.Warn.fatal_error "Uncaught array copy"
        else
          super#visit_EApp env hd args
    end
  end
    #visit_files
    () files

let precleanup files =
  let files = expand_array_copies files in
  let files = flatten_sequences files in
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
