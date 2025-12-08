open Krml.Ast
module H = Krml.Helpers

(* All the transformations that need to happen in order for the program to type-check as valid Low*
   *)

let expr_of_constant (w, n) = with_type (TInt w) (EConstant (w, n))

let remove_array_eq =
  object
    inherit Krml.DeBruijn.map_counting_cg as super

    method! visit_expr (((n_cgs, n_binders) as env), _) e =
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
                with_type TBool
                  (EApp (Builtin.(expr_of_builtin_t ~cgs:(diff, [ n ]) array_eq [ t ]), [ a1; a2 ]))
            | "{core::cmp::PartialEq<&0 (@Slice<U>)> for @Array<T, N>}" ->
                let hd =
                  if !Options.no_const then
                    Builtin.array_eq_slice_mut
                  else
                    Builtin.array_eq_slice_shared
                in
                with_type TBool
                  (EApp (Builtin.(expr_of_builtin_t ~cgs:(diff, [ n ]) hd [ t ]), [ a1; a2 ]))
            | _ -> failwith ("unknown array eq impl: " ^ impl)
          else
            failwith "TODO: non-byteeq array comparison"
      | _ -> super#visit_expr (env, e.typ) e

    method! visit_DFunction _ cc flags n_cgs n t lid bs e =
      super#visit_DFunction (n_cgs, 0) cc flags n_cgs n t lid bs e
  end

let expression_of_cg (n_cgs, n_binders) (cg : cg) =
  match cg with
  | CgConst n -> H.mk_sizet (int_of_string (snd n))
  | CgVar var ->
      let diff = n_binders - n_cgs in
      with_type (TInt SizeT) (EBound (var + diff))

(* slice_to_array<&[T], [T; N], Error>(src) -> 
   let arr: Arr<T,N>; memcpy(arr.data, src.ptr, N); Ok arr 
   slice_to_ref_array<&[T], &[T;N], Error, len>(src) ->
   let arr: Arr<T,N>; memcpy(arr.data, src.ptr); slice_to_ref_array2<&[T], &[T;N], Error, len>(src, &arr)
   *)
let constness_of_slice_type t =
  match t with
  | TApp (lid, _) when lid = Builtin.dst_ref_shared -> true
  | TApp (lid, _) when lid = Builtin.dst_ref_mut -> false
  | _ -> assert false

let expand_slice_to_array =
  object (_self)
    inherit Krml.DeBruijn.map_counting_cg as super

    method! visit_expr ((count, _) as env) e =
      match e.node with
      | EApp
          ( {
              node =
                ETApp
                  ( { node = EQualified lid; _ },
                    _,
                    _,
                    [ _; (TCgApp (TApp (_, [ t ]), cg) as arr_t); _ ] );
              _;
            },
            es )
        when lid = Builtin.slice_to_array.name ->
          let src = Krml.KList.one es in
          let result_t = e.typ in
          (*let arr = .. *)
          let arr = with_type arr_t (EBound 0) in
          let src = Krml.DeBruijn.lift 1 src in
          let dst = with_type (TBuf (t, false)) (EField (arr, "data")) in
          let src = with_type (TBuf (t, constness_of_slice_type src.typ)) (EField (src, "ptr")) in
          let n = Krml.DeBruijn.lift 1 (expression_of_cg count cg) in
          let zero = H.zero SizeT in
          let memcpy = H.with_unit (EBufBlit (src, zero, dst, zero, n)) in
          (* let arr = any in {memcpy (src, slice); OK (arr);} *)
          with_type result_t
            (ELet
               ( H.fresh_binder "arr" arr_t,
                 H.any,
                 with_type result_t
                   (ESequence [ memcpy; with_type result_t (ECons ("Ok", [ arr ])) ]) ))
      | EApp
          ( {
              node =
                ETApp
                  ( { node = EQualified lid; _ },
                    cgs,
                    _,
                    [
                      slice_t;
                      (TBuf ((TCgApp (TApp (_, [ t ]), cg) as arr_t), _) as arr_ref_t);
                      err_t;
                    ] );
              _;
            },
            [ slice ] )
        when lid = Builtin.slice_to_ref_array.name ->
          assert (cgs = []);
          (* allocate a Arr<T,C>, do memcpy and let the C macro do the choose and define the return
             or error value *)
          let const = constness_of_slice_type slice_t in
          let slice_to_ref_array2 = Builtin.(expr_of_builtin slice_to_ref_array2) in
          let ts = [ slice_t; arr_ref_t; err_t ] in
          let slice_to_ref_array2 =
            with_type
              (Krml.DeBruijn.subst_tn ts Builtin.slice_to_ref_array2.typ)
              (ETApp (slice_to_ref_array2, [], [], ts))
          in
          let arr = with_type arr_t (EBound 0) in
          let arr_ref = with_type arr_ref_t (EAddrOf arr) in
          let slice = Krml.DeBruijn.lift 1 slice in
          let dst = with_type (TBuf (t, true)) (EField (arr, "data")) in
          let src = with_type (TBuf (t, const)) (EField (slice, "ptr")) in
          let n = Krml.DeBruijn.lift 1 (expression_of_cg count cg) in
          (* let n = with_type (TInt SizeT) (EField (slice, "meta")) in *)
          let zero = H.zero SizeT in
          let memcpy = H.with_unit (EBufBlit (src, zero, dst, zero, n)) in
          with_type e.typ
            (ELet
               ( H.fresh_binder "arr" arr_t,
                 H.any,
                 with_type e.typ
                   (ESequence
                      [ memcpy; with_type e.typ (EApp (slice_to_ref_array2, [ slice; arr_ref ])) ])
               ))
      | _ -> super#visit_expr env e

    method! visit_DFunction _ cc flags n_cgs n t name bs e =
      super#visit_DFunction (n_cgs, 0) cc flags n_cgs n t name bs e
  end

(** Comes from [drop_unused] in Inlining.ml, we use it to remove the builtin function defined using
    abstract syntax when they are not used. Otherwise they will refer to the undefined Range type
    and fail the check *)

let builtin_func_lids = List.map lid_of_decl Builtin.builtin_defined_funcs

let drop_unused_builtin files =
  let open Krml in
  let open Krml.Common in
  let seen = Hashtbl.create 41 in

  let body_of_lid = Helpers.build_map files (fun map d -> Hashtbl.add map (lid_of_decl d) d) in

  let visitor =
    object (self)
      inherit [_] iter as super
      method! visit_EQualified (before, _) lid = self#discover before lid
      method! visit_TQualified before lid = self#discover before lid

      method! visit_TApp before lid ts =
        self#discover before lid;
        List.iter (self#visit_typ before) ts

      method private discover before lid =
        if not (Hashtbl.mem seen lid) then begin
          Hashtbl.add seen lid ();
          if Hashtbl.mem body_of_lid lid then
            ignore (super#visit_decl (lid :: before) (Hashtbl.find body_of_lid lid))
        end

      method! visit_decl _ d =
        let flags = flags_of_decl d in
        let lid = lid_of_decl d in
        if (not (List.exists (( = ) Private) flags)) && not (Drop.lid lid) then begin
          Hashtbl.add seen lid ();
          super#visit_decl [ lid ] d
        end
    end
  in
  visitor#visit_files [] files;
  Hashtbl.add seen ([ "LowStar"; "Ignore" ], "ignore") ();
  filter_decls
    (fun d ->
      let flags = flags_of_decl d in
      let lid = lid_of_decl d in
      if
        (List.exists (( = ) Private) flags || Drop.lid lid)
        && (not (Hashtbl.mem seen lid))
        && List.mem lid builtin_func_lids
      then
        None
      else
        Some d)
    files

let precleanup files =
  let files = remove_array_eq#visit_files (0, 0) files in
  let files = drop_unused_builtin files in
  let files = expand_slice_to_array#visit_files (0, 0) files in
  files

let merge files =
  let open Krml.Idents in
  let open Krml.PrintAst.Ops in
  let merge_decl lid d1 d2 =
    match d1, d2 with
    | _ when Krml.Idents.LidSet.mem lid Builtin.skip -> None
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
