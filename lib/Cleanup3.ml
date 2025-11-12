(* Administrative cleanups that do not get checked. *)

(* CG-polymorphic external signatures generally cannot be implemented with C functions, and Eurydice
   expects those to be hand-written using macros. There is one exception, though:
   - all of the const generics appear in positions that would anyhow decay to pointers (e.g.,
     void f<N>(int x[N]) can be replaced by void f(int *x) -- it's the same in C)
   - the return type is unit -- the implementation doesn't need to receive the return type as an
     argument
*)

module B = Builtin
open Krml
open Ast

let decay_cg_externals =
  object (self)
    inherit [_] Krml.Ast.map as super

    (* Since we allocate new names, we reuse the C name allocation facility *)
    inherit Simplify.scope_helpers

    method! visit_file env f =
      current_file <- fst f;
      super#visit_file env f

    method! visit_TCgArray (env, under_external) t n =
      if under_external then
        raise Exit
      else
        super#visit_TCgArray (env, under_external) t n

    method! visit_TCgApp (env, under_external) t n =
      if under_external then
        raise Exit
      else
        super#visit_TCgApp (env, under_external) t n

    method! visit_DExternal (scope_env, _) cc flags n_cgs n name t hints =
      let t_ret, t_args = Helpers.flatten_arrow t in
      (* MSVC throws a tantrum if it receives a zero-sized array parameter,
         interpreting this as a stack allocation instead of an array type that
         ought to decay to pointer. *)
      let t_args =
        List.map
          (function
            | TArray (t, (_, "0")) -> TBuf (t, false)
            | t -> t)
          t_args
      in
      if t_ret = TUnit && n_cgs > 0 then
        let t_args =
          List.map
            (function
              | TCgArray (t, _) -> TBuf (t, false)
              | t -> t)
            t_args
        in
        try
          (* This throws and aborts if there are some const generics left. *)
          let t_args = List.map (self#visit_typ (scope_env, true)) t_args in

          (* We're good. Find the allocated C name for our declaration, and allocate a new C name for
             the extra declaration *)
          let c_name = Option.get (GlobalNames.lookup (fst scope_env) (name, Other)) in
          let new_name = fst name, snd name ^ "_" in
          self#record scope_env ~is_type:false ~is_external:true flags new_name;
          let new_c_name = Option.get (GlobalNames.lookup (fst scope_env) (new_name, Other)) in

          (* We build: #define <<c_name>>(x0, ..., xn, _ret_t) \
             <<new_c_name>>(x0, ..., xn) *)
          let prelude =
            (* Names of the arguments *)
            let names =
              if List.length hints = List.length t_args then
                hints
              else
                List.init (List.length t_args) (fun i -> KPrint.bsprintf "x_%d" i)
            in
            KPrint.bsprintf "#define %s(%s) %s(%s)" c_name
              (String.concat ", " (names @ [ "_ret_t" ]))
              new_c_name (String.concat ", " names)
          in
          DExternal
            ( cc,
              [ Common.Prologue prelude ] @ flags,
              0,
              n,
              new_name,
              Helpers.fold_arrow t_args t_ret,
              hints )
        with Exit -> DExternal (cc, flags, n_cgs, n, name, Helpers.fold_arrow t_args t_ret, hints)
      else
        DExternal (cc, flags, n_cgs, n, name, Helpers.fold_arrow t_args t_ret, hints)
  end

let build_cg_macros =
  object (self)
    inherit [_] Krml.Ast.reduce
    method private zero = Krml.Idents.LidSet.empty
    method private plus = Krml.Idents.LidSet.union

    method! visit_DExternal () _ _ n_cgs n name _ _ =
      if n > 0 || n_cgs > 0 then
        Krml.Idents.LidSet.singleton name
      else
        self#zero
  end

let distinguished_names =
  [
    (B.arr, [ TInt UInt8 ], [ CgConst (SizeT, "2") ]), ([ "Eurydice" ], "array_u8x2");
    (B.arr, [ TInt UInt8 ], [ CgConst (SizeT, "4") ]), ([ "Eurydice" ], "array_u8x4");
    (B.arr, [ TInt UInt8 ], [ CgConst (SizeT, "8") ]), ([ "Eurydice" ], "array_u8x8");
    (B.dst_ref_shared, [ TInt UInt8; TInt SizeT ], []), ([ "Eurydice" ], "borrow_slice_u8");
    (B.dst_ref_shared, [ TInt Int16; TInt SizeT ], []), ([ "Eurydice" ], "borrow_slice_i16");
    (B.dst_ref_mut, [ TInt UInt8; TInt SizeT ], []), ([ "Eurydice" ], "mut_borrow_slice_u8");
    (B.dst_ref_mut, [ TInt Int16; TInt SizeT ], []), ([ "Eurydice" ], "mut_borrow_slice_i16");
  ]

(*This identifies the decls which should be generated after monomorphism, but is already defined
 in eurydice_glue.h for implementing the builtin functions. The slices are for
 libcrux, specifically to be able to define the intrinsic function signatures *)
let is_builtin_lid lid =
  match lid with
  | [ "Prims" ], "string" (* used to pass the checker, defined in glue.h *) -> true
  | _ -> List.exists (fun (_, lid') -> lid' = lid) distinguished_names

let remove_builtin_decls files =
  let checker = function
    | DType (lid, _, _, _, _) when is_builtin_lid lid -> None
    | decl -> Some decl
  in
  List.map (fun (name, decls) -> name, List.filter_map checker decls) files

let also_skip_prefix_for_external_types (scope_env, _) =
  let open Krml in
  object (_self)
    inherit [_] iter as _super

    method! visit_TQualified () lid =
      if GlobalNames.lookup scope_env (lid, Type) = None && GlobalNames.skip_prefix lid then
        let target = GlobalNames.target_c_name ~attempt_shortening:true ~kind:Type lid in
        let actual = GlobalNames.extend scope_env scope_env false (lid, Type) target in
        if actual <> fst target then
          KPrint.bprintf "Warning! The skip_prefix options generate name conflicts\n"
  end
