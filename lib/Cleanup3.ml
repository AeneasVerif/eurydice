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

(** The function to reorder the type declarations & definitions based on their dependencies To
    resolve potential cyclic dependencies, the following strategies are used:
    - All types are forward-declared first, to resolve shallow dependencies, e.g., the pointer types
    - For complete dependencies, i.e., a type definition requires the complete (sized) definition of
      another type, we make a topological sort of the type definitions. As Rust will not have cyclic
      complete dependencies, this should always succeed. This function assumes the uniqueness of
      each type definition (non-forward). *)
let resolve_typ_dependencies files =
  (* Classify each definition into three categories: 
     type definitions (non-forward / abbrev), abbreviations, and others *)
  let filter_partition_decls decls =
    let folder (type_defs, abbrev, forward, other_decls) = function
      | DType (_, _, _, _, Abbrev _) as d -> type_defs, d :: abbrev, forward, other_decls
      | DType (_, _, _, _, Forward _) as d -> type_defs, abbrev, d :: forward, other_decls
      | DType (lid, _, _, _, _) as d -> (lid, d) :: type_defs, abbrev, forward, other_decls
      | d -> type_defs, abbrev, forward, d :: other_decls
    in
    let defs, a, b, c = List.fold_left folder ([], [], [], []) decls in
    (* Assumes the uniqueness of type definitions, should be guaranteed by the previous procedures *)
    let defs = Hashtbl.of_seq (List.to_seq defs) in
    (* Do not forget to maintain the original order of the definitions *)
    defs, List.rev a, List.rev b, List.rev c
  in
  let remove_defined_forwards type_defs forward_decls =
    List.filter
      (function
        | DType (lid, _, _, _, Forward _) -> not (Hashtbl.mem type_defs lid)
        | _ -> assert false)
      forward_decls
  in
  let gen_forward_decls type_defs =
    Hashtbl.to_seq type_defs
    |> Seq.filter_map (fun (_, decl) ->
           match decl with
           (* Do not generate forward declarations for enums, as they will be turned to `uint8_t`
              This conflicts with the `struct` declaration *)
           | DType (_, _, _, _, Enum _) -> None
           | DType (lid, params, size, align, kind) ->
               let forward =
                 match kind with
                 | Union _ -> Common.FUnion
                 | Flat _ | Variant _ -> Common.FStruct
                 | _ -> assert false
               in
               Some (DType (lid, params, size, align, Forward forward))
           | _ -> assert false)
    |> List.of_seq
  in
  let topological_sort graph =
    let stack = ref [] in
    let rec visit key =
      (* If the stuff is not found, it means it is not within the dependency-resolution scope *)
      match Hashtbl.find_opt graph key with
      | None -> ()
      | Some (state, deps, content) -> (
          match !state with
          | `Done -> ()
          | `Visiting -> failwith "Cyclic type definitions detected"
          | `ToDo ->
              state := `Visiting;
              List.iter visit deps;
              state := `Done;
              stack := content :: !stack)
    in
    Hashtbl.iter (fun key _ -> visit key) graph;
    List.rev !stack
  in
  let rec typ_deps_in_typ typ =
    match typ with
    | TQualified lid -> [ lid ]
    | TArray (t, _) | TCgArray (t, _) -> typ_deps_in_typ t
    | TTuple types -> List.flatten (List.map typ_deps_in_typ types)
    | TAnonymous typ_def -> typ_def_kind_deps typ_def
    (* A pointer is not a complete dependency, i.e., it does not need to know the type size *)
    | TBuf _
    (* Likewise, a TArrow means a function pointer type *)
    | TArrow _
    (* Non-monomorphized generic external clearly do not participate in type dependencies
       Besides, its type arguments are not counted as dependencies *)
    | TApp _
    (* Likewise, if there is unresolved [TCgApp], it means its head must be external generic type *)
    | TCgApp _ -> []
    | TBound _ | TPoly _ ->
        failwith
          "Non-monomorphized types should have been removed before type dependency resolution!"
    | TBool | TUnit | TAny | TInt _ -> []
  and typ_def_kind_deps kind =
    match kind with
    | Flat fields -> List.flatten (List.map (fun (_, (typ, _)) -> typ_deps_in_typ typ) fields)
    | Variant branches ->
        List.flatten
          (List.map
             (fun (_, fields) ->
               List.flatten (List.map (fun (_, (typ, _)) -> typ_deps_in_typ typ) fields))
             branches)
    | Enum _ -> []
    | Union cases -> List.flatten (List.map (fun (_, typ) -> typ_deps_in_typ typ) cases)
    | Abbrev _ | Forward _ -> assert false
  in
  let partition_enums decls =
    let is_enum = function
      | DType (_, _, _, _, Enum _) -> true
      | _ -> false
    in
    List.partition is_enum decls
  in
  let topological_sort_type_defs type_defs =
    let typ_def_deps = function
      | DType (_, _, _, _, kind) -> typ_def_kind_deps kind
      | _ -> assert false
    in
    let get_dependencies decl = List.sort_uniq compare (typ_def_deps decl) in
    let graph =
      Hashtbl.to_seq type_defs
      |> Seq.map (fun (lid, decl) -> lid, (ref `ToDo, get_dependencies decl, decl))
      |> Hashtbl.of_seq
    in
    topological_sort graph
  in
  (* In the abbrev case, all possible names are dependencies *)
  let abbrev_typ_deps typ =
    (object
       inherit [_] Krml.Ast.reduce
       method private zero = []
       method private plus = ( @ )
       method! visit_TQualified () lid = [ lid ]
    end)
      #visit_typ
      () typ
    |> List.sort_uniq compare
  in
  let resolve_abbrev_dependencies abbrev_decls =
    let graph =
      Hashtbl.of_seq
        (List.to_seq abbrev_decls
        |> Seq.map (fun decl ->
               match decl with
               | DType (lid, _, _, _, Abbrev typ) ->
                   let deps = abbrev_typ_deps typ in
                   lid, (ref `ToDo, deps, decl)
               | _ -> assert false))
    in
    topological_sort graph
  in
  let resolve_typ_dependencies decls =
    let type_defs, abbrev_decls, forward_decls, other_decls = filter_partition_decls decls in
    let additional_forward_decls = remove_defined_forwards type_defs forward_decls in
    let forward_decls = gen_forward_decls type_defs in
    let sorted_type_defs = topological_sort_type_defs type_defs in
    let enums, non_enums = partition_enums sorted_type_defs in
    let abbrev_decls = resolve_abbrev_dependencies abbrev_decls in
    forward_decls @ additional_forward_decls @ enums @ abbrev_decls @ non_enums @ other_decls
  in
  List.map (fun (name, decls) -> name, resolve_typ_dependencies decls) files
