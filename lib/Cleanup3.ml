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

let add_extra_type_to_slice_index =
  object (_self)
    inherit [_] map as super

    method! visit_ETApp (((), _) as env) e cgs cgs' ts =
      match e.node, cgs, cgs', ts with
      | EQualified ([ "Eurydice" ], "slice_index"), [], [], [ t_elements ] ->
          ETApp (e, cgs, cgs', ts @ [ TBuf (t_elements, false) ])
      | _ -> super#visit_ETApp env e cgs cgs' ts

    method! visit_EApp env e es =
      match e.node, es with
      | ( ETApp
            ({ node = EQualified ([ "Eurydice" ], "slice_subslice"); _ }, [], [], t_elements :: _),
          [
            e_slice;
            {
              node = EFlat [ (Some "start", e_start); (Some "end", e_end) ];
              typ = TQualified ([ "core"; "ops"; "range" ], id);
              _;
            };
          ] )
        when KString.starts_with id "Range" ->
          EApp
            ( with_type TUnit
                (ETApp
                   ( with_type TUnit (EQualified ([ "Eurydice" ], "slice_subslice3")),
                     [],
                     [],
                     [ TBuf (t_elements, false) ] )),
              [ e_slice; e_start; e_end ] )
      | ( ETApp
            ({ node = EQualified ([ "Eurydice" ], "array_to_subslice"); _ }, _, [], t_elements :: _),
          [
            e_slice;
            {
              node = EFlat [ (Some "start", e_start); (Some "end", e_end) ];
              typ = TQualified ([ "core"; "ops"; "range" ], id);
              _;
            };
          ] )
        when KString.starts_with id "Range" ->
          EApp
            ( with_type TUnit
                (ETApp
                   ( with_type TUnit (EQualified ([ "Eurydice" ], "array_to_subslice3")),
                     [],
                     [],
                     [ TBuf (t_elements, false) ] )),
              [ e_slice; e_start; e_end ] )
      | _ -> super#visit_EApp env e es
  end

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
