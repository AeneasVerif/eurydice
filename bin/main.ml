let () =
  let usage =
    Printf.sprintf
      {|Eurydice: from Rust to C

Usage: %s [OPTIONS] FILES

FILES are .llbc files produced by Charon. Eurydice will generate one C file per
llbc file.

Supported options:|}
      Sys.argv.(0)
  in
  let module O = Eurydice.Options in
  let debug s =
    Krml.Options.debug_modules := Krml.KString.split_on_char ',' s @ !Krml.Options.debug_modules
  in
  let funroll_loops = ref 16 in
  let spec =
    [
      "--log", Arg.Set_string O.log_level, " log level, use * for everything";
      "--debug", Arg.String debug, " debug options, to be passed to krml";
      "--output", Arg.Set_string Krml.Options.tmpdir, " output directory in which to write files";
      ( "--header",
        Arg.Set_string Krml.Options.header,
        " path to a header file to be prepended to the generated C" );
      "--config", Arg.Set_string O.config, " YAML configuration file";
      ( "--keep-going",
        Arg.Set O.keep_going,
        " keep going even though extracting some definitions might fail" );
      "-fcomments", Arg.Set O.comments, " keep inline comments";
      "-funroll-loops", Arg.Set_int funroll_loops, " unrool loops up to N";
      ( "-fc++17-compat",
        Arg.Set Krml.Options.cxx17_compat,
        " instead of generating C11/C++20 code (default), generate C++17-only code" );
    ]
  in
  let spec = Arg.align spec in
  let files = ref [] in
  let fatal_error =
    Printf.ksprintf (fun s ->
        print_endline s;
        exit 255)
  in
  let anon_fun f =
    if Filename.check_suffix f ".llbc" then
      files := f :: !files
    else
      fatal_error "Unknown file extension for %s" f
  in
  begin
    try Arg.parse spec anon_fun usage
    with e ->
      Printf.printf "Error parsing command-line: %s\n%s\n" (Printexc.get_backtrace ())
        (Printexc.to_string e);
      fatal_error "Incorrect invocation, was: %s\n" (String.concat "␣" (Array.to_list Sys.argv))
  end;

  if !files = [] then
    fatal_error "%s" (Arg.usage_string spec usage);

  let terminal_width =
    Terminal.Size.(
      match get_dimensions () with
      | Some d -> d.columns
      | None -> 80)
  in
  let pfiles b files =
    PPrint.(ToBuffer.pretty 0.95 terminal_width b (Krml.PrintAst.print_files files ^^ hardline))
  in
  let fail file line =
    Printf.printf "%s:%d exiting\n" file line;
    exit 1
  in

  (* This is where the action happens *)
  Eurydice.Logging.enable_logging !O.log_level;
  (* Type applications are compiled as a regular external type. *)
  Krml.(
    Options.(
      allow_tapps := true;
      minimal := true;
      curly_braces := true;
      add_very_early_include := [ All, "\"eurydice_glue.h\"" ];
      parentheses := true;
      no_shadow := true;
      extern_c := true;
      cxx_compat := true;
      unroll_loops := !funroll_loops;
      static_header :=
        [
          Bundle.Prefix [ "core"; "convert" ];
          Bundle.Prefix [ "core"; "num" ];
          Bundle.Prefix [ "Eurydice"; "Int128" ];
        ];
      Warn.parse_warn_error (!warn_error ^ "+8"));
    Monomorphization.NameGen.short_names := true;
    AstToCStar.no_return_type_lids :=
      [
        [ "Eurydice" ], "slice_index";
        [ "Eurydice" ], "slice_subslice";
        [ "Eurydice" ], "slice_subslice3";
        [ "Eurydice" ], "slice_subslice_to";
        [ "Eurydice" ], "slice_subslice_from";
        [ "Eurydice" ], "array_to_slice";
        [ "Eurydice" ], "array_to_subslice";
        [ "Eurydice" ], "array_to_subslice3";
        [ "Eurydice" ], "array_to_subslice_to";
        [ "Eurydice" ], "array_to_subslice_from";
        [ "Eurydice" ], "array_repeat";
        [ "Eurydice" ], "slice_len";
        [ "Eurydice" ], "slice_copy";
        [ "Eurydice" ], "array_eq";
        [ "Eurydice" ], "slice_to_array2";
      ]);

  (* Some logic for more precisely tracking readonly functions, so as to remove
     excessive uu__ variables. *)
  let readonly_lids = Hashtbl.create 41 in
  let readonly_map = Hashtbl.create 41 in
  let fill_readonly_table files =
    List.iter
      (fun (_, decls) ->
        List.iter
          (function
            | Krml.Ast.DFunction (_, _, _, _, _, name, _, body) ->
                Hashtbl.add readonly_map name body
            | _ -> ())
          decls)
      files
  in

  Krml.(
    Helpers.is_readonly_builtin_lid_ :=
      let is_readonly_pure_lid_ = !Helpers.is_readonly_builtin_lid_ in
      fun lid t ->
        let ret_t, _ = Helpers.flatten_arrow t in
        is_readonly_pure_lid_ lid t
        || (match lid with
           | "libcrux_intrinsics" :: _, _ -> ret_t <> TUnit
           | [ "Eurydice" ], "vec_len"
           | [ "Eurydice" ], "vec_index"
           | [ "Eurydice" ], "slice_index"
           | [ "Eurydice" ], "slice_len"
           | [ "Eurydice" ], "slice_to_ref_array"
           | [ "Eurydice" ], "slice_subslice"
           | [ "Eurydice" ], "slice_subslice2"
           | [ "Eurydice" ], "slice_subslice3"
           | [ "Eurydice" ], "slice_subslice_from"
           | [ "Eurydice" ], "slice_of_dst"
           | [ "Eurydice" ], "array_to_slice"
           | [ "Eurydice" ], "array_to_subslice"
           | [ "Eurydice" ], "array_to_subslice2"
           | [ "Eurydice" ], "array_to_subslice3"
           | [ "Eurydice" ], "array_repeat"
           | [ "core"; "mem" ], "size_of"
           | "core" :: "slice" :: _, "as_mut_ptr"
           | "core" :: "num" :: _, ("rotate_left" | "from_le_bytes" | "wrapping_add") -> true
           | _ -> false)
        || Hashtbl.mem readonly_lids lid
        ||
        match Hashtbl.find_opt readonly_map lid with
        | Some body ->
            let ro = Helpers.is_readonly_c_expression body in
            if ro then
              Hashtbl.add readonly_lids lid ();
            ro
        | _ -> false);

  let files =
    Eurydice.Builtin.files
    @ [
        Eurydice.PreCleanup.merge
          (List.map
             (fun filename ->
               let llbc = Eurydice.LoadLlbc.load_file filename in
               Eurydice.AstOfLlbc.file_of_crate llbc)
             !files);
      ]
  in

  Printf.printf "1️⃣ LLBC ➡️  AST\n";
  Eurydice.Logging.log "Phase0" "%a" pfiles files;
  let files = Eurydice.PreCleanup.precleanup files in

  Eurydice.Logging.log "Phase1" "%a" pfiles files;
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    fail __FILE__ __LINE__;

  Printf.printf "2️⃣ Cleanup\n";
  let config =
    if !O.config = "" then
      None
    else
      Some (Eurydice.Bundles.parse_config (Eurydice.Bundles.load_config !O.config))
  in
  let files =
    match config with
    | None -> files
    | Some config ->
        let files = Eurydice.Bundles.bundle files config in
        let files = Eurydice.Bundles.libraries files in
        let files = Krml.Bundles.topological_sort files in
        Krml.KPrint.bprintf "File order after topological sort: %s\n"
          (String.concat ", " (List.map fst files));
        files
  in
  let files = Eurydice.Cleanup1.cleanup files in

  Eurydice.Logging.log "Phase2" "%a" pfiles files;
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    fail __FILE__ __LINE__;

  Printf.printf "3️⃣ Monomorphization, datatypes\n";
  let files = Eurydice.Cleanup2.resugar_loops#visit_files () files in
  Eurydice.Logging.log "Phase2.1" "%a" pfiles files;
  (* Sanity check for the big rewriting above. *)
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    fail __FILE__ __LINE__;
  let files = Eurydice.Cleanup2.improve_names files in
  let files = Eurydice.Cleanup2.recognize_asserts#visit_files () files in
  (* Temporary workaround until Aeneas supports nested loops *)
  let files = Eurydice.Cleanup2.inline_loops#visit_files () files in
  (* Following the krml order of phases here *)
  let files = Krml.Inlining.inline_type_abbrevs files in
  let files = Krml.Monomorphization.functions files in
  Eurydice.Logging.log "Phase2.12" "%a" pfiles files;
  let files = Krml.Simplify.optimize_lets files in
  let files = Krml.DataTypes.simplify files in
  (* Must happen now, before Monomorphization.datatypes, because otherwise
     MonomorphizationState.state gets filled with lids that later on get eliminated on the basis
     that they were empty structs to begin with, which would send Checker off the rails *)
  let files = Krml.DataTypes.remove_empty_structs files in
  let files = Krml.Monomorphization.datatypes files in
  (* Cannot use remove_unit_buffers as it is technically incorrect *)
  let files = Krml.DataTypes.remove_unit_fields#visit_files () files in
  Eurydice.Logging.log "Phase2.13" "%a" pfiles files;
  let files = Krml.Inlining.inline files in
  let files =
    match config with
    | None -> files
    | Some config ->
        let files = Eurydice.Bundles.reassign_monomorphizations files config in
        Eurydice.Logging.log "Phase2.15" "%a" pfiles files;
        let files = Krml.Bundles.topological_sort files in
        files
  in
  Eurydice.Logging.log "Phase2.2" "%a" pfiles files;
  (* Sanity check for the big rewriting above. *)
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    fail __FILE__ __LINE__;
  let files = Krml.Inlining.drop_unused files in
  let files = Eurydice.Cleanup2.remove_array_temporaries#visit_files () files in
  Eurydice.Logging.log "Phase2.25" "%a" pfiles files;
  let files = Eurydice.Cleanup2.remove_array_repeats#visit_files () files in
  Eurydice.Logging.log "Phase2.26" "%a" pfiles files;
  let files = Eurydice.Cleanup2.rewrite_slice_to_array#visit_files () files in
  let ((map, _, _) as map3), files = Krml.DataTypes.everything files in
  Eurydice.Cleanup2.fixup_monomorphization_map map;
  let files = Eurydice.Cleanup2.remove_discriminant_reads map3 files in
  Eurydice.Logging.log "Phase2.3" "%a" pfiles files;
  let files = Eurydice.Cleanup2.remove_trivial_ite#visit_files () files in
  Eurydice.Logging.log "Phase2.4" "%a" pfiles files;
  let files = Eurydice.Cleanup2.remove_trivial_into#visit_files () files in
  let files = Krml.Structs.pass_by_ref files in
  Eurydice.Logging.log "Phase2.5" "%a" pfiles files;
  let files = Eurydice.Cleanup2.remove_literals files in
  (* Eurydice does something more involved than krml and performs a conservative
     approximation of functions that are known to be pure readonly (i.e.,
     functions that do not write to memory). *)
  fill_readonly_table files;
  let files = Krml.Simplify.optimize_lets files in
  Eurydice.Logging.log "Phase2.55" "%a" pfiles files;
  let files = Eurydice.Cleanup2.remove_array_from_fn files in
  Eurydice.Logging.log "Phase2.6" "%a" pfiles files;
  (* remove_array_from_fn, above, creates further opportunities for removing unused functions. *)
  let files = Krml.Inlining.drop_unused files in
  let files = Eurydice.Cleanup2.remove_implicit_array_copies#visit_files () files in
  (* Creates opportunities for removing unused variables *)
  let files = Eurydice.Cleanup2.remove_assign_return#visit_files () files in
  (* These two need to come before... *)
  let files = Krml.Inlining.cross_call_analysis files in
  let files = Krml.Simplify.remove_unused files in
  Eurydice.Logging.log "Phase2.7" "%a" pfiles files;
  (* This chunk which reuses key elements of simplify2 *)
  let files = Eurydice.Cleanup2.check_addrof#visit_files () files in
  let files = Krml.Simplify.sequence_to_let#visit_files () files in
  let files = Eurydice.Cleanup2.hoist#visit_files [] files in
  let files = Eurydice.Cleanup2.fixup_hoist#visit_files () files in
  Eurydice.Logging.log "Phase2.75" "%a" pfiles files;
  let files = Eurydice.Cleanup2.globalize_global_locals files in
  Eurydice.Logging.log "Phase2.8" "%a" pfiles files;
  let files = Eurydice.Cleanup2.reconstruct_for_loops#visit_files () files in
  let files = Krml.Simplify.misc_cosmetic#visit_files () files in
  let files = Krml.Simplify.let_to_sequence#visit_files () files in
  Eurydice.Logging.log "Phase2.9" "%a" pfiles files;
  let files = Eurydice.Cleanup2.float_comments files in
  Eurydice.Logging.log "Phase2.95" "%a" pfiles files;
  let files = Eurydice.Cleanup2.bonus_cleanups#visit_files [] files in
  (* Macros stemming from globals -- FIXME why is this not Krml.AstToCStar.mk_macros_set? *)
  let files, macros = Eurydice.Cleanup2.build_macros files in

  Eurydice.Logging.log "Phase3" "%a" pfiles files;
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    fail __FILE__ __LINE__;

  let scope_env = Krml.Simplify.allocate_c_env files in
  Eurydice.Cleanup3.(also_skip_prefix_for_external_types scope_env)#visit_files () files;
  let files = Eurydice.Cleanup3.decay_cg_externals#visit_files (scope_env, false) files in
  let files = Eurydice.Cleanup3.add_extra_type_to_slice_index#visit_files () files in
  Eurydice.Logging.log "Phase3.1" "%a" pfiles files;
  let c_name_map = Krml.GlobalNames.mapping (fst scope_env) in

  let open Krml in
  let file_of_map = Bundle.mk_file_of files in
  let deps = Bundles.direct_dependencies_with_internal files file_of_map in
  let files =
    List.map
      (fun (f, ds) ->
        let is_fine = function
          | [ "LowStar"; "Ignore" ], "ignore" | "Eurydice" :: _, _ ->
              (* | "core" :: _, _ -> *)
              true
          | _ -> false
        in
        ( f,
          List.filter_map
            (fun d ->
              match d with
              | Krml.Ast.DExternal (_, _, _, _, lid, t, _)
                when Krml.Monomorphization.(
                       (has_variables [ t ] || has_cg_array [ t ]) && not (is_fine lid)) ->
                  KPrint.bprintf
                    "Warning: %a is a type/const-polymorphic assumed function, must be implemented \
                     with a macro, dropping it\n"
                    Krml.PrintAst.Ops.plid lid;
                  None
              | _ -> Some d)
            ds ))
      files
  in
  Eurydice.Logging.log "Phase3.2" "%a" pfiles files;

  (* The following phase reads the "target" parameter for each file, if any, from the config
     and if set, then it adds the attribute `KRML_ATTRIBUTE_TARGET(target)` to each function
     in the generated C file. This is used, in particular, to mark certain functions as only
     to be compiled on target architectures like `avx2` *)
  let files =
    List.map
      (fun (f, ds) ->
        let open Eurydice.Bundles in
        let target_attribute =
          match config with
          | None -> ""
          | Some c -> (
              match List.find_opt (fun (x : file) -> x.name = f) c with
              | None -> ""
              | Some f -> f.target)
        in
        ( f,
          List.filter_map
            (fun d ->
              match d with
              | Krml.Ast.DFunction (cc, fl, x, y, t, l, b, e) when target_attribute <> "" ->
                  Some (Krml.Ast.DFunction (cc, fl @ [ Target target_attribute ], x, y, t, l, b, e))
              | _ -> Some d)
            ds ))
      files
  in

  Eurydice.Logging.log "Phase3.3" "%a" pfiles files;
  let files =
    List.map
      (fun (f, ds) ->
        ( f,
          List.filter
            (fun d -> not (Krml.Idents.LidSet.mem (Krml.Ast.lid_of_decl d) Eurydice.Builtin.skip))
            ds ))
      files
  in
  let files = AstToCStar.mk_files files c_name_map Idents.LidSet.empty macros in

  (* Uncomment to debug C* AST *)
  (* List.iter (fun (f, p) -> *)
  (*   print_endline f; *)
  (*   print_endline (CStar.show_program p ); *)
  (*   print_endline "" *)
  (* ) files; *)

  let headers = CStarToC11.mk_headers c_name_map files in
  let deps = CStarToC11.drop_empty_headers deps headers in
  let internal_headers =
    Bundles.StringSet.of_list
      (List.filter_map
         (function
           | name, C11.Internal _ -> Some name
           | _ -> None)
         headers)
  in
  let public_headers =
    Bundles.StringSet.of_list
      (List.filter_map
         (function
           | name, C11.Public _ -> Some name
           | _ -> None)
         headers)
  in
  let files = CStarToC11.mk_files c_name_map files in
  let files = List.filter (fun (_, decls) -> List.length decls > 0) files in
  Krml.Output.maybe_create_internal_dir headers;
  Krml.Driver.fstar := "dummy";
  ignore (Output.write_c files internal_headers deps);
  ignore (Output.write_h headers public_headers deps);

  Printf.printf "✅ Done\n"
