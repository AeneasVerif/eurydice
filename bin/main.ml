let () =
  let usage = Printf.sprintf
{|Eurydice: from Rust to C

Usage: %s [OPTIONS] FILES

FILES are .llbc files produced by Charon. Eurydice will generate one C file per
llbc file.

Supported options:|}
    Sys.argv.(0)
  in
  let module O = Eurydice.Options in
  let debug s = Krml.Options.debug_modules := Krml.KString.split_on_char ',' s @ !Krml.Options.debug_modules in
  let spec = [
    "--log", Arg.Set_string O.log_level, " log level, use * for everything";
    "--debug", Arg.String debug, " debug options, to be passed to krml";
    "--output", Arg.Set_string Krml.Options.tmpdir, " output directory in which to write files";
    "--config", Arg.Set_string O.config, " YAML configuration file";
    "--const_generics", Arg.String (fun s ->
      match String.lowercase_ascii s with
      | "runtime" -> O.cg := Runtime
      | "static" -> O.cg := Static
      | _ -> failwith "Unrecognized value for option --const_generics"
    ), " runtime, or static (default): pass const-generic arguments at runtime, \
      or perform whole-program static monomorphization (larger code size)";
  ] in
  let spec = Arg.align spec in
  let files = ref [] in
  let fatal_error = Printf.kprintf (fun s -> print_endline s; exit 255) in
  let anon_fun f =
    if Filename.check_suffix f ".llbc" then
      files := f :: !files
    else
      fatal_error "Unknown file extension for %s" f
  in
  begin try
    Arg.parse spec anon_fun usage
  with e ->
    Printf.printf "Error parsing command-line: %s\n%s\n" (Printexc.get_backtrace ()) (Printexc.to_string e);
    fatal_error "Incorrect invocation, was: %s\n"
      (String.concat "␣" (Array.to_list Sys.argv))
  end;

  if !files = [] then
    fatal_error "%s" (Arg.usage_string spec usage);

  let terminal_width = Terminal.Size.(match get_dimensions () with Some d -> d.columns | None -> 80) in
  let pfiles b files =
    PPrint.(ToBuffer.pretty 0.95 terminal_width b (Krml.PrintAst.print_files files ^^ hardline))
  in

  (* This is where the action happens *)
  Eurydice.Logging.enable_logging !O.log_level;
  (* Type applications are compiled as a regular external type. *)
  Krml.(Options.(
    allow_tapps := true;
    minimal := true;
    curly_braces := true;
    add_include := [ All, "\"eurydice_glue.h\"" ];
    (* header := "/* This file compiled from Rust to C by Eurydice \ *)
    (*   <http://github.com/aeneasverif/eurydice> */"; *)
    parentheses := true;
    no_shadow := true;
    static_header := [
      Bundle.Prefix [ "core"; "convert" ];
      Bundle.Prefix [ "core"; "num" ]
    ];
  ));
  Krml.(Warn.parse_warn_error !Options.warn_error);

  Krml.Helpers.is_readonly_builtin_lid_ :=
    (let is_readonly_pure_lid_ = !Krml.Helpers.is_readonly_builtin_lid_ in
    fun lid ->
      is_readonly_pure_lid_ lid ||
        match lid with
        | [ "Eurydice" ], "vec_len"
        | [ "Eurydice" ], "vec_index"
        | [ "core"; "num"; "u32"; _ ], "rotate_left" ->
            true
        | _ ->
            false
    );

  let files = Eurydice.Builtin.files @ List.map (fun filename ->
    let llbc = Eurydice.LoadLlbc.load_file filename in
    Eurydice.Builtin.adjust (Eurydice.AstOfLlbc.file_of_crate llbc)
  ) !files in

  Printf.printf "1️⃣ LLBC ➡️  AST\n";
  let files = Eurydice.PreCleanup.precleanup files in

  Eurydice.Logging.log "Phase1" "%a" pfiles files;
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    exit 1;

  Printf.printf "2️⃣ Cleanup\n";
  let files =
    if !O.config = "" then
      files
    else
      let config = Eurydice.Bundles.load_config !O.config in
      let config = Eurydice.Bundles.parse_config config in
      let files = Eurydice.Bundles.bundle files config in
      let files = Krml.Bundles.topological_sort files in
      Krml.KPrint.bprintf "File order after topological sort: %s\n"
        (String.concat ", " (List.map fst files));
      files
  in
  let files = Eurydice.Cleanup1.cleanup files in

  Eurydice.Logging.log "Phase2" "%a" pfiles files;
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    exit 1;

  let runtime_cg = Eurydice.Options.runtime_cg () in

  Printf.printf "3️⃣ Monomorphization, datatypes\n";
  let files = if runtime_cg then Eurydice.RuntimeCg.disable_cg_monomorphization#visit_files () files else files in
  let files = Krml.Monomorphization.functions files in
  let files = Krml.Monomorphization.datatypes files in
  Eurydice.Logging.log "Phase2.5" "%a" pfiles files;
  let files =
    if runtime_cg then
      let missing = Eurydice.RuntimeCg.enumerate_cg_monomorphizations files in
      Eurydice.RuntimeCg.debug missing;
      files
    else
      files
  in
  let files = Krml.Inlining.drop_unused files in
  let files = Eurydice.Cleanup2.remove_array_repeats#visit_files () files in
  let files = Eurydice.Cleanup2.rewrite_slice_to_array#visit_files () files in
  let files = Krml.DataTypes.simplify files in
  let files = Krml.DataTypes.optimize files in
  let _, files = Krml.DataTypes.everything files in
  let files = Eurydice.Cleanup2.remove_trivial_ite#visit_files () files in
  let files = Eurydice.Cleanup2.remove_trivial_into#visit_files () files in
  let files = Krml.Structs.pass_by_ref files in
  let files = Eurydice.Cleanup2.remove_literals#visit_files () files in
  let files = Krml.Simplify.optimize_lets files in
  (* let files = Eurydice.Cleanup2.break_down_nested_arrays#visit_files () files in *)
  let files = Eurydice.Cleanup2.remove_implicit_array_copies#visit_files (0, 0) files in
  let files = Krml.Simplify.sequence_to_let#visit_files () files in
  let files = Krml.Simplify.hoist#visit_files [] files in
  let files = Krml.Simplify.fixup_hoist#visit_files () files in
  let files = Krml.Simplify.misc_cosmetic#visit_files () files in
  let files = Krml.Simplify.let_to_sequence#visit_files () files in
  let files = Krml.Inlining.cross_call_analysis files in
  let files = Krml.Simplify.remove_unused files in
  let files = Eurydice.Cleanup2.remove_array_from_fn#visit_files () files in
  (* Macros stemming from globals *)
  let files, macros = Eurydice.Cleanup2.build_macros files in
  let files = if runtime_cg then Eurydice.RuntimeCg.erase_and_decay_cgs#visit_files (0, 0) files else files in


  Eurydice.Logging.log "Phase3" "%a" pfiles files;
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then
    exit 1;

  let scope_env = Krml.Simplify.allocate_c_env files in
  let files = Eurydice.Cleanup3.decay_cg_externals#visit_files (scope_env, false) files in
  let macros =
    let cg_macros = Eurydice.Cleanup3.build_cg_macros#visit_files () files in
    Krml.Idents.LidSet.(union (union macros cg_macros) Eurydice.Builtin.macros)
  in
  let c_name_map = Krml.GlobalNames.mapping (fst scope_env) in

  let open Krml in
  let file_of_map = Bundle.mk_file_of files in
  let deps = Bundles.direct_dependencies_with_internal files file_of_map in
  let files = List.map (fun (f, ds) ->
    let is_fine = function
      | ["LowStar"; "Ignore"], "ignore"
      | "Eurydice" :: _, _ ->
      (* | "core" :: _, _ -> *)
          true
      | _ ->
          false
    in
    f, List.filter_map (fun d ->
      match d with
      | Krml.Ast.DExternal (_, _, _, _, lid, t, _) when Krml.Monomorphization.(
        (has_variables [ t ] || has_cg_array [ t ]) && not (is_fine lid)
      ) ->
          KPrint.bprintf "Warning: %a is a type/const-polymorphic assumed function, \
            must be implemented with a macro, dropping it\n" Krml.PrintAst.Ops.plid lid;
          None
      | _ ->
          Some d
    ) ds
  ) files in
  let files = AstToCStar.mk_files files c_name_map Idents.LidSet.empty macros in

  let headers = CStarToC11.mk_headers c_name_map files in
  let deps = CStarToC11.drop_empty_headers deps headers in
  let internal_headers = Bundles.StringSet.of_list
    (List.filter_map (function (name, C11.Internal _) -> Some name | _ -> None) headers)
  in
  let public_headers = Bundles.StringSet.of_list
    (List.filter_map (function (name, C11.Public _) -> Some name | _ -> None) headers)
  in
  let files = CStarToC11.mk_files c_name_map files in
  let files = List.filter (fun (_, decls) -> List.length decls > 0) files in
  Krml.Output.maybe_create_internal_dir headers;
  ignore (Output.write_c files internal_headers deps);
  ignore (Output.write_h headers public_headers deps);

  Printf.printf "✅ Done\n"
