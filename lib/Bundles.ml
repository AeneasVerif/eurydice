(* A more modern version of the krml facility that matches on lids (instead of file names), and
   relies on a YAML file for configuration (rather than the cryptic syntax). *)

module L = Logging

module K = Krml.Ast

open Krml.Ast

type pattern =
  | Prefix of string list
  | Exact of string list
  | Lid of lident
  [@@deriving show]

type visibility = Api | Internal | Private
  [@@deriving show]

type file = {
  name: string;
  inline_static: bool;
  library: bool;
  definitions: (pattern * visibility) list;
  monomorphizations_using: (pattern * visibility) list;
  monomorphizations_of: (pattern * visibility) list;
  monomorphizations_exact: (pattern * visibility) list;
}

type config = file list

(** Loading & parsing *)

let load_config (path: string): Yaml.value =
  (* TODO: library not found: Yaml_unix *)
  let contents = Krml.Utils.file_get_contents path in
  match Yaml.of_string contents with
  | Error (`Msg s) ->
      Krml.Warn.fatal_error "Issue reading configuration file: %s" s
  | Ok v ->
      v

let parsing_error f = Krml.Warn.fatal_error ("Issue parsing configuration file: " ^^ f)

let parse_pattern (v: Yaml.value): pattern =
  match v with
  | `A vs ->
      let rec parse acc = function
        | (`String "*") :: [] -> Prefix (List.rev acc)
        | (`String "*") :: _ -> parsing_error "wildcards only allowed at the end"
        | (`String s) :: tl -> parse (s :: acc) tl
        | _ :: _ -> parsing_error "only strings in patterns"
        | [] -> Exact (List.rev acc)
      in
      parse [] vs
  | _ ->
      parsing_error "pattern not a list"

let parse_exact v: pattern =
  match parse_pattern v with
  | Exact lid -> Lid (Krml.KList.split_at_last lid)
  | _ -> parsing_error "monomorphizations_exact does not take wildcards"


let parse_file (v: Yaml.value): file =
  match v with
  | `O ls ->
      let count = ref 0 in
      let lookup k = try let r = List.assoc k ls in incr count; Some r with Not_found -> None in
      let lookup_ k ls = try let r = List.assoc k ls in incr count; Some r with Not_found -> None in
      let name =
        match lookup "name" with
        | Some (`String name) -> name
        | Some _ -> parsing_error "name not a string"
        | None -> parsing_error "missing name"
      in
      let inline_static =
        match lookup "inline_static" with
        | Some (`Bool inline_static) -> inline_static
        | Some _ -> parsing_error "inline_static not a bool"
        | None -> false
      in
      let library =
        match lookup "library" with
        | Some (`Bool library) -> library
        | Some _ -> parsing_error "library not a bool"
        | None -> false
      in
      let map_or f o k =
        match lookup_ k o with
        | None -> []
        | Some (`A l) -> List.map f l
        | Some _ -> failwith (k ^ " is not a list")
      in
      (* TODO: fix copy-pasting *)
      let parse_pattern_api p = parse_pattern p, Api in
      let parse_exact_api p = parse_exact p, Api in
      let definitions, monomorphizations_of, monomorphizations_using, monomorphizations_exact =
        match lookup "api" with
        | None -> [], [], [], []
        | Some (`A api) -> List.map parse_pattern_api api, [], [], []
        | Some (`O o) ->
            map_or parse_pattern_api o "patterns" @ map_or parse_exact_api o "exact",
            map_or parse_pattern_api o "monomorphizations_of",
            map_or parse_pattern_api o "monomorphizations_using",
            map_or parse_exact_api o "monomorphizations_exact"
        | Some _ -> parsing_error "api neither a list or a dictionary"
      in
      let parse_pattern_internal p = parse_pattern p, Internal in
      let parse_exact_internal p = parse_exact p, Internal in
      let definitions, monomorphizations_of, monomorphizations_using, monomorphizations_exact =
        match lookup "internal" with
        | None -> definitions, monomorphizations_of, monomorphizations_using, monomorphizations_exact
        | Some (`A internal_) -> definitions @ List.map parse_pattern_internal internal_, [], [], []
        | Some (`O o) ->
            definitions @ map_or parse_pattern_internal o "patterns" @ map_or parse_exact_internal o "exact",
            monomorphizations_of @ map_or parse_pattern_internal o "monomorphizations_of",
            monomorphizations_using @ map_or parse_pattern_internal o "monomorphizations_using",
            monomorphizations_exact @ map_or parse_exact_internal o "monomorphizations_exact"
        | Some _ -> parsing_error "internal neither a list or a dictionary"
      in
      let parse_pattern_private p = parse_pattern p, Private in
      let parse_exact_private p = parse_exact p, Private in
      let definitions, monomorphizations_of, monomorphizations_using, monomorphizations_exact =
        match lookup "private" with
        | None -> definitions, monomorphizations_of, monomorphizations_using, monomorphizations_exact
        | Some (`A private_) -> definitions @ List.map parse_pattern_private private_, [], [], []
        | Some (`O o) ->
            definitions @ map_or parse_pattern_private o "patterns" @ map_or parse_exact_private o "exact",
            monomorphizations_of @ map_or parse_pattern_private o "monomorphizations_of",
            monomorphizations_using @ map_or parse_pattern_private o "monomorphizations_using",
            monomorphizations_exact @ map_or parse_exact_private o "monomorphizations_exact"
        | Some _ -> parsing_error "private neither a list or a dictionary"
      in
      let include_ =
        match lookup "include_in_h" with
        | None -> []
        | Some (`A include_) -> List.map (function `String s -> Krml.Options.HeaderOnly name, s | _ -> parsing_error "include_in_h must be a string") include_
        | Some _ -> parsing_error "include_in_h must be a list"
      in
      let c_include_ =
        match lookup "include_in_c" with
        | None -> []
        | Some (`A include_) -> List.map (function `String s -> Krml.Options.COnly name, s | _ -> parsing_error "include_in_c must be a string") include_
        | Some _ -> parsing_error "include_in_c must be a list"
      in
      if !count < List.length ls then
        parsing_error "extraneous fields in file";
      Krml.Options.(add_early_include := include_ @ c_include_ @ !add_early_include);
      { name; definitions; inline_static; library; monomorphizations_using; monomorphizations_of; monomorphizations_exact }
  | _ ->
      parsing_error "file must be an object"

let parse_config (v: Yaml.value): config =
  match v with
  | `O [ "files", `A files ] ->
      List.map parse_file files
  | `O [ "files", _ ] ->
      parsing_error "files is not a sequence"
  | `O _ ->
      parsing_error "top-level object must be made of a single entry: files"
  | _ ->
      parsing_error "YAML file must be an object with key files"

(** Constructing bundles *)

let starts_with l prefix =
  List.length prefix <= List.length l &&
  fst (Krml.KList.split (List.length prefix) l) = prefix

(* `lid` matches pattern `p` *)
let matches lid p =
  match p with
  | Exact m ->
      m = fst lid
  | Prefix prefix ->
      starts_with (fst lid) prefix
  | Lid lid' ->
      lid = lid'

let find_map f l =
  List.find_map (fun (arg, ret) -> if f arg then Some ret else None) l

let mark_internal =
  let add_if name flags =
    let is_internal = List.mem Krml.Common.Internal flags in
    if not is_internal && not (Krml.Inlining.always_live name) then
      Krml.Common.Internal :: List.filter ((<>) Krml.Common.Private) flags
    else
      flags
  in
  function
  | DFunction (cc, flags, n_cgs, n, typ, name, binders, body) ->
      DFunction (cc, add_if name flags, n_cgs, n, typ, name, binders, body)
  | DGlobal (flags, name, n, typ, body) ->
      DGlobal (add_if name flags, name, n, typ, body)
  | DType (lid, flags, n_cgs, n, def) ->
      DType (lid, add_if lid flags, n_cgs, n, def)
  | DExternal (cc, flags, n_cg, n, lid, t, pp) ->
      DExternal (cc, add_if lid flags, n_cg, n, lid, t, pp)

let adjust vis decl =
  match vis with
  | Api -> decl
  | Private -> Krml.Bundles.mark_private decl
  | Internal -> mark_internal decl

let smaller acc vis =
  match acc, vis with
  | Api, _ -> vis
  | Private, Api -> Private
  | Private, _ -> vis
  | Internal, _ -> Internal

let bundle (files: Krml.Ast.file list) (c: config): files =
  let bundled = Hashtbl.create 137 in
  let bundle name decl =
    if Hashtbl.mem bundled name then
      Hashtbl.replace bundled name (decl :: Hashtbl.find bundled name)
    else
      Hashtbl.add bundled name [ decl ]
  in
  let record_library lid =
    Krml.Options.(library := Lid lid :: !library)
  in
  let record_inline_static lid =
    Krml.Options.(static_header := Lid lid :: !static_header)
  in
  let files = List.map (fun ((filename: string), (decls: Krml.Ast.decl list)) ->
    filename, List.filter_map (fun decl ->
      let lid = lid_of_decl decl in
      let rec find config =
        match config with
        | [] ->
            Krml.(KPrint.bprintf "%a doesn't go anywhere\n" PrintAst.Ops.plid lid);
            false
        | { name; definitions; inline_static; library; _ } :: config ->
            (* Krml.KPrint.bprintf "for %s, definitions are :\n" name; *)
            (* List.iter (fun (p, vis) -> *)
            (*   Krml.KPrint.bprintf "%s: %s\n" (show_visibility vis) (show_pattern p) *)
            (* ) definitions; *)
            match List.filter_map (fun (pat, vis) -> if matches lid pat then Some vis else None) definitions with
            | [] ->
              find config
            | vis_ ->
                (* Not sure this is the right semantics, oh well *)
                let vis = List.fold_left smaller Api vis_ in
                (* Krml.(KPrint.bprintf "%a goes into %s at vis %s\n" PrintAst.Ops.plid lid name (show_visibility vis)); *)
                (* if List.length vis_ > 1 then *)
                (*   Krml.(KPrint.bprintf "vis_ was: %s\n" (String.concat ", " (List.map show_visibility vis_))); *)
                let decl = adjust vis decl in
                bundle name decl;
                if inline_static then
                  record_inline_static lid;
                if library then
                  record_library lid;
                true
      in
      if find c then
        None
      else
        Some decl
    ) decls
  ) files in
  let files = List.filter (fun (filename, decls) ->
    (* Collision between the original crate name (e.g. libcrux_kyber) and the destination bundle
       (e.g. libcrux_kyber). *)
    if Hashtbl.mem bundled filename then begin
      List.iter (bundle filename) decls;
      false
    end else
      true
  ) files in
  Hashtbl.fold (fun filename decls acc ->
    (filename, List.rev decls) :: acc
  ) bundled (List.filter (fun (_, decls) -> decls <> []) files)


let libraries (files: Krml.Ast.file list): files =
  List.map (fun (f, decls) ->
    f, List.filter_map (fun d ->
      let lid = Krml.Ast.lid_of_decl d in
      if List.exists (fun p -> Krml.(Bundle.pattern_matches_lid p lid)) !Krml.Options.library then begin
        Logging.log "Libraries" "%a becomes abstract\n" Krml.PrintAst.Ops.plid lid;
        match d with
        | DType (_, _, _, _, Abbrev _) as t ->
            Some t
        | DType _ ->
            None
        | d ->
            Krml.Builtin.make_abstract_function_or_global d
      end else
        Some d
    ) decls
  ) files

let topological_sort decls =
  let module T = struct type color = White | Gray | Black end in
  let open T in
  let graph = Hashtbl.create 41 in
  List.iter (fun decl ->
    let deps = object (self)
        inherit [_] reduce
        method zero = []
        method plus = (@)
        method! visit_EQualified _ lid =
          [ lid ]
        method! visit_TQualified _ lid =
          [ lid ]
        method! visit_TApp _ lid ts =
          [ lid ] @ List.concat_map (self#visit_typ ()) ts
      end#visit_decl () decl
    in
    Hashtbl.add graph (lid_of_decl decl) (ref White, deps, decl)
  ) decls;
  let stack = ref [] in
  let rec dfs lid =
    if Hashtbl.mem graph lid then
      let r, deps, decl = Hashtbl.find graph lid in
      match !r with
      | Black -> ()
      | Gray -> failwith "dependency cycle"
      | White ->
          r := Gray;
          List.iter dfs deps;
          r := Black;
          stack := decl :: !stack
  in
  List.iter (fun decl -> dfs (lid_of_decl decl)) decls;
  List.rev !stack

(* Second phase of bundling, post-monomorphization. This is Eurydice-specific,
   as we oftentimes need to move definitions that have been /specialized/ using
   e.g. a platform-specific trait into their own file. *)
let reassign_monomorphizations (files: Krml.Ast.file list) (config: config) =
  let open Krml.Ast in
  let open Krml.PrintAst.Ops in
  (* Pure sanity check *)
  let count_decls files =
    List.fold_left (fun acc (_, decls) -> List.length decls + acc) 0 files
  in
  let c0 = count_decls files in
  let target_of_lid = Hashtbl.create 41 in
  let (|||) o1 o2 =
    match o1, o2 with
    | Some o1, Some o2 -> Some (smaller o1 o2)
    | Some o1, None
    | None, Some o1 -> Some o1
    | None, None -> None
  in
  let uses monomorphizations_using t =
    object
      inherit [_] reduce as super
      method zero = None
      method plus o1 o2 = if o1 = None then o2 else o1
      method! visit_TQualified _ lid' =
        find_map (matches lid') monomorphizations_using
      method! visit_TApp _ lid' ts =
        find_map (matches lid') monomorphizations_using ||| super#visit_TApp () lid' ts
    end#visit_typ () t
  in
  (* Review the function monomorphization state.
     Semantics of `monomorphizations_using`:
       if `lid`, below, is the result of a (function) monomorphization that
       *uses* (in its arguments, `cgs`, below) an `lid'` that matches a
       `monomorphizations_using` clause of file `name`, then `lid` moves to
       `name`.
     Semantics of `monomorphizations_of`: unlike above, this matches a
       generic lid (e.g., we want all the monomorphized instances of `Result` to
       go into a single file)
     Semantics of `monomorphizations_exact`: self-explanatory
  *)
  Hashtbl.iter (fun (generic_lid, cgs, ts) monomorphized_lid ->
    match List.find_map (fun { name; monomorphizations_using; monomorphizations_of; monomorphizations_exact; _ } ->
      (* Monomorphization using given trait name, amongst the arguments *)
      List.find_map (fun e ->
        match e.node with
        | EQualified lid' -> find_map (matches lid') monomorphizations_using
        | _ -> None
      ) cgs |||
      (* Monomorphization using given type name *)
      List.find_map (uses monomorphizations_using) ts |||
      (* Monomorphization of a given polymorphic name *)
      find_map (matches generic_lid) monomorphizations_of |||
      (* Monomorphization resulting in exactly this name *)
      find_map (matches monomorphized_lid) monomorphizations_exact |>
      Option.map (fun vis -> name, vis)
    ) config with
    | Some name ->
        Hashtbl.add target_of_lid monomorphized_lid name
    | None ->
        ()
  ) Krml.MonomorphizationState.generated_lids;
  (* Review the type monomorphization state. *)
  Hashtbl.iter (fun (generic_lid, ts, _) (_, monomorphized_lid) ->
    (* Krml.KPrint.bprintf "generic=%a, monomorphized=%a\n" plid generic_lid plid monomorphized_lid; *)
    match List.find_map (fun { name; monomorphizations_of; monomorphizations_using; monomorphizations_exact; _ } ->
      List.find_map (uses monomorphizations_using) ts |||
      find_map (matches generic_lid) monomorphizations_of |||
      find_map (matches monomorphized_lid) monomorphizations_exact |>
      Option.map (fun vis -> name, vis)
    ) config with
    | Some name ->
        Hashtbl.add target_of_lid monomorphized_lid name
    | None ->
        ()
  ) Krml.Monomorphization.state;
  (* Debug *)
  Hashtbl.iter (fun lid (target, vis) ->
    L.log "Reassign" "%a goes into %s (vis: %s)" plid lid target (show_visibility vis)
  ) target_of_lid;
  (* Filter the files, plucking out those that move and registering them under
     the right file name in `reassigned`. We maintain the invariant of one entry
     per key in the table. *)
  let reassigned = Hashtbl.create 41 in
  let files = List.map (fun (f, decls) ->
    f, List.filter (fun decl ->
      match Hashtbl.find_opt target_of_lid (lid_of_decl decl) with
      | None -> true
      | Some (target, vis) ->
          let decl = adjust vis decl in
          if Hashtbl.mem reassigned target then
            Hashtbl.replace reassigned target (decl :: Hashtbl.find reassigned target)
          else
            Hashtbl.add reassigned target [ decl ];
          false
    ) decls
  ) files in
  (* Extend each file with the definitions that are moving into it. *)
  let files = List.map (fun (f, decls) ->
    let reassigned =
      if Hashtbl.mem reassigned f then
        let r = Hashtbl.find reassigned f in
        Hashtbl.remove reassigned f;
        r
      else
        []
    in
    f, decls @ reassigned
  ) files in
  (* A quick topological sort to make sure type declarations come *before*
     functions that use them. *)
  let files = List.map (fun (f, decls) -> f, topological_sort decls) files in

  (* Deal with files that did not exist previously. *)
  let files = files @ Hashtbl.fold (fun f reassigned acc -> (f, reassigned) :: acc) reassigned [] in
  let c1 = count_decls files in
  assert (c0 = c1);
  files
