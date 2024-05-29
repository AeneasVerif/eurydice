(* A more modern version of the krml facility that matches on lids (instead of file names), and
   relies on a YAML file for configuration (rather than the cryptic syntax). *)

module L = Logging

open Krml.Ast

type pattern =
  | Prefix of string list
  | Exact of string list

type file = {
  name: string;
  api: pattern list;
  private_: pattern list;
  inline_static: bool;
  library: bool;
  monomorphizations_using: pattern list;
  monomorphizations_of: pattern list;
  monomorphizations_exact: lident list;
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

let parse_exact v: lident =
  match parse_pattern v with
  | Exact lid -> Krml.KList.split_at_last lid
  | _ -> parsing_error "monomorphizations_exact does not take wildcards"


let parse_file (v: Yaml.value): file =
  match v with
  | `O ls ->
      let count = ref 0 in
      let lookup k = try let r = List.assoc k ls in incr count; Some r with Not_found -> None in
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
      let api =
        match lookup "api" with
        | None -> []
        | Some (`A api) -> List.map parse_pattern api
        | Some _ -> parsing_error "api not a list"
      in
      let monomorphizations_of =
        match lookup "monomorphizations_of" with
        | None -> []
        | Some (`A monomorphizations_of) -> List.map parse_pattern monomorphizations_of
        | Some _ -> parsing_error "monomorphizations_of not a list"
      in
      let monomorphizations_using =
        match lookup "monomorphizations_using" with
        | None -> []
        | Some (`A monomorphizations_using) -> List.map parse_pattern monomorphizations_using
        | Some _ -> parsing_error "monomorphizations_using not a list"
      in
      let monomorphizations_exact =
        match lookup "monomorphizations_exact" with
        | None -> []
        | Some (`A monomorphizations_exact) -> List.map parse_exact monomorphizations_exact
        | Some _ -> parsing_error "monomorphizations_exact not a list"
      in
      let private_ =
        match lookup "private" with
        | None -> []
        | Some (`A private_) -> List.map parse_pattern private_
        | Some _ -> parsing_error "private not a list"
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
      { name; api; private_; inline_static; library; monomorphizations_using; monomorphizations_of; monomorphizations_exact }
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

open Krml.Ast

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

let bundle (files: file list) (c: config): files =
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
  let files = List.map (fun (filename, decls) ->
    filename, List.filter_map (fun decl ->
      let lid = lid_of_decl decl in
      let rec find files =
        match files with
        | [] ->
            Krml.(KPrint.bprintf "%a doesn't go anywhere\n" PrintAst.Ops.plid lid);
            false
        | { name; api; private_; inline_static; library; _ } :: files ->
            if List.exists (matches lid) api then begin
              (* Krml.(KPrint.bprintf "%a goes (api) into %s\n" PrintAst.Ops.plid lid name); *)
              bundle name decl;
              if inline_static then
                record_inline_static lid;
              if library then
                record_library lid;
              true
            end else if List.exists (matches lid) private_ then begin
              (* Krml.(KPrint.bprintf "%a goes (private) into %s\n" PrintAst.Ops.plid lid name); *)
              bundle name (Krml.Bundles.mark_private decl);
              if inline_static then
                record_inline_static lid;
              if library then
                record_library lid;
              true
            end else
              find files
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


let libraries (files: file list): files =
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

(* Second phase of bundling, post-monomorphization. This is Eurydice-specific,
   as we oftentimes need to move definitions that have been /specialized/ using
   e.g. a platform-specific trait into their own file. *)
let reassign_monomorphizations files (config: config) =
  let open Krml.Ast in
  let open Krml.PrintAst.Ops in
  (* Pure sanity check *)
  let count_decls files =
    List.fold_left (fun acc (_, decls) -> List.length decls + acc) 0 files
  in
  let c0 = count_decls files in
  let target_of_lid = Hashtbl.create 41 in
  let uses monomorphizations_using t =
    object
      inherit [_] reduce as super
      method zero = false
      method plus = (||)
      method! visit_TQualified _ lid' =
        List.exists (matches lid') monomorphizations_using
      method! visit_TApp _ lid' ts =
        List.exists (matches lid') monomorphizations_using || super#visit_TApp () lid' ts
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
      if List.exists (fun e ->
        match e.node with
        | EQualified lid' -> List.exists (matches lid') monomorphizations_using
        | _ -> false
      ) cgs || List.exists (uses monomorphizations_using) ts then
        Some name
      else if List.exists (matches generic_lid) monomorphizations_of then
        Some name
      else if List.mem monomorphized_lid monomorphizations_exact then
        Some name
      else
        None
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
      if List.exists (fun t ->
        uses monomorphizations_using t
      ) ts then
        Some name
      else if List.exists (matches generic_lid) monomorphizations_of then
        Some name
      else if List.mem monomorphized_lid monomorphizations_exact then
        Some name
      else
        None
    ) config with
    | Some name ->
        Hashtbl.add target_of_lid monomorphized_lid name
    | None ->
        ()
  ) Krml.Monomorphization.state;
  (* Debug *)
  Hashtbl.iter (fun lid target ->
    L.log "Reassign" "%a goes into %s" plid lid target
  ) target_of_lid;
  (* Filter the files, plucking out those that move and registering them under
     the right file name in `reassigned`. We maintain the invariant of one entry
     per key in the table. *)
  let reassigned = Hashtbl.create 41 in
  let files = List.map (fun (f, decls) ->
    f, List.filter (fun decl ->
      match Hashtbl.find_opt target_of_lid (lid_of_decl decl) with
      | None -> true
      | Some target ->
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
  let files = List.map (fun (f, decls) ->
    let module T = struct type color = White | Gray | Black end in
    let open T in
    let graph = Hashtbl.create 41 in
    List.iter (fun decl ->
      let deps = object (self)
          inherit [_] reduce
          method zero = []
          method plus = (@)
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
    f, List.rev !stack
  ) files in

  (* Deal with files that did not exist previously. *)
  let files = files @ Hashtbl.fold (fun f reassigned acc -> (f, reassigned) :: acc) reassigned [] in
  let c1 = count_decls files in
  assert (c0 = c1);
  files
