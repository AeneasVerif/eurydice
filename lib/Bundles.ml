(* A more modern version of the krml facility that matches on lids (instead of file names), and
   relies on a YAML file for configuration (rather than the cryptic syntax). *)

module L = Logging

type pattern =
  | Prefix of string list
  | Exact of string list

type file = {
  name: string;
  api: pattern list;
  private_: pattern list;
  inline_static: bool;
  library: bool;
  specialized_over_trait_impl: pattern list;
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
      let specialized_over_trait_impl =
        match lookup "specialized_over_trait_impl" with
        | None -> []
        | Some (`A specialized_over_trait_impl) -> List.map parse_pattern specialized_over_trait_impl
        | Some _ -> parsing_error "specialized_over_trait_impl not a list"
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
      Krml.Options.(add_include := include_ @ c_include_ @ !add_include);
      { name; api; private_; inline_static; library; specialized_over_trait_impl }
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

let reassign_monomorphizations files (config: config) =
  let open Krml.Ast in
  let open Krml.PrintAst.Ops in
  let target_of_lid = Hashtbl.create 41 in
  Hashtbl.iter (fun (_, cgs, _) lid ->
    match List.find_map (fun { name; specialized_over_trait_impl; _ } ->
      if List.exists (fun e ->
        match e.node with
        | EQualified lid -> List.exists (matches lid) specialized_over_trait_impl
        | _ -> false
      ) cgs then
        Some name
      else
        None
    ) config with
    | Some name ->
        Hashtbl.add target_of_lid lid name
    | None ->
        ()
  ) Krml.MonomorphizationState.generated_lids;
  Hashtbl.iter (fun lid target ->
    L.log "Reassign" "%a goes into %s" plid lid target
  ) target_of_lid;
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
  let files = List.map (fun (f, decls) ->
    f, decls @ (try List.rev (Hashtbl.find reassigned f) with Not_found -> [])
  ) files in
  files
