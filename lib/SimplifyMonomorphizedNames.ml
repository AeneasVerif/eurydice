module K = Krml.Ast

module LidentMap = Map.Make (struct
  type t = K.lident

  let compare = compare
end)

let lids_of_decls (decls : K.program) : (K.lident * Krml.Common.flag list) list
    =
  List.map (fun d -> (K.lid_of_decl d, K.flags_of_decl d)) decls

module MonomorphizationSource = struct
  type t = {
    original_lid : K.lident;
    const_generics : K.expr list;
    types : K.typ list;
  }
  (** Describes a monomorphization *)

  open struct
    open struct
      let helper_buffer (type a) (f : Buffer.t -> a -> unit) (x : a) : string =
        let buf = Buffer.create 80 in
        f buf x;
        Buffer.contents buf

      let ( >> ) f g x = g (f x)
      let drop_newlines = String.split_on_char '\n' >> String.concat " "
    end

    let pexpr = helper_buffer Krml.PrintAst.pexpr >> drop_newlines
    let ptyp = helper_buffer Krml.PrintAst.ptyp >> drop_newlines
  end

  let pp fmt src =
    Format.fprintf fmt "This item is a monomorphization of `%s`."
      (Krml.Idents.string_of_lident src.original_lid);
    let print_list header list printer =
      match list with
      | [] -> ()
      | l ->
          Format.fprintf fmt "\n -> %s:\n%s" header
            (List.map printer l
            |> List.map (fun s -> "    + " ^ s)
            |> String.concat "\n")
    in
    print_list "Const generics and traits methods" src.const_generics pexpr;
    print_list "Types" src.types ptyp
end

(** Computes a lookup function that maps monomorphized identifiers to
their monomorphization sources. Note: this function is stateful, it
reads {Krml.MonomorphizationState.generated_lids}. The map is
constructed given that state at the moment of the call. *)
let compute_monomorphization_source_lookup () :
    K.lident -> MonomorphizationSource.t option =
  let map =
    Hashtbl.fold
      (fun (original_lid, const_generics, types) lident acc ->
        LidentMap.add lident
          MonomorphizationSource.{ original_lid; const_generics; types }
          acc)
      Krml.MonomorphizationState.generated_lids LidentMap.empty
  in
  fun key -> LidentMap.find_opt key map

(** Tags a declaration with a comment. *)
let add_comment_to_decl (comment : string) : K.decl -> K.decl =
  (object
     inherit [_] K.map
     method! visit_flags _ flags = flags @ [ Comment comment ]
  end)
    #visit_decl
    ()

(** Adds comments describing monomorphizations *)
let add_monomorphization_comments (files : K.files) : K.files =
  let lookup = compute_monomorphization_source_lookup () in
  (object
     inherit [_] K.map

     method! visit_decl _ decl =
       match lookup (K.lid_of_decl decl) with
       | None -> decl
       | Some source ->
           add_comment_to_decl ([%show: MonomorphizationSource.t] source) decl
  end)
    #visit_files
    () files

(** Given a list of monorphizations (a pair of a monomorphized
identifier and a monomorphization source), finds the minimal prefix of
generics for each monomorphizations so that names are unique. *)
let minimize_names (names : (K.lident * MonomorphizationSource.t) list) =
  let _const_arities =
    List.map
      (fun (_, (src : MonomorphizationSource.t)) ->
        List.length src.const_generics)
      names
    |> List.sort_uniq Int.compare
  in
  ()

let rename (file : Krml.Ast.file) =
  let lids = lids_of_decls (snd file) in
  let lookup = compute_monomorphization_source_lookup () in
  let lid_to_olid, oname_counts =
    let list =
      List.filter_map
        (fun (lid, _) ->
          lookup lid
          |> Option.map (fun (mono : MonomorphizationSource.t) ->
                 (lid, mono.original_lid)))
        lids
    in
    let lid_to_olid = List.to_seq list |> LidentMap.of_seq in
    let olid_counts =
      List.fold_left
        (fun acc (_, olid) ->
          LidentMap.update olid
            (function Some n -> Some (n + 1) | _ -> Some 1)
            acc)
        LidentMap.empty list
    in
    (lid_to_olid, olid_counts)
  in
  let count_olid lid =
    match LidentMap.find_opt lid oname_counts with Some n -> n | _ -> 0
  in
  let lids =
    List.filter (fun (_, flags) -> List.mem Krml.Common.Private flags) lids
  in
  let lids = List.map fst lids in
  let lids = List.filter (fun lid -> count_olid lid == 1) lids in
  let renamings =
    List.map (fun lid -> (lid, LidentMap.find lid lid_to_olid)) lids
    |> List.to_seq |> LidentMap.of_seq
  in
  let result =
    (object
       inherit [_] K.map

       method! visit_lident _ lid =
         match LidentMap.find_opt lid renamings with
         | Some lid -> lid
         | _ -> lid
    end)
      #visit_file
      () file
  in
  result
