module K = Krml.Ast

module LidentMap = Map.Make (struct
  type t = K.lident

  let compare = compare
end)

let lids_of_decls (decls : K.program) : (K.lident * Krml.Common.flag list) list
    =
  List.map (fun d -> (K.lid_of_decl d, K.flags_of_decl d)) decls

open struct
  let ( >> ) f g x = g (f x)
end

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

  let to_id (src : t) : string =
    let obj =
      Krml.Monomorphization.monomorphize_data_types (Hashtbl.create 0)
    in
    let { types = ts; const_generics = cgs; _ } = src in
    let doc =
      PPrint.(
        separate_map underscore Krml.PrintAst.print_typ (List.map obj#pretty ts)
        ^^
        if cgs = [] then empty
        else underscore ^^ separate_map underscore Krml.PrintAst.print_expr cgs)
    in
    Krml.KPrint.bsprintf "%s__%a" (snd src.original_lid) Krml.PrintCommon.pdoc
      doc

  let to_lid (src : t) : K.lident = (fst src.original_lid, to_id src)
end

(** Computes a lookup function that maps monomorphized identifiers to
their monomorphization sources. Note: this function is stateful, it
reads {Krml.MonomorphizationState.generated_lids}. The map is
constructed given that state at the moment of the call. *)
let compute_monomorphization_source_lookup () :
    MonomorphizationSource.t LidentMap.t =
  Hashtbl.fold
    (fun (original_lid, const_generics, types) lident acc ->
      LidentMap.add lident
        MonomorphizationSource.{ original_lid; const_generics; types }
        acc)
    Krml.MonomorphizationState.generated_lids LidentMap.empty

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
  let mono_map = compute_monomorphization_source_lookup () in
  (object
     inherit [_] K.map

     method! visit_decl _ decl =
       match LidentMap.find_opt (K.lid_of_decl decl) mono_map with
       | None -> decl
       | Some source ->
           add_comment_to_decl ([%show: MonomorphizationSource.t] source) decl
  end)
    #visit_files
    () files

module List = struct
  include List

  let hd_opt : 'a. 'a t -> 'a option = function hd :: _ -> Some hd | _ -> None
  let snoc = function hd :: tl -> Some (hd, tl) | _ -> None

  let rec transpose : 'a. 'a option t -> 'a t option = function
    | Some hd :: tl -> (
        match transpose tl with Some tl -> Some (hd :: tl) | None -> None)
    | None :: _ -> None
    | [] -> Some []

  (** Finds the longest prefix in a list of lists *)
  let common_prefix ~(eq : 'a -> 'a -> bool) (lists : 'a list list) =
    let rec aux lists =
      match List.map snoc lists |> transpose with
      | None -> []
      | Some ((first_hd, first_tl) :: rest)
        when List.for_all (fun (hd, _) -> eq first_hd hd) rest ->
          first_hd :: aux (first_tl :: List.map snd rest)
      | _ -> []
    in
    aux lists

  let rec drop n list =
    if n <= 0 then list
    else match list with _ :: tl -> drop (n - 1) tl | _ -> []

  let rec take n list =
    if n <= 0 then []
    else match list with hd :: tl -> hd :: take (n - 1) tl | [] -> []

  let strip_common_prefix ~(eq : 'a -> 'a -> bool) (lists : 'a list list) :
      'a list list =
    let prefix_len = common_prefix ~eq lists |> length in
    map (drop prefix_len) lists

  let rec zip_exn l r =
    match (l, r) with
    | [], [] -> []
    | lhd :: ltl, rhd :: rtl -> (lhd, rhd) :: zip_exn ltl rtl
    | _ -> failwith "zip: left and right are not of the same size"

  let group_by' (f : 'a -> 'g) (l : 'a list) : ('g * 'a list) list =
    let ht = Hashtbl.create 40 in
    List.iter
      (fun x ->
        let repr = f x in
        let group = Hashtbl.find_opt ht repr |> Option.value ~default:[] in
        Hashtbl.replace ht repr (x :: group))
      l;
    Hashtbl.to_seq ht |> List.of_seq
    |> List.map (fun (repr, l) -> (repr, List.rev l))

  let group_by (projection : 'a -> 'g) (l : 'a list) : 'a list list =
    group_by' projection l |> List.map snd

  let unzip (l : ('a * 'b) list) : 'a list * 'b list =
    (List.map fst l, List.map snd l)

  let rec split_at n acc (list : 'a list) : 'a list * 'a list =
    if n <= 0 then (List.rev acc, list)
    else
      match list with
      | hd :: tl -> split_at (n - 1) (hd :: acc) tl
      | [] -> (List.rev acc, [])
end

(** Check wether two arrays share a same prefix of size `len` *)
let share_common_prefix (len : int) (a : 'a array) (b : 'a array) : bool =
  if len > Array.length a || len > Array.length b then false
  else
    let exception NotEqual in
    try
      for i = 0 to len - 1 do
        if not (Array.get a i = Array.get b i) then raise NotEqual
      done;
      true
    with NotEqual -> false

(** Check wether n arrays have unique prefix of size `len` *)
let unique_prefix (len : int) (arrays : 'a array array) : bool =
  let n = Array.length arrays in
  let exception NotUnique in
  try
    for j = 0 to n - 1 do
      let a = Array.get arrays j in
      for k = j + 1 to n - 1 do
        let b = Array.get arrays k in
        if share_common_prefix len a b then raise NotUnique
      done
    done;
    true
  with NotUnique -> false

let smallest_unique_prefix_len (arrays : 'a array array) : int option =
  if Array.length arrays = 0 then Some 0
  else
    let max = Array.get arrays 0 |> Array.length in
    let exception Found of int in
    try
      for len = 1 to max do
        if unique_prefix len arrays then raise (Found len)
      done;
      None
    with Found len -> Some len

let smallest_unique_prefixes (l : 'a list list) : 'a list list =
  match Array.(List.map of_list l |> of_list) |> smallest_unique_prefix_len with
  | None -> l
  | Some len -> List.map (List.take len) l

(** Given a list of monorphizations (a pair of a monomorphized
identifier and a monomorphization source), generates a replacement map
by stripping type or cg prefixes *)
let minimize_names_strip_prefixes
    (names : (K.lident * MonomorphizationSource.t) list) : K.lident LidentMap.t
    =
  let reduced_types =
    smallest_unique_prefixes
      (List.map (fun (_, src) -> src.MonomorphizationSource.types) names)
    |> List.(map hd_opt >> transpose)
  in
  (match reduced_types with
  | Some l ->
      List.zip_exn names l
      |> List.map (fun ((name, src), typ) ->
             ( name,
               MonomorphizationSource.
                 { src with types = [ typ ]; const_generics = [] } ))
  | None -> (
      let reduced_cgs =
        smallest_unique_prefixes
          (List.map
             (fun (_, src) -> src.MonomorphizationSource.const_generics)
             names)
        |> List.(map hd_opt >> transpose)
      in
      match reduced_cgs with
      | Some l ->
          List.zip_exn names l
          |> List.map (fun ((name, src), cg) ->
                 ( name,
                   MonomorphizationSource.
                     { src with const_generics = [ cg ]; types = [] } ))
      | None -> names))
  |> List.map (fun (name, src) -> (name, MonomorphizationSource.to_lid src))
     (* (name, (fst name, MonomorphizationSource.to_id src))) *)
  |> List.to_seq |> LidentMap.of_seq

let rename_visitor map =
  object
    inherit [_] K.map

    method! visit_lident _ lid =
      LidentMap.find_opt lid map |> Option.value ~default:lid
  end

let minimize_names (files : Krml.Ast.file list) : Krml.Ast.file list =
  let mono_map = compute_monomorphization_source_lookup () in
  let visitor =
    let map =
      LidentMap.bindings mono_map
      |> List.group_by (fun (_, src) -> src.MonomorphizationSource.original_lid)
      |> List.map minimize_names_strip_prefixes
      |> List.fold_left (LidentMap.union (fun _ x _ -> Some x)) LidentMap.empty
    in
    prerr_endline "------------";
    prerr_endline ([%show: (K.lident * K.lident) list] (LidentMap.bindings map));
    prerr_endline "------------";
    (* let map = LidentMap.empty in *)
    rename_visitor map
  in
  visitor#visit_files () files

let rename (file : Krml.Ast.file) =
  let lids = lids_of_decls (snd file) in
  let mono_map = compute_monomorphization_source_lookup () in
  let lid_to_olid, oname_counts =
    let list =
      List.filter_map
        (fun (lid, _) ->
          LidentMap.find_opt lid mono_map
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
  let result = (rename_visitor renamings)#visit_file () file in
  result
