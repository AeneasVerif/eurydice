(* Utilities to record the order of declarations given by Charon, then recover
   a source-order rank for final, transformed declarations.
 *)
open Krml.Ast

(* Record the order of declarations. *)
type source_order = (lident, int) Hashtbl.t

(* Record which declaration comes from which other one. *)
type lid_origins = (lident, lident) Hashtbl.t

(* Function that assigns a sorting order to each lid. *)
type ranker = lident -> int

(* Record lids in the order that they're passed to this function. *)
let record_lid_orders (lids : lident list) : source_order =
  let source_order = Hashtbl.create 41 in
  let next = ref 0 in
  let record lid =
    if not (Hashtbl.mem source_order lid) then begin
      Hashtbl.add source_order lid !next;
      incr next
    end
  in
  List.iter record lids;
  source_order

(* Record that [to_lid] was made from [from_lid] and should be ordered like [from_lid]. *)
let add_origin (lid_origins : lid_origins) ~(from_lid : lident) ~(to_lid : lident) : unit =
  if from_lid <> to_lid && not (Hashtbl.mem lid_origins to_lid) then
    Hashtbl.add lid_origins to_lid from_lid

(* Inspect the karamel monomorphization tables to record which mono lid came from which poly lid. *)
let record_monomorphization_origins (lid_origins : lid_origins) : unit =
  Hashtbl.iter
    (fun (generic_lid, _, _) (_, monomorphized_lid) ->
      add_origin lid_origins ~from_lid:generic_lid ~to_lid:monomorphized_lid)
    Krml.MonomorphizationState.state;
  Hashtbl.iter
    (fun (generic_lid, _, _) monomorphized_lid ->
      add_origin lid_origins ~from_lid:generic_lid ~to_lid:monomorphized_lid)
    Krml.MonomorphizationState.generated_lids

(* Karamel hash-conses tag enum declarations by constructor set, so several
   datatypes can point at the same generated tag lid. Use the datatype whose
   preferred tag lid was actually selected when we can tell from the name;
   otherwise fall back to the earliest source-ranked datatype. *)
let record_datatype_tag_origins (lid_origins : lid_origins) (rank_of_lid : ranker)
    ((_, _, tag_remap) : Krml.DataTypes.map) =
  let earliest_ranked_lid lids =
    let compare_source_rank lid1 lid2 =
      match rank_of_lid lid1, rank_of_lid lid2 with
      | rank1, rank2 when rank1 <> rank2 -> compare rank1 rank2
      | _, _ -> compare lid1 lid2
    in
    match lids with
    | [] -> invalid_arg "earliest_ranked_lid"
    | lid :: lids ->
        List.fold_left
          (fun best lid ->
            if compare_source_rank lid best < 0 then
              lid
            else
              best)
          lid lids
  in
  let source_has_preferred_tag_lid tag_lid source_lid =
    tag_lid = source_lid || tag_lid = (fst source_lid, snd source_lid ^ "_tags")
  in

  let tag_sources = Hashtbl.create 41 in
  Hashtbl.iter
    (fun source_lid tag_lid ->
      let source_lids =
        match Hashtbl.find_opt tag_sources tag_lid with
        | Some source_lids -> source_lids
        | None -> []
      in
      Hashtbl.replace tag_sources tag_lid (source_lid :: source_lids))
    tag_remap;
  Hashtbl.iter
    (fun tag_lid source_lids ->
      let source_lid =
        match List.find_opt (source_has_preferred_tag_lid tag_lid) source_lids with
        | Some source_lid -> source_lid
        | None -> earliest_ranked_lid source_lids
      in
      add_origin lid_origins ~from_lid:source_lid ~to_lid:tag_lid)
    tag_sources

(* Return a function that computes the rank for a given lid. *)
let build_ranker (source_order : source_order) (lid_origins : lid_origins) : ranker =
  let rank_cache = Hashtbl.create 41 in
  let rec ranker_inner_loop seen lid =
    match Hashtbl.find_opt rank_cache lid with
    | Some rank -> rank
    | None ->
        let rank =
          match Hashtbl.find_opt source_order lid with
          | Some rank -> rank
          | None -> (
              if List.mem lid seen then
                failwith "loop in declaration origins"
              else
                match
                  Hashtbl.find_opt lid_origins lid
                with
                | Some from_lid -> ranker_inner_loop (lid :: seen) from_lid
                | None ->
                    failwith
                      (Printf.sprintf
                         "missing declaration origin for %s; add this lid to \
                          `DeclOrder.record_lid_orders` or `DeclOrder.add_origin`"
                         (Krml.Idents.string_of_lident lid)))
        in
        Hashtbl.add rank_cache lid rank;
        rank
  in
  ranker_inner_loop []

let sort_files (source_order : source_order) ~(lid_origins : lid_origins)
    ?(datatype_map : Krml.DataTypes.map option) (files : files) : files =
  let rank_of_lid = build_ranker source_order lid_origins in
  let rank_of_decl decl = rank_of_lid (lid_of_decl decl) in

  (* Record the final lid relationships we were missing. *)
  record_monomorphization_origins lid_origins;
  Option.iter (record_datatype_tag_origins lid_origins rank_of_lid) datatype_map;

  (* Stable sort by Charon rank. *)
  let sort_decls decls =
    decls
    |> List.map (fun decl -> rank_of_decl decl, decl)
    |> List.stable_sort (fun (rank1, _) (rank2, _) -> compare rank1 rank2)
    |> List.map snd
  in
  (* Sort by rank, then stable sort topologically so that definitions come after their dependencies. *)
  let files =
    files |> List.map (fun (name, decls) -> name, decls |> sort_decls |> Bundles.topological_sort)
  in
  Bundles.filter_forward files
