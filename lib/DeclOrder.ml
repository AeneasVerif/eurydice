(* Utilities to record the order of declarations given by Charon, so that we
   can reuse it at the end of the pipeline instead of our own (less stable)
   topological sort.
 *)
open Krml.Ast

let order = Hashtbl.create 41
let next = ref 0

let reset () =
  Hashtbl.clear order;
  next := 0

let record_lid lid =
  if not (Hashtbl.mem order lid) then begin
    Hashtbl.add order lid !next;
    incr next
  end

let record_decls decls = List.iter (fun decl -> record_lid (lid_of_decl decl)) decls
let record_file (_, decls) = record_decls decls

let inherit_order ~from_lid ~to_lid =
  match Hashtbl.find_opt order from_lid with
  | Some order_id when not (Hashtbl.mem order to_lid) ->
      Hashtbl.add order to_lid order_id;
      true
  | _ -> false

let refresh_monomorphized_lids () =
  let changed = ref true in
  while !changed do
    changed := false;
    Hashtbl.iter
      (fun (generic_lid, _, _) (_, monomorphized_lid) ->
        changed := inherit_order ~from_lid:generic_lid ~to_lid:monomorphized_lid || !changed)
      Krml.MonomorphizationState.state;
    Hashtbl.iter
      (fun (generic_lid, _, _) monomorphized_lid ->
        changed := inherit_order ~from_lid:generic_lid ~to_lid:monomorphized_lid || !changed)
      Krml.MonomorphizationState.generated_lids
  done

let order_of_decl decl = Hashtbl.find_opt order (lid_of_decl decl)

let compare_order current_pos1 decl1 current_pos2 decl2 =
  match order_of_decl decl1, order_of_decl decl2 with
  | Some order1, Some order2 when order1 <> order2 -> compare order1 order2
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some _, Some _ | None, None -> compare current_pos1 current_pos2

let sort_decls decls =
  decls
  |> List.mapi (fun i decl -> i, decl)
  |> List.stable_sort (fun (i1, decl1) (i2, decl2) -> compare_order i1 decl1 i2 decl2)
  |> List.map snd

let file_order (_, decls) =
  List.fold_left
    (fun acc decl ->
      match order_of_decl decl with
      | Some order_id -> min acc order_id
      | None -> acc)
    max_int decls

let sort_files files =
  refresh_monomorphized_lids ();
  (* Charon gives us a global declaration order, but after bundling and
     monomorphization that order is split across files and fresh lids. Sort each
     file's remaining declarations by the recorded source order, and keep
     declarations that have no source-order entry in their current relative
     order. *)
  files
  |> List.mapi (fun i (name, decls) -> i, (name, sort_decls decls))
  |> List.stable_sort (fun (i1, file1) (i2, file2) ->
         match compare (file_order file1) (file_order file2) with
         | 0 -> compare i1 i2
         | n -> n)
  |> List.map snd

let sort_files_for_c files =
  refresh_monomorphized_lids ();
  (* The final C output has stricter declaration-before-use requirements than
     Charon's source order promises, especially for fresh Karamel declarations.
     Start from Charon order, then reuse Eurydice's existing declaration-level
     DFS so only actual local dependencies move earlier. *)
  files
  |> List.mapi (fun i (name, decls) -> i, (name, decls |> sort_decls |> Bundles.topological_sort))
  |> List.stable_sort (fun (i1, file1) (i2, file2) ->
         match compare (file_order file1) (file_order file2) with
         | 0 -> compare i1 i2
         | n -> n)
  |> List.map snd
