open Krml.Ast

type type_node = lident * typ list * cg list
type fun_node = lident * expr list * typ list

type t = {
  names : (lident, Charon.Types.name) Hashtbl.t;
  types : (type_node, lident) Hashtbl.t;
  functions : (fun_node, lident) Hashtbl.t;
}

let create () =
  { names = Hashtbl.create 41; types = Hashtbl.create 41; functions = Hashtbl.create 41 }

let merge metadata =
  let merged = create () in
  List.iter
    (fun metadata ->
      Hashtbl.iter (Hashtbl.replace merged.names) metadata.names;
      Hashtbl.iter (Hashtbl.replace merged.types) metadata.types;
      Hashtbl.iter (Hashtbl.replace merged.functions) metadata.functions)
    metadata;
  merged

let add_name t lid name = Hashtbl.replace t.names lid name
let name t lid = Hashtbl.find_opt t.names lid

let has_instantiation name =
  List.exists
    (function
      | Charon.Types.PeInstantiated _ -> true
      | _ -> false)
    name

let exists f = function
  | Some x -> f x
  | None -> false

let is_instantiated t lid = exists has_instantiation (name t lid)
