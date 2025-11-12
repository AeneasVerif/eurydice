open Krml.Ast
module Builtin_ = Builtin
open Krml

let gen_types () =
  Monomorphization.NameGen.short_names := true;
  let map = Monomorphization.build_def_map [ "", AstOfLlbc.extra_decls ] in
  let monomorphizer = Monomorphization.monomorphize_data_types map in
  List.iter
    (fun node -> ignore (monomorphizer#visit_node false node))
    [
      Builtin_.arr, [ TInt UInt8 ], [ CgConst (SizeT, "2") ];
      Builtin_.arr, [ TInt UInt8 ], [ CgConst (SizeT, "4") ];
      Builtin_.arr, [ TInt UInt8 ], [ CgConst (SizeT, "8") ];
      Builtin_.dst_ref_shared, [ TInt UInt8; TInt SizeT ], [];
      Builtin_.dst_ref_shared, [ TInt Int16; TInt SizeT ], [];
      Builtin_.dst_ref_mut, [ TInt UInt8; TInt SizeT ], [];
      Builtin_.dst_ref_mut, [ TInt Int16; TInt SizeT ], [];
    ];
  let files = [ "types", monomorphizer#clear () ] in
  let scope_env = Simplify.allocate_c_env files in
  let c_name_map = GlobalNames.mapping (fst scope_env) in
  let file_of_map = Bundle.mk_file_of files in
  let deps = Bundles.direct_dependencies_with_internal files file_of_map in
  let files = AstToCStar.mk_files files c_name_map Idents.LidSet.empty Idents.LidSet.empty in
  let headers = CStarToC11.mk_headers c_name_map files in
  let public_headers =
    Bundles.StringSet.of_list
      (List.filter_map
         (function
           | name, C11.Public _ -> Some name
           | _ -> None)
         headers)
  in
  ignore (Output.write_h headers public_headers deps)
