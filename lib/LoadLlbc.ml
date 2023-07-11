let load_file filename =
  let ic = Yojson.Basic.from_file filename in
  Charon.LlbcOfJson.crate_of_json ic
