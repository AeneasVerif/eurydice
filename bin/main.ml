let () =
  let json = Eurydice.LoadLlbc.load_file "../charon/tests/llbc/array.llbc" in
  let _ast = Eurydice.AstOfLlbc.file_of_crate json (* materialize file name here *) in
  print_endline "done"
