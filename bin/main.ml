let () =
  let filename = "../charon/tests/llbc/array.llbc" in
  Eurydice.Logging.enable_logging "*";
  let llbc = Eurydice.LoadLlbc.load_file filename in
  let _ast = Eurydice.AstOfLlbc.file_of_crate llbc in
  print_endline "done"
