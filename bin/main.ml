let () =
  let json = Eurydice.LoadLlbc.load_file "../charon/tests/llbc/array.llbc" in
  match json with
  | Ok _ ->
      print_endline "ok"
  | Error e ->
      print_endline e;
      exit 1
