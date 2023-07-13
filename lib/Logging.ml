module StringSet = Set.Make(String)

type logging =
  | None
  | All
  | Some of StringSet.t

let logging = ref None

let enable_logging (modules: string) =
  let modules = String.split_on_char ',' modules in
  if modules = [ "*" ] then
    logging := All
  else
    logging := Some (StringSet.of_list modules)

let has_logging m =
  match !logging with
  | None -> false
  | All -> true
  | Some s -> StringSet.mem m s

let dummy = Buffer.create 1

let log (type a) (m: string) (fmt: (a, Buffer.t, unit, unit) format4) =
  if has_logging m then
    Krml.KPrint.bfprintf stderr (fmt ^^ "\n")
  else
    Printf.ibprintf dummy fmt
