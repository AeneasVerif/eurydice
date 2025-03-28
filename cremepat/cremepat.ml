let mkcolor x = Printf.sprintf "\x1b[38;5;%dm" x

let green = mkcolor 119
let red = mkcolor 203
let blue = mkcolor 81
let yellow = mkcolor 227
let orange = mkcolor 202
let underline = "\x1b[4m"
let reset = "\x1b[0m"  

let parse arg =
  let the_parser = MenhirLib.Convert.Simplified.traditional2revised Parse.fragment in
  let lexbuf = Sedlexing.Utf8.from_string arg in
  try
    the_parser (fun _ -> Lex.token lexbuf)
  with
  | Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ as e ->
      Printf.eprintf "Lexing error in: %s\n" arg;
      raise e
  | Parse.Error as e ->
      let start, end_ = Sedlexing.loc lexbuf in
      let start = start - !Lex.cur_line in
      let end_ = end_ - !Lex.cur_line in
      let buf = Buffer.create 256 in
      List.iteri (fun i line ->
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
        if i + 1 = !Lex.lines then begin
          Buffer.add_string buf red;
          for _j = 0 to start do
            Buffer.add_char buf ' '
          done;
          for _j = start to end_ - 1 do
            Buffer.add_char buf '^'
          done;
          Buffer.add_string buf reset;
          Buffer.add_char buf '\n'
        end
      ) (String.split_on_char '\n' arg);
      Printf.eprintf "Parse error, line %d, characters %d-%d: %s\n" !Lex.lines start end_ (Buffer.contents buf);
      raise e

open Ppxlib

let expand ~ctxt (payload: string) =
  ignore ctxt;
  let pt = parse payload in
  ignore pt;
  failwith "todo"

let my_extension =
  Extension.V3.declare
    "cremepat"
    Pattern
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension

let () =
  Driver.register_transformation
    ~rules:[rule]
    "cremepat"
