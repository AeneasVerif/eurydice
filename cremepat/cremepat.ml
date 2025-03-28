module Terminal = struct
  let mkcolor x = Printf.sprintf "\x1b[38;5;%dm" x

  let green = mkcolor 119
  let red = mkcolor 203
  let blue = mkcolor 81
  let yellow = mkcolor 227
  let orange = mkcolor 202
  let underline = "\x1b[4m"
  let reset = "\x1b[0m"  
end

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
          Buffer.add_string buf Terminal.red;
          for _j = 0 to start do
            Buffer.add_char buf ' '
          done;
          for _j = start to end_ - 1 do
            Buffer.add_char buf '^'
          done;
          Buffer.add_string buf Terminal.reset;
          Buffer.add_char buf '\n'
        end
      ) (String.split_on_char '\n' arg);
      Printf.eprintf "Parse error, line %d, characters %d-%d: %s\n" !Lex.lines start end_ (Buffer.contents buf);
      raise e

open Ppxlib

type env = string list
let empty = []
let push env x = x :: env
let find env x = List.find x env

let lident ~loc x =
  { txt = Lident x; loc }

let ppat_node ~loc pat =
  let open Ast_builder.Default in
  ppat_record ~loc [
    lident ~loc "node", pat
  ] Open

let rec ppat_list ~loc pats =
  let open Ast_builder.Default in
  List.fold_right (fun pat acc ->
    ppat_construct ~loc (lident ~loc "::") (Some (ppat_tuple ~loc [
      pat;
      acc
    ]))
  ) pats (ppat_construct ~loc (lident ~loc "[]") None)

let ppat_cons_many ~loc cons args =
  let open Ast_builder.Default in
  ppat_node ~loc (ppat_construct ~loc (lident ~loc cons) (Some (
    ppat_tuple ~loc args)))

let ppat_cons_one ~loc cons arg =
  let open Ast_builder.Default in
  ppat_node ~loc (ppat_construct ~loc (lident ~loc cons) (Some arg))

let compile_parse_tree (env: env) loc (pt: ParseTree.expr) (* : Astlib.Ast_503.Parsetree.pattern *) =
  let open Ast_builder.Default in
  let rec compile env pt =
    match pt with
    | ParseTree.Let (b, e1, e2) ->
        let p1 = compile env e1 in
        let env = push env b in 
        let p2 = compile env e2 in
        (* ELet (_, p1, p2) *)
        ppat_cons_many ~loc "ELet" [
          ppat_any ~loc;
          p1;
          p2
        ]

    | Sequence ps ->
        ppat_cons_one ~loc "ESequence" (
          ppat_list ~loc (List.map (compile env) ps))

    | App (_, _, _) -> failwith "todo!!"
    | Addr _ -> failwith "todo!!"
    | Index (_, _) -> failwith "todo!!"
    | While (_, _) -> failwith "todo!!"
    | Match (_, _) -> failwith "todo!!"
    | Record _ -> failwith "todo!!"
    | Int _ -> failwith "todo!!"
    | Qualified _ -> failwith "todo!!"
    | BoundVar _ -> failwith "todo!!"
    | PatternVar _ -> failwith "todo!!"
    | Break -> failwith "todo!!"
    | Bool _ -> failwith "todo!!"
  in
  compile env pt

let expand ~ctxt (payload: string) =
  let pt = parse payload in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  compile_parse_tree empty loc pt

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
