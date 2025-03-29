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

(* Environments for the compile-time compilation of parse trees to OCaml pattern ASTs *)

type env = string list
let empty = []
let push env x = x :: env
let find env x =
  let exception Found of int in
  try
    List.iteri (fun i x' ->
      if x = x' then
        raise (Found i)
    ) env;
    raise Not_found
  with
    Found i -> i

(* Helpers to build nodes *)

let lident ~loc x =
  { txt = Lident x; loc }

let rec ppat_list ~loc pats =
  let open Ast_builder.Default in
  List.fold_right (fun pat acc ->
    ppat_construct ~loc (lident ~loc "::") (Some (ppat_tuple ~loc [
      pat;
      acc
    ]))
  ) pats (ppat_construct ~loc (lident ~loc "[]") None)

(* Variants for things that are not expressions (i.e. not 'a nodes) *)

let ppat_cons_many' ~loc cons args =
  let open Ast_builder.Default in
  ppat_construct ~loc (lident ~loc cons) (Some (
    ppat_tuple ~loc args))

let ppat_cons_one' ~loc cons arg =
  let open Ast_builder.Default in
  ppat_construct ~loc (lident ~loc cons) (Some arg)

let ppat_cons_zero' ~loc cons =
  let open Ast_builder.Default in
  ppat_construct ~loc (lident ~loc cons) None

let ppat_string ~loc s =
  let open Ast_builder.Default in
  ppat_constant ~loc (Pconst_string (s, loc, None))

let ppat_int ~loc s =
  let open Ast_builder.Default in
  ppat_constant ~loc (Pconst_integer (string_of_int s, None))

let ppat_bool ~loc s =
  ppat_cons_zero' ~loc (string_of_bool s)

(* Variants that produce the { node = ...; _ } part automatically *)

let ppat_node ~loc pat =
  let open Ast_builder.Default in
  ppat_record ~loc [
    lident ~loc "node", pat
  ] Open

let ppat_cons_many ~loc cons args =
  ppat_node ~loc (ppat_cons_many' ~loc cons args)

let ppat_cons_one ~loc cons arg =
  ppat_node ~loc (ppat_cons_one' ~loc cons arg)

let ppat_cons_zero ~loc cons =
  ppat_node ~loc (ppat_cons_zero' ~loc cons)

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

    | App (e, ts, es) ->
        (* EApp (ETApp (e, ts), es) *)
        ppat_cons_many ~loc "EApp" [
          ppat_cons_many ~loc "ETApp" [
            compile env e;
            ppat_any ~loc; (* no syntax to match on const-generics *)
            ppat_any ~loc; (* no syntax to match on method-generics *)
            compile_list_pattern (compile_typ env) ts;
          ];
          ppat_list ~loc (List.map (compile env) es)
        ]

    | Addr e ->
        ppat_cons_one ~loc "EAddrOf" (compile env e)

    | Index (e1, e2) ->
        let p1 = compile env e1 in
        let p2 = compile env e2 in
        ppat_cons_many ~loc "EBufRead" [ p1; p2 ]

    | While (e1, e2) ->
        let p1 = compile env e1 in
        let p2 = compile env e2 in
        ppat_cons_many ~loc "EWhile" [ p1; p2 ]

    | Match (e, bs) ->
        let e = compile env e in
        ppat_cons_many ~loc "EMatch" [
          ppat_any ~loc; (* no syntax to match on match flavor *)
          e;
          ppat_list ~loc (List.map (fun (p, e) ->
            let p = compile_pat env p in
            let e = compile env e in
            ppat_tuple ~loc [
              ppat_any ~loc; (* no syntax to match on binders in patterns *)
              p;
              e
            ]
          ) bs)
        ]

    | Record es ->
        ppat_cons_one ~loc "EFlat" (
          ppat_list ~loc (List.map (fun (f, e) ->
            ppat_tuple ~loc [
              ppat_cons_one' ~loc "Some" (ppat_string ~loc f);
              compile env e
            ]) es))

    | Int i ->
        ppat_cons_many ~loc "EConstant" [
          ppat_any ~loc; (* no syntax to match on width of constants *)
          ppat_string ~loc (string_of_int i)
        ]

    | Qualified path ->
        ppat_cons_one ~loc "EQualified" (compile_path env path)

    | BoundVar s ->
        let i = find env s in
        ppat_cons_one ~loc "EBound" (ppat_int ~loc i)

    | PatternVar s ->
        ppat_var ~loc { txt = s; loc }

    | Break ->
        ppat_cons_zero ~loc "EBreak"

    | Bool b ->
        ppat_cons_one ~loc "EBool" (ppat_bool ~loc b)

  and compile_path env (pt: ParseTree.path) =
    let m, n = match List.rev pt with n :: m -> List.rev m, n | _ -> failwith "impossible" in
    ppat_tuple ~loc [
      ppat_list ~loc (List.map (compile_path_item env) m);
      compile_path_item env n
    ]

  and compile_path_item _env (pt: ParseTree.path_item) =
    match pt with
    | Wild -> ppat_any ~loc
    | Name s -> ppat_string ~loc s

  and compile_list_pattern: 'a. ('a -> _) -> 'a ParseTree.list_pattern -> _ =
    fun f pt ->
      match pt with
      | Wild ->
          ppat_any ~loc
      | List pts ->
          ppat_list ~loc (List.map f pts)

  and compile_typ env (pt: ParseTree.typ) =
    match pt with
    | TQualified path ->
        ppat_cons_one' ~loc "TQualified" (compile_path env path)

    | TPatternVar s ->
        ppat_var ~loc { txt = s; loc }

    | TApp (TQualified t, ts) ->
        ppat_cons_many' ~loc "TApp" [
          compile_path env t;
          compile_list_pattern (compile_typ env) ts
        ]

    | TApp (TPatternVar s, ts) ->
        ppat_cons_many' ~loc "TApp" [
          ppat_var ~loc { txt = s; loc };
          compile_list_pattern (compile_typ env) ts
        ]

    | TApp (TApp _, _) ->
        failwith "cannot nest type applications on the left"

  and compile_pat env (pt: ParseTree.pat) =
    match pt with
    | Cons (cons, ps) ->
        ppat_cons_many ~loc "PCons" [
          ppat_string ~loc cons;
          compile_list_pattern (compile_pat env) ps
        ]
    | Wild ->
        ppat_cons_zero ~loc "PWild"

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
