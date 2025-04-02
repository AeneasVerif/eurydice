open Sedlexing
open Parse

let digit = [%sedlex.regexp? '0' .. '9']
let integer = [%sedlex.regexp? Plus digit]
let low_alpha = [%sedlex.regexp? 'a' .. 'z']
let up_alpha = [%sedlex.regexp? 'A' .. 'Z']
let anyident = [%sedlex.regexp? up_alpha | low_alpha | '_' | '-' | digit]
let lident = [%sedlex.regexp? low_alpha, Star anyident]
let uident = [%sedlex.regexp? up_alpha, Star anyident]
let uvar = [%sedlex.regexp? '?', Star anyident]
let uvarlist = [%sedlex.regexp? '?', Star anyident, '.', '.']
let locate _ tok = tok, Lexing.dummy_pos, Lexing.dummy_pos

let keywords =
  [ "match", MATCH; "true", TRUE; "break", BREAK; "false", FALSE; "while", WHILE; "let", LET ]

let lines = ref 1
let cur_line = ref 0

let rec token lexbuf =
  match%sedlex lexbuf with
  | integer ->
      let l = Utf8.lexeme lexbuf in
      locate lexbuf (INT (int_of_string l))
  | uident ->
      let l = Utf8.lexeme lexbuf in
      locate lexbuf (UIDENT l)
  | lident ->
      let l = Utf8.lexeme lexbuf in
      begin
        try locate lexbuf (List.assoc l keywords) with Not_found -> locate lexbuf (LIDENT l)
      end
  | uvar ->
      let l = Utf8.lexeme lexbuf in
      let l = String.sub l 1 (String.length l - 1) in
      locate lexbuf (UVAR l)
  | uvarlist ->
      let l = Utf8.lexeme lexbuf in
      let l = String.sub l 1 (String.length l - 3) in
      locate lexbuf (UVARLIST l)
  | "&" -> locate lexbuf AMP
  | ";" -> locate lexbuf SEMI
  | "->" -> locate lexbuf ARROW
  | "," -> locate lexbuf COMMA
  | "=" -> locate lexbuf EQUALS
  | "[" -> locate lexbuf LBRACK
  | "]" -> locate lexbuf RBRACK
  | "<" -> locate lexbuf LANGLE
  | ">" -> locate lexbuf RANGLE
  | "{" -> locate lexbuf LCURLY
  | "}" -> locate lexbuf RCURLY
  | "(" -> locate lexbuf LPAREN
  | ")" -> locate lexbuf RPAREN
  (* | "_" -> locate lexbuf UNDERSCORE *)
  | "::" -> locate lexbuf COLONCOLON
  | ":" -> locate lexbuf COLON
  | "\n" ->
      incr lines;
      cur_line := fst (loc lexbuf);
      token lexbuf
  | eof -> locate lexbuf EOF
  | white_space -> token lexbuf
  | any ->
      let l = Utf8.lexeme lexbuf in
      failwith (Printf.sprintf "unhandled token: %s, len=%d" l (String.length l))
  | _ -> assert false
