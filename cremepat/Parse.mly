%{
  open ParseTree
%}

%token<int>     INT
%token<string>  UIDENT LIDENT UVAR UVARLIST
%token          EOF COMMA EQUALS LBRACK RBRACK LANGLE RANGLE LCURLY RCURLY
%token          COLON COLONCOLON AMP LPAREN RPAREN SEMI
%token          MATCH TRUE FALSE LET WHILE BREAK ARROW

%type <expr> expr
%type <path_item> path_item
%type <pat> pat
%type <typ> typ
%start <expr> fragment

%%

(* Identifiers *)

%inline
uident:
| u = UIDENT
  { u }

%inline
lident:
| l = LIDENT
  { l }

%inline
ident:
| s = LIDENT
  { s }
| s = UIDENT
  { s }

(* Paths *)

path_item:
| i = ident
  { Name i }
| p = UVAR
  { assert (p = ""); Wild }
| _p = UVARLIST
  { failwith "TODO" }

%inline
path:
| p = iseparated_twoplus_list(COLONCOLON, path_item)
  { p }

(* Helpers *)
%inline iseparated_twoplus_list(separator, X):
  x1 = X; separator; x2 = X
    { [ x1; x2 ] }
| x1 = X; separator; x2 = X; separator; xs = separated_nonempty_list(separator, X)
    { x1 :: x2 :: xs }

(* Types *)

typ:
| t = typ ts = delimited(LANGLE, separated_list(COMMA, typ), RANGLE)
  { TApp (t, ts) }
| x = UVAR
  { TPatternVar (if x = "" then "_" ^ gensym () else x) }
| x = UVARLIST
  { TListPatternVar (if x = "" then "_" ^ gensym () else x) }
| p = path
  { TQualified p }

(* Patterns *)

pat:
| u = uident
  { Cons (u, []) }
| u = uident p = delimited(LPAREN, separated_list(COMMA, pat), RPAREN)
  { Cons (u, p) }

(* Expressions *)

expr:
| LET b = lident EQUALS e1 = app_expr SEMI e2 = expr
  { Let (b, e1, e2) }
| e = seq_expr
  { e }

seq_expr:
| e1 = app_expr SEMI e2 = seq_expr
  { match e2 with Sequence es -> Sequence (e1 :: es) | _ -> Sequence [ e1; e2 ] }
| e = app_expr
  { e }

app_expr:
| e = app_expr ts = ioption(delimited(LANGLE, separated_list(COMMA, typ), RANGLE)) es = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
  { App (e, Option.value ~default:[] ts, es) }
| AMP e = index_expr
  { Addr e }
| e = index_expr
  { e }

index_expr:
| e1 = index_expr e2 = delimited(LBRACK, expr, RBRACK)
  { Index (e1, e2) }
| e = atomic_expr
  { e }

atomic_expr:
| WHILE e1 = index_expr e2 = delimited(LCURLY, expr, RCURLY)
  { While (e1, e2) }
| MATCH e = index_expr bs = delimited(LCURLY, separated_list(COMMA, separated_pair(pat, ARROW, expr)), RCURLY)
  { Match (e, bs) }
| e = delimited(LCURLY, separated_nonempty_list(COMMA, separated_pair(lident, COLON, expr)), RCURLY)
  { Record e }
| i = INT 
  { Int i }
| p = path
  { Qualified p }
| x = lident
  { BoundVar x }
| x = UVAR
  { PatternVar (if x = "" then "_" ^ gensym () else x) }
| x = UVARLIST
  { ListPatternVar (if x = "" then "_" ^ gensym () else x) }
| BREAK
  { Break }
| FALSE
  { Bool false }
| TRUE
  { Bool true }
| e = delimited(LPAREN, expr, RPAREN)
  { e }

(* Entry point *)
fragment:
| e = expr EOF
  { e }
