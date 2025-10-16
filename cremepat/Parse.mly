%{
  open ParseTree
%}

%token<int>     INT
%token<string>  UIDENT LIDENT UVAR UVARLIST
%token          EOF COMMA EQUALS LBRACK RBRACK LBRACKHASH LANGLE RANGLE LCURLY RCURLY
%token          COLON COLONCOLON AMP LPAREN RPAREN LPARENHASH SEMI
%token          MATCH TRUE FALSE LET WHILE BREAK ARROW ABORT

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
  { if p = "" then Wild else Var p }
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

%inline
with_vars(X):
| x = UVAR
  { PatternVar (if x = "" then "_" ^ gensym () else x) }
| x = UVARLIST
  { ListPatternVar (if x = "" then "_" ^ gensym () else x) }
| x = X
  { Fixed x }

%inline
fixed(X):
| x = X
  { Fixed x }

(* Types *)

pre_typ:
| t = typ ts = delimited(LANGLE, separated_list(COMMA, typ), RANGLE)
  { TApp (t, ts) }
| p = path
  { TQualified p }

typ:
| t = with_vars(pre_typ)
  { t }

(* Patterns *)

pre_pat:
| u = uident
  { Cons (u, []) }
| u = uident p = delimited(LPAREN, separated_list(COMMA, pat), RPAREN)
  { Cons (u, p) }
| u = uident p = pat
  { Cons (u, [ p ]) }

pat:
| t = with_vars(pre_pat)
  { t }

(* Expressions *)

expr:
| e = fixed(pre_expr)
  { e }
| e = seq_expr
  { e }

pre_expr:
| LET b = lident EQUALS e1 = app_expr SEMI e2 = expr
  { Let (b, e1, e2) }

seq_expr:
| e = fixed(pre_seq_expr)
  { e }
| e = app_expr
  { e }

pre_seq_expr:
| e1 = app_expr SEMI e2 = seq_expr
  { match e2 with Fixed (Sequence e2) -> Sequence (e1 :: e2) | _ -> Sequence [ e1; e2 ] }

app_expr:
| e = fixed(pre_app_expr)
  { e }
| e = index_expr
  { e }

pre_app_expr:
| head = app_expr
  cgs = ioption(delimited(LBRACKHASH, separated_list(COMMA, expr), RBRACK))
  methods = ioption(delimited(LPARENHASH, separated_list(COMMA, expr), RPAREN))
  ts = ioption(delimited(LANGLE, separated_list(COMMA, typ), RANGLE))
  args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
  {
    let cgs = Option.value ~default:[] cgs in
    let methods = Option.value ~default:[] methods in
    let ts = Option.value ~default:[] ts in
    App { head; cgs; methods; ts; args }
  }
| AMP e = index_expr
  { Addr e }

index_expr:
| e = fixed(pre_index_expr)
  { e }
| e = atomic_expr
  { e }

pre_index_expr:
| e1 = index_expr e2 = delimited(LBRACK, expr, RBRACK)
  { Index (e1, e2) }

atomic_expr:
| e = with_vars(pre_atomic_expr)
  { e }
| e = delimited(LPAREN, expr, RPAREN)
  { e }

pre_atomic_expr:
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
| BREAK
  { Break }
| ABORT
  { Abort }
| FALSE
  { Bool false }
| TRUE
  { Bool true }

(* Entry point *)
fragment:
| e = expr EOF
  { e }

