(* Strictly a parse tree *)
type expr =
  (* Binding most loosely *)
  | Let of string * expr * expr
  | Sequence of expr list
  | App of expr * typ list * expr list
  | Addr of expr
  | Index of expr * expr
  (* Atomic -- we terminate matches and loops using braces, we are not barbarians. *)
  | While of expr * expr
  | Match of expr * branch list
  | Record of (string * expr) list
  | Int of int
  | Qualified of path
  | BoundVar of string
  | PatternVar of string
  | ListPatternVar of string
  | Break
  | Bool of bool

and path =
  path_item list

and path_item =
  | Name of string
  | Wild

and branch =
  pat * expr

and pat =
  | Cons of string * pat list
  | Wild

and typ =
  | TQualified of path
  | TPatternVar of string
  | TListPatternVar of string
  | TApp of typ * typ list

let gensym =
  let r = ref 0 in
  fun () ->
    incr r;
    "$x" ^ string_of_int !r
