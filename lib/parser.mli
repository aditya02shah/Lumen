type operator =
  | ADD
  | MULTIPLY
  | SUBTRACT
  | DIVIDE
  | MOD
  | POW
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN_EQUALS
  | GREATER_THAN
  | LOGICAL_AND
  | LOGICAL_OR

type expr =
  | INT of int
  | STRING of string
  | INCREMENT of expr
  | BINARY of operator * expr * expr

exception Invalid_expr

val match_expr : Lexer.token list -> expr list * Lexer.token list
val format_expr : expr -> string
