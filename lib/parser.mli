type arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

type expr =
  | INT of int
  | STRING of string
  | INCREMENT of expr
  | BINARY of arithmetic_op * expr * expr

exception Invalid_expr

val match_expr : Lexer.token list -> expr list * Lexer.token list
val format_expr : expr -> string
