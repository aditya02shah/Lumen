exception Invalid_expr

type arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

type expr =
  | INT of int
  | STRING of string
  | INCREMENT of expr
  | BINARY of arithmetic_op * expr * expr

type varName = IDENT of string
type asst = { name : varName; expr : expr }
type varDecl = { dtype : Lexer.dtype; vars : varName list }

val match_expr : Lexer.token list -> expr list * Lexer.token list
val format_expr : expr -> string
val match_asst : Lexer.token list -> asst option * Lexer.token list
val format_asst : asst option -> string
val match_vardecl : Lexer.token list -> varDecl option * Lexer.token list
