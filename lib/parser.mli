exception Invalid_expr

type arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

type expr =
  | INT of int
  | STRING of string
  | INCREMENT of expr
  | BINARY of arithmetic_op * expr * expr

type ident = IDENT of string
type value = IDENTIFIER of ident | NUMBER of int | BOOLEAN of bool
type asst = { name : ident; expr : expr }
type varDecl = { dtype : Lexer.dtype; vars : ident list }
type argList = value list
type paramList = (Lexer.dtype * ident) list
type funcCall = { funcName : ident; args : argList }

val match_expr : Lexer.token list -> expr list * Lexer.token list
val format_expr : expr -> string
val match_asst : Lexer.token list -> asst option * Lexer.token list
val format_asst : asst option -> string
val match_vardecl : Lexer.token list -> varDecl option * Lexer.token list
val match_arglist : Lexer.token list -> argList option * Lexer.token list
val match_funcall : Lexer.token list -> funcCall option * Lexer.token list
val match_paramList : Lexer.token list -> paramList option * Lexer.token list
