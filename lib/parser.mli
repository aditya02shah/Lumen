(* --EXCEPTIONS-- *)
exception Invalid_expr

(* --TYPE DECLARATIONS-- *)
type parser_errors = string list

(* --PARSER IMPLEMENTATION-- *)
val match_expr :
  Lexer.located_token list ->
  Ast.expr option * Lexer.located_token list * parser_errors

val format_expr : Ast.expr -> string

val match_cond_expr :
  Lexer.located_token list ->
  Ast.cond_expr option * Lexer.located_token list * parser_errors

val match_var_decl :
  Lexer.located_token list ->
  Ast.var_decl option * Lexer.located_token list * parser_errors

val match_asst :
  Lexer.located_token list ->
  bool ->
  Ast.asst option * Lexer.located_token list * parser_errors

val format_asst : Ast.asst option -> string

val match_arg_list :
  Lexer.located_token list ->
  Ast.position ->
  Ast.arg_list option * Lexer.located_token list * parser_errors

val match_param_list :
  Lexer.located_token list ->
  Ast.param_list * Lexer.located_token list * parser_errors

val match_func_call :
  Lexer.located_token list ->
  Ast.position ->
  Ast.func_call option * Lexer.located_token list * parser_errors

val match_loop :
  Lexer.located_token list ->
  Ast.loop option * Lexer.located_token list * parser_errors

val match_block :
  Lexer.located_token list ->
  Ast.block option * Lexer.located_token list * parser_errors

val match_stmt :
  Lexer.located_token list ->
  Ast.stmt list * Lexer.located_token list * parser_errors

val match_func_decl :
  Lexer.located_token list ->
  Ast.func_decl option * Lexer.located_token list * parser_errors

(* --PUBLIC INTERFACE-- *)
val parse_prog :
  Lexer.located_token list ->
  Ast.func_decl list * Lexer.located_token list * parser_errors
