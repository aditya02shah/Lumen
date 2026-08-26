type dtype = INT | BOOL

type token =
  (* data *)
  | NUMBER of int
  | IDENTIFIER of string
  (* keywords *)
  | FOR
  | WHILE
  | RETURN
  | MUT
  (* syntax *)
  | DATATYPE of dtype
  | OPTION
  | SOME
  | NONE
  | WHITESPACE
  | SEMICOLON
  | L_PAREN
  | R_PAREN
  | L_BRACE
  | R_BRACE
  | L_BRACKET
  | R_BRACKET
  | COMMA
  (* operations *)
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | MOD
  | EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN_EQUALS
  | GREATER_THAN
  | LOGICAL_AND
  | LOGICAL_OR
  | INCREMENT
  | UNKNOWN of string (* TODO *)

exception Unknown_token

val tokenize : string -> token list
val print_list : ('a -> string) -> 'a list -> unit
val format_token : token -> string
