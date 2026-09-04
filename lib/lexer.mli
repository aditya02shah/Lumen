type dtype = INT | BOOL (* supported datatypes *)

type token =
  (* data *)
  | NUMBER of int
  | BOOLEAN of bool
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
  | ASSIGNMENT
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
  | POW
  | INCREMENT
  | EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN_EQUALS
  | GREATER_THAN
  | LOGICAL_AND
  | LOGICAL_OR
  | NOT
  | UNKNOWN of string (* unrecognized tokens *)

val tokenize : string -> token list * token list
val print_list : ('a -> string) -> 'a list -> unit
val format_token : token -> string
