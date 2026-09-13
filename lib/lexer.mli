(* --TYPE DECLARATIONS-- *)
type dtype = INT | BOOL (* supported datatypes *)

(* Recognised tokens *)
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
  | IF
  | ELSE
  (* syntax *)
  | DATATYPE of dtype
  | VOID
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

(* position-aware types *)
type position = int (* line number *)
type located_str = string * position (* source strings *)
type located_token = token * position (* source tokens *)
type lexer_errors = located_token list (* source errors *)

(* --HELPER FUNCTIONS-- *)
val print_list : ('a -> string) -> 'a list -> unit
val format_token : token -> string

(* --PUBLIC INTERFACE-- *)
val tokenize : string -> located_token list * lexer_errors
