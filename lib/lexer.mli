(* --TYPE DECLARATIONS-- *)
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
  | DATATYPE of Ast.dtype
  | VOID
  | OPTION
  | SOME
  | NONE
  | LET
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

type partial_token =
  (* helps handle tokens spanning multiple characters *)
  | DIGIT
  | CHAR
  | NEGATIVE_DIGIT
  | WS

type accum =
  (* type representing accumlated tokens *)
  | Accum of char list * partial_token
  | Empty

(* Position-aware types *)
type located_str = string * Ast.position (* source strings *)
type located_token = token * Ast.position (* source tokens *)
type lexer_errors = located_token list (* source errors *)

val format_token : token -> string

(* --PUBLIC INTERFACE-- *)
val tokenize : string -> located_token list * lexer_errors
