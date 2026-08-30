(* expression *)

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
  (* terminals *)
  | INT of int
  | STRING of string
  (* non-terminals *)
  (* unary *)
  | INCREMENT of expr
  (* binary *)
  | BINARY of operator * expr * expr
