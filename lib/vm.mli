(* --TYPE DECLARATIONS-- *)
type var_offset = int
type address = int
type operand = INT of int | BOOL of bool

(* VM operations *)
type opcode =
  (* ALU *)
  | ARITHMETIC of Ast.arithmetic_op
  | BOOLEAN of Ast.logical_compare
  (* functions *)
  | CALL of address * operand list
  (* ctrl flow *)
  | JMP of address * address
  | UNCONDITIONAL_JMP of address
  | RET
  (* data *)
  | LOAD_LOCAL of var_offset
  | STORE_LOCAL of var_offset
  | PUSH_CONST of operand

val run_program : opcode list -> unit
