(* --TYPE DECLARATIONS-- *)
type var_offset = int
type address = int
type relative_address = int
type operand = INT of int | BOOL of bool

(* VM operations *)
type opcode =
  (* ALU *)
  | ARITHMETIC of Ast.arithmetic_op
  | BOOLEAN of Ast.logical_compare
  | LOGICAL_COMBINATION of Ast.logical_combination
  (* functions *)
  | CALL of address * opcode list list
  (* ctrl flow *)
  | JMP of relative_address * relative_address (* boolean comparisons *)
  | UNCONDITIONAL_JMP of relative_address
  | RET
  | RET_VOID
  (* optional types *)
  | JMP_OPT of relative_address * relative_address (* optional types *)
  | SET_OPT_FLAG of bool
  (* data *)
  | LOAD_LOCAL of var_offset
  | STORE_LOCAL of var_offset
  | PUSH_CONST of operand

(* --GLOBAL VARS-- *)
val line_counter : (int, int) Hashtbl.t

(* --PUBLIC INTERFACE-- *)
val run_program : opcode list -> int -> unit
