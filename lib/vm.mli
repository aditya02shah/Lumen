(* --TYPE DECLARATIONS-- *)
type inst_idx = int
type exec_count = int
type inst_counter = (inst_idx, exec_count) Hashtbl.t

(* --PUBLIC INTERFACE-- *)
val run_program : Opcode.opcode list -> int -> bool -> unit
