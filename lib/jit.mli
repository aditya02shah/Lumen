(* --EXCEPTIONS-- *)
exception Jit_error

(* --TYPE DECLARATIONS-- *)
type basic_block_position = { blk_start : int; blk_end : int }

(* --JIT IMPLEMENTATION-- *)
val get_program_hotspots :
  Vm.opcode list -> int -> int -> basic_block_position list
