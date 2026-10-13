(* --EXCEPTIONS-- *)
exception Jit_error

(* --TYPE DECLARATIONS-- *)
type func_binary = { init : bytes; body : bytes; epilogue : bytes }

(* --JIT IMPLEMENTATION-- *)
val compile_arguments_to_asm : Opcode.opcode list list -> bytes
val compile_hot_function : Opcode.opcode list -> func_binary
val get_jmp_asm_offsets : int -> int list -> Opcode.opcode list -> int list
