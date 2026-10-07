(* --TYPE DECLARATIONS-- *)
type reg = REG of int

(* --EMIT IMPLEMENTATION-- *)
val add : reg -> reg -> reg -> bytes
val add_imm : reg -> reg -> int -> bytes
val sub_imm : reg -> reg -> int -> bytes
val store : reg -> reg -> int -> bytes
val load : reg -> reg -> int -> bytes
val mov_imm : reg -> int -> bytes
val mov_reg : reg -> reg -> bytes
val ret : unit -> bytes
