(* --TYPE DECLARATIONS-- *)
type reg = REG of int

(* --EMIT IMPLEMENTATION-- *)
(* data *)
val mov_imm : reg -> int -> bytes
val mov_reg : reg -> reg -> bytes
val load : reg -> reg -> int -> bytes
val store : reg -> reg -> int -> bytes

(* arithmetic *)
val add : reg -> reg -> reg -> bytes
val add_imm : reg -> reg -> int -> bytes
val sub_imm : reg -> reg -> int -> bytes
val mul : reg -> reg -> reg -> bytes
val div_signed : reg -> reg -> reg -> bytes

(* comparison *)
val cmp : reg -> reg -> reg -> bytes
val cset : reg -> Ast.logical_compare -> bytes
val bitwise : reg -> reg -> reg -> Ast.logical_combination -> bytes

(* functions *)
val ret : unit -> bytes
