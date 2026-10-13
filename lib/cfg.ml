let hashtable_size = 50

let jit_threshold =
  15 (* if instruction is executed >= jit_threshold times, jit it *)

type debug = DIR of string | FALSE

(* let debug = DIR "backend/test/" *)
let debug = FALSE
