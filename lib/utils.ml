(* UTILS IMPLEMENTATION *)
exception Negative_exponent (* negative exponents *)

let read_file (filename : string) : string =
  (* read content from file path *)
  try In_channel.with_open_text filename In_channel.input_all
  with Sys_error msg -> failwith ("Could not read file: " ^ msg)

let format_msg (pos : int) (msg : string) : string =
  (* formatting util *)
  "Line (" ^ string_of_int pos ^ ") - " ^ msg

let rec pow (base : int) (exp : int) : int =
  (* simple power function *)
  if exp < 0 then raise Negative_exponent
  else if exp = 0 then 1
  else base * pow base (exp - 1)
