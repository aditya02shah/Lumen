(* UTILS IMPLEMENTATION *)
let read_file (filename : string) : string =
  (* read content from file path *)
  try In_channel.with_open_text filename In_channel.input_all
  with Sys_error msg -> failwith ("Could not read file: " ^ msg)

let format_msg (pos : int) (msg : string) : string =
  (* formatting util *)
  "Line (" ^ string_of_int pos ^ ") - " ^ msg
