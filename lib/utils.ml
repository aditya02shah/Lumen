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

let rec replace_list_element_at_idx (l : 'a list) (idx : int) (new_val : 'a) :
    'a list =
  (* replace element at a particular idx of the list *)
  match l with
  | [] -> []
  | x :: xs ->
      if idx = 0 then new_val :: xs
      else x :: replace_list_element_at_idx xs (idx - 1) new_val

let rec rotate_list (l : 'a list) (idx : int) (accum : 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs ->
      if idx = 0 then (x :: xs) @ List.rev accum
      else rotate_list xs (idx - 1) (x :: accum)
