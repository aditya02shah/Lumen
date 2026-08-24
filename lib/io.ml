let read_file (filename : string) : string =
  try In_channel.with_open_text filename In_channel.input_all
  with Sys_error msg -> failwith ("Could not read file: " ^ msg)
