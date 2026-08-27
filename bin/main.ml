let path = Sys.argv.(1)
let prog = Io.read_file path
let tokens, errors = Lexer.tokenize prog

let () =
  Lexer.print_list Lexer.format_token tokens;

  match errors with
  | [] -> ()
  | _ ->
      print_string "\nERRORS:\n";
      Lexer.print_list Lexer.format_token errors
