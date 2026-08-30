(* let path = Sys.argv.(1)
let prog = Io.read_file path
let tokens, errors = Lexer.tokenize prog

let () =
  Lexer.print_list Lexer.format_token tokens;

  match errors with
  | [] -> ()
  | _ ->
      print_string "\nERRORS:\n";
      Lexer.print_list Lexer.format_token errors *)

(* let tokens2 =
  [ Lexer.NUMBER 2; Lexer.MULTIPLY; Lexer.NUMBER 3; Lexer.ADD; Lexer.NUMBER 5 ] *)
(* let tokens2 = [ Lexer.NUMBER 2; Lexer.ADD; Lexer.NUMBER 3 ] *)
let tokens2 =
  [
    Lexer.NUMBER 2;
    Lexer.MULTIPLY;
    Lexer.NUMBER 3;
    Lexer.ADD;
    Lexer.NUMBER 5;
    Lexer.DIVIDE;
    Lexer.NUMBER 7;
    Lexer.ADD;
    Lexer.NUMBER 10;
    Lexer.DIVIDE;
    Lexer.NUMBER 11;
  ]

let () =
  match Parser.match_expr tokens2 with
  | Some e, _ -> print_endline (Parser.format_expr e)
  | None, _ -> print_endline "not found"
