let interpret_program (path : string) : unit =
  (* lex *)
  let tokens, lexer_errors = Lexer.tokenize path in

  print_endline "\n=== Lexer Errors ===";
  print_endline
    (if lexer_errors = [] then "None"
     else
       String.concat "\n"
         (List.map
            (fun (token, line) ->
              Printf.sprintf "Line %d: %s" line (Lexer.format_token token))
            lexer_errors));

  if lexer_errors <> [] then ()
  else
    (* parse *)
    let prog, remaining, parse_errs = Parser.parse_prog tokens in

    print_endline "\n=== Parser Errors ===";
    print_endline
      (if parse_errs = [] then "None" else String.concat "\n" parse_errs);

    if parse_errs <> [] then ()
    else
      (* semantic check *)
      let semantic_errors = Semantic_check.check_prog prog in

      print_endline "\n=== Semantic Errors ===";
      print_endline
        (if semantic_errors = [] then "None"
         else String.concat "\n" semantic_errors);

      if semantic_errors <> [] then () else ()

(* --MAIN-- *)
let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "examples/for.sui"
  in
  interpret_program path
