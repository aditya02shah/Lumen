let interpret_program (path : string) : unit =
  (* lex *)
  let tokens, lexer_errors = Lexer.tokenize path in

  if lexer_errors <> [] then
    List.iter
      (fun (token, line) ->
        Printf.printf "LEXER ERROR line %d: %s\n" line
          (Lexer.format_token token))
      lexer_errors
  else
    (* parse *)
    let prog, remaining, parse_errs = Parser.parse_prog tokens in

    if parse_errs <> [] then
      List.iter (fun e -> Printf.printf "PARSE ERROR: %s\n" e) parse_errs
    else
      (* semantic check *)
      let semantic_errors = Semantic_check.check_prog prog in

      if semantic_errors <> [] then
        List.iter
          (fun e -> Printf.printf "SEMANTIC ERROR: %s\n" e)
          semantic_errors
      else
        (* interpret *)
        let result = Interpreter.eval_program prog in
        match result with
        | Some (Semantic_check.INT i) -> Printf.printf "%d\n" i
        | Some (Semantic_check.BOOL b) -> Printf.printf "%b\n" b
        | None -> print_endline "no value returned"

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "examples/demo9.sui"
  in
  interpret_program path
