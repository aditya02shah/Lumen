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
        let opcodes, main_start = Bytecode.convert_ast_to_bytecode prog in

        Vm.run_program opcodes main_start;

        List.iteri
          (fun i opcode ->
            let count =
              match Hashtbl.find_opt Vm.line_counter i with
              | Some count -> count
              | None -> 0
            in
            Printf.printf "%d: %-30s [%d]\n" i (string_of_opcode opcode) count)
          opcodes

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "examples/demo2.sui"
  in
  interpret_program path
