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

        let exec_info = Vm.run_program opcodes main_start true in

        List.iteri
          (fun i opcode ->
            let count =
              match Hashtbl.find_opt exec_info i with
              | Some count -> count
              | None -> 0
            in
            Printf.printf "%d: %-30s [%d]\n" i (string_of_opcode opcode) count)
          opcodes;

        let hotspot_bb =
          Jit.get_program_hotspots opcodes main_start Cfg.hotspot_threshold
        in
        List.iteri
          (fun i { Jit.blk_start; blk_end } ->
            Printf.printf "Hotspot Basic block %d start at %d and end at %d\n" i
              blk_start blk_end)
          hotspot_bb;

        let func = List.filteri (fun i _ -> i < 6) opcodes in
        let call = List.nth opcodes 6 in
        let arguments = match call with CALL (addr, args) -> args | _ -> [] in

        let binary = Jit.compile_hot_function func arguments false in
        let final = run_code binary in
        Printf.printf "Result from C: %d\n" final

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "examples/demo2.sui"
  in
  interpret_program path
