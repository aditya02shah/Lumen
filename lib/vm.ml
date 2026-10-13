(* --EXCEPTIONS-- *)
exception Internal_error (* should have been handled by the checker *)

(* --TYPE DECLARATIONS-- *)
external run_code : bytes -> int = "run_code"

(* structure of each stack frame present in the call stack *)
type data = RET_ADDR of Opcode.address | MAIN (* store return address *)

type stack_frame = {
  mutable local_var_region : Opcode.operand list;
      (* stores local variables for each call frame; stable storage *)
  operand_stack : Opcode.operand Stack.t;
      (* used to load temporary values and facilitates alu operations; transient storage *)
  data_region : data; (* stores return address; facilitates function calls *)
}

type previous = ADDR of Opcode.address ref | None
type flag = VAL of bool | EMPTY

(* execution information *)
type inst_idx = int
type exec_count = int
type inst_counter = (inst_idx, exec_count) Hashtbl.t

(* jit information *)
type func_position = { func_start : int; func_end : int }

let jit_cache : (func_position, Jit.func_binary) Hashtbl.t =
  Hashtbl.create Cfg.hashtable_size

(* --HOTSPOT DETECTION-- *)
let rec get_func_containing_inst (inst_idx : int)
    (func_positions : func_position list) : func_position option
    (* returns the basic block containing the instruction, None otherwise *) =
  match func_positions with
  | { func_start; func_end } :: rem ->
      (* check whether instruction lies within the basic block *)
      if inst_idx >= func_start && inst_idx <= func_end then
        Some { func_start; func_end }
      else
        (* continue looking through the other basic blocks *)
        get_func_containing_inst inst_idx rem
  | _ -> None

let get_func_position (bytecode : Opcode.opcode list) : func_position list =
  (* returns starting position of each basic block in the program *)
  let rec get_funcs (code : Opcode.opcode list) (func_start_idx : int)
      (cur_inst_idx : int) (functions : func_position list) : func_position list
      =
    match code with
    | RET :: rem | RET_VOID :: rem ->
        (* end of current basic block *)
        get_funcs rem (cur_inst_idx + 1) (cur_inst_idx + 1)
          ({ func_start = func_start_idx; func_end = cur_inst_idx } :: functions)
    | op :: rem ->
        (* still in current basic block *)
        get_funcs rem func_start_idx (cur_inst_idx + 1) functions
    | [] -> List.rev functions
  in
  get_funcs bytecode 0 0 []

(* --VM IMPLEMENTATION-- *)
(* --Virtual Machine (VM) STATE-- *)
let call_stack : stack_frame Stack.t =
  Stack.create () (* keep a track of programs execution state *)

let inst_ptr : Opcode.address ref =
  ref 0 (* address of next instruction to be executed *)

let bool_flag : flag ref = ref (VAL false)
(* set by comparison operations; faciliates jump instructions for control flow *)

let opt_flag : flag ref = ref (VAL false)
(* set by optional functions, to indictate which path was taken *)

let prev_function : previous = ADDR (ref 0)

let clear_flags () : unit =
  bool_flag := EMPTY;
  opt_flag := EMPTY

let rec execute_operation (op : Opcode.opcode) : Opcode.operand option =
  (* returns operand value executing the last instruction; otherwise returns None *)
  match op with
  | ARITHMETIC ap -> (
      (* pop two operands from the operand stack region of the stack frame *)
      let operand_st = (Stack.top call_stack).operand_stack in
      let val2 = Stack.pop operand_st in
      let val1 = Stack.pop operand_st in

      (* calculate value *)
      match (val1, val2) with
      | INT i1, INT i2 ->
          let new_value =
            match ap with
            | ADD -> i1 + i2
            | MULTIPLY -> i1 * i2
            | SUBTRACT -> i1 - i2
            | DIVIDE -> i1 / i2
            | MOD -> i1 mod i2
            | POW -> Utils.pow i1 i2
          in
          (* push result onto operand stack *)
          ignore (Stack.push (Opcode.INT new_value) operand_st);

          (* update instruction pointer *)
          inst_ptr := !inst_ptr + 1;
          None
      | _ -> raise Internal_error)
  | BOOLEAN bp -> (
      (* pop two operands from the operand stack region of the stack frame *)
      let operand_st = (Stack.top call_stack).operand_stack in
      let val2 = Stack.pop operand_st in
      let val1 = Stack.pop operand_st in

      (* calculate result *)
      match (val1, val2) with
      | INT i1, INT i2 ->
          let result =
            match bp with
            | LESS_THAN -> i1 < i2
            | LESS_THAN_EQUALS -> i1 <= i2
            | GREATER_THAN -> i1 > i2
            | GREATER_THAN_EQUALS -> i1 >= i2
            | EQUALS -> i1 = i2
            | NOT_EQUALS -> i1 <> i2
          in
          (* update flag with result *)

          bool_flag := VAL result;
          (* update instruction pointer *)
          inst_ptr := !inst_ptr + 1;
          None
      | _ -> raise Internal_error)
  | LOGICAL_COMBINATION lc -> (
      let operand_st = (Stack.top call_stack).operand_stack in
      let val2 = Stack.pop operand_st in
      let val1 = Stack.pop operand_st in

      (* calculate result *)
      match (val1, val2) with
      | BOOL b1, BOOL b2 ->
          let result =
            match (lc, b1, b2) with
            | LOGICAL_AND, true, true -> true
            | LOGICAL_AND, _, _ -> false
            | LOGICAL_OR, false, false -> false
            | LOGICAL_OR, _, _ -> true
          in
          (* update flag with result *)
          bool_flag := VAL result;
          (* update instruction pointer *)
          inst_ptr := !inst_ptr + 1;
          None
      | _ -> raise Internal_error)
  | CALL (address, operand_list) ->
      (* prev_function := !inst_ptr; *)
      (match prev_function with ADDR prev -> prev := !inst_ptr | _ -> ());

      (* store the next instruction to be executed after the function call *)
      let next_inst = !inst_ptr + 1 in
      (* each operand can be a list of instructions itself *)
      let rec process_operands (op_list : Opcode.opcode list) : Opcode.operand =
        (* process each parameter *)
        match op_list with
        | p :: rem ->
            (* execute instruction *)
            ignore (execute_operation p);
            process_operands rem
        | [] ->
            (* since each operand is an expressions, its evaluated value will be on top of the operand stack *)
            Stack.pop (Stack.top call_stack).operand_stack
      in
      let vars =
        List.map (fun operands -> process_operands operands) operand_list
      in

      (* create a new stack frame for the function call *)
      let new_stack_frame : stack_frame =
        {
          local_var_region = vars;
          (* store arguments as local vars in the new stack frame *)
          operand_stack = Stack.create ();
          data_region = RET_ADDR next_inst;
          (* store return address (next instn after function call) *)
        }
      in
      (* push stack frame onto the call stack *)
      ignore (Stack.push new_stack_frame call_stack);
      (* update instruction pointer so we begin executing the function *)
      inst_ptr := address;
      None
  | JMP (br1_rel_addr, br2_rel_addr) ->
      (* update instruction pointer based on result *)
      let rel_addr =
        match !bool_flag with
        | VAL true -> br1_rel_addr
        | VAL false -> br2_rel_addr
        | EMPTY -> raise Internal_error
      in
      inst_ptr := !inst_ptr + rel_addr;
      None
  | JMP_OPT (br1_rel_addr, br2_rel_addr) ->
      (* update instruction pointer based on result *)
      let rel_addr =
        match !bool_flag with
        | VAL true -> br1_rel_addr
        | VAL false -> br2_rel_addr
        | EMPTY -> raise Internal_error
      in
      inst_ptr := !inst_ptr + rel_addr;
      None
  | SET_OPT_FLAG value ->
      (* update value *)
      opt_flag := VAL value;
      (* update instruction pointer *)
      inst_ptr := !inst_ptr + 1;
      None
  | UNCONDITIONAL_JMP rel_addr ->
      inst_ptr := !inst_ptr + rel_addr;
      None
  | RET -> (
      ignore (prev_function = None);
      (* determine if stack frame has a return address *)
      let stack_frame = Stack.top call_stack in
      let return_addr = stack_frame.data_region in
      let return_value = Stack.top stack_frame.operand_stack in

      match return_addr with
      | RET_ADDR addr ->
          (* return address indicates the next instn to be executed; update instn ptr *)
          inst_ptr := addr;
          (* remove stack frame from call stack since function has finished execution *)
          ignore (Stack.pop call_stack);
          (* return value is stored on the operand stack of stack frame of callee; 
          provide it to caller by placing the return value on its operand stack *)
          Stack.push return_value (Stack.top call_stack).operand_stack;
          None
      | MAIN ->
          (* update instruction pointer *)
          inst_ptr := !inst_ptr + 1;
          (* we are at the main stack frame, we have reached the end of the program; 
          return the value on operand stack *)
          Some return_value)
  | RET_VOID -> (
      let stack_frame = Stack.top call_stack in
      let return_addr = stack_frame.data_region in

      match return_addr with
      | RET_ADDR addr ->
          (* return address indicates the next instn to be executed; update instn ptr *)
          inst_ptr := addr;
          (* remove stack frame from call stack since function has finished execution *)
          ignore (Stack.pop call_stack);
          None
      | MAIN ->
          (* update instruction pointer *)
          inst_ptr := !inst_ptr + 1;
          None)
  | LOAD_LOCAL var_offset ->
      (* gets value of local variable and push it onto the operand stack *)
      let frame = Stack.top call_stack in
      let var_value = List.nth frame.local_var_region var_offset in
      Stack.push var_value frame.operand_stack;

      (* update instruction pointer *)
      inst_ptr := !inst_ptr + 1;
      None
  | STORE_LOCAL var_offset ->
      (* pop value from operand stack and store it as a local variable  *)
      let frame = Stack.top call_stack in
      let value = Stack.pop frame.operand_stack in

      (* if local variable already exists, update its value; otherwise create a new entry *)
      let num_local_vars = List.length frame.local_var_region in
      if var_offset >= num_local_vars then
        (* var doesnt exist *)
        frame.local_var_region <- frame.local_var_region @ [ value ]
      else
        (* var exists, so update it *)
        frame.local_var_region <-
          Utils.replace_list_element_at_idx frame.local_var_region var_offset
            value;

      (* update instruction pointer *)
      inst_ptr := !inst_ptr + 1;
      None
  | PUSH_CONST operand ->
      (* push operand onto stack *)
      let frame = Stack.top call_stack in
      Stack.push operand frame.operand_stack;
      (* update instruction pointer *)
      inst_ptr := !inst_ptr + 1;
      None

(* --PUBLIC INTERFACE-- *)
let run_program (bytecode : Opcode.opcode list) (main_offset : int)
    (verbose : bool) : unit =
  (* used to keep track of how many times each instruction was executed *)
  let counter : inst_counter = Hashtbl.create Cfg.hashtable_size in
  (* setup stack frame for main *)
  let main_stack_frame : stack_frame =
    {
      local_var_region = [];
      operand_stack = Stack.create ();
      data_region = MAIN;
    }
  in
  (* push stack frame onto call stack *)
  ignore (Stack.push main_stack_frame call_stack);
  inst_ptr := main_offset;

  let func_positions = get_func_position bytecode in

  (* begin execution *)
  let rec process_program (byte_stream : Opcode.opcode list) : unit =
    let inst_idx = !inst_ptr in
    (* get instruction using instruction ptr *)
    let inst = List.nth byte_stream inst_idx in

    let jitted =
      match Hashtbl.find_opt counter inst_idx with
      | Some v ->
          Hashtbl.replace counter inst_idx (v + 1);
          if v + 1 >= Cfg.jit_threshold then
            (* find function containing instruction *)
            match get_func_containing_inst inst_idx func_positions with
            | Some { func_start; func_end } ->
                (* get function body *)
                let func_body =
                  List.filteri
                    (fun idx _ -> idx >= func_start && idx <= func_end)
                    byte_stream
                in

                (* adjust jump offsets *)
                let func_body =
                  List.mapi
                    (fun i inst ->
                      match inst with
                      | Opcode.JMP (a1, a2) ->
                          let new_addr =
                            Jit.get_jmp_asm_offsets i [ a1; a2 ] func_body
                          in
                          Opcode.JMP (List.nth new_addr 0, List.nth new_addr 1)
                      | UNCONDITIONAL_JMP a ->
                          let new_addr =
                            Jit.get_jmp_asm_offsets i [ a ] func_body
                          in
                          Opcode.UNCONDITIONAL_JMP (List.nth new_addr 0)
                      | _ -> inst)
                    func_body
                in

                (* get function arguments *)
                let func_call_idx =
                  let prev =
                    match prev_function with
                    | ADDR p -> p
                    | _ -> raise Internal_error
                  in
                  !prev
                in
                let func_arguments =
                  match List.nth byte_stream func_call_idx with
                  | CALL (_, args) -> args
                  | _ -> raise Internal_error
                in

                (* encode function arguments *)
                let args = Jit.compile_arguments_to_asm func_arguments in

                (* check whether function has been compiled already *)
                let binary =
                  match Hashtbl.find_opt jit_cache { func_start; func_end } with
                  | Some entry ->
                      (* function has already been compiled. reconstruct its binary *)
                      Bytes.concat Bytes.empty
                        [ entry.init; args; entry.body; entry.epilogue ]
                  | None ->
                      (* function needs to be compiled  *)
                      let prog = Jit.compile_hot_function func_body in

                      Hashtbl.replace jit_cache { func_start; func_end } prog;

                      Bytes.concat Bytes.empty
                        [ prog.init; args; prog.body; prog.epilogue ]
                in
                (match Cfg.debug with
                | DIR d ->
                    let oc =
                      let fname =
                        Printf.sprintf "%s/F_%d_%d" d func_start func_end
                      in
                      open_out_bin fname
                    in

                    output_bytes oc binary;
                    close_out oc
                | FALSE -> ());

                (* execute it *)
                let result = run_code binary in

                (* clear flags *)
                clear_flags ();

                (match Cfg.debug with
                | DIR d ->
                    Printf.printf
                      "Ran JIT for func{ST=%d,END=%d}and got RES = %d!\n"
                      func_start func_end result
                | FALSE -> ());

                (match (Stack.top call_stack).data_region with
                | RET_ADDR addr ->
                    ignore (Stack.pop call_stack);
                    Stack.push (Opcode.INT result)
                      (Stack.top call_stack).operand_stack;
                    inst_ptr := addr
                | MAIN -> raise Internal_error);
                true
            | None -> raise Internal_error
          else false
      | None ->
          Hashtbl.add counter inst_idx 1;
          false
    in

    if jitted then process_program byte_stream
    else
      (* execute instruction *)
      match execute_operation inst with
      | Some value -> (
          if
            (* execute_operation only returns a value when the program has
               finished execution; display result *)
            verbose
          then
            match value with
            | INT i -> Printf.printf "Main function returned INT(%d)!\n" i
            | BOOL b -> Printf.printf "Main function returned BOOL(%b)!\n" b)
      | None -> process_program byte_stream (* continue processing program *)
  in
  process_program bytecode
