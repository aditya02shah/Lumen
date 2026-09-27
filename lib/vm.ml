(* --EXCEPTIONS-- *)
exception Internal_error (* should have been handled by the checker *)

(* --TYPE DECLARATIONS-- *)
type var_offset = int (* idx of local var *)
type address = int
type operand = INT of int | BOOL of bool

(* VM operations *)
type opcode =
  (* ALU *)
  | ARITHMETIC of Ast.arithmetic_op
  | BOOLEAN of Ast.logical_compare
  (* functions *)
  | CALL of address * operand list
  (* ctrl flow *)
  | JMP of address * address
  | RET
  (* data *)
  | LOAD_LOCAL of var_offset
  | STORE_LOCAL of var_offset
  | PUSH_CONST of operand

(* structure of each stack frame present in the call stack *)
type data = RET_ADDR of address | MAIN (* store return address *)

type stack_frame = {
  mutable local_var_region : operand list;
      (* stores local variables for each call frame; stable storage *)
  operand_stack : operand Stack.t;
      (* used to load temporary values and facilitates alu operations; transient storage *)
  data_region : data; (* stores return address; facilitates function calls *)
}

(* --Virtual Machine (VM) STATE-- *)
let call_stack : stack_frame Stack.t =
  Stack.create () (* keep a track of programs execution state *)

let inst_ptr : address ref =
  ref 0 (* address of next instruction to be executed *)

let flag : bool ref = ref false
(* set by comparison operations; faciliates jump instructions for control flow *)

(* --VM IMPLEMENTATION-- *)
let execute_operation (op : opcode) : operand option =
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
          ignore (Stack.push (INT new_value) operand_st);

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
          flag := result;
          (* update instruction pointer *)
          inst_ptr := !inst_ptr + 1;
          None
      | _ -> raise Internal_error)
  | CALL (address, operand_list) ->
      (* store the next instruction to be executed after the function call *)
      let next_inst = !inst_ptr + 1 in

      (* create a new stack frame for the function call *)
      let new_stack_frame : stack_frame =
        {
          local_var_region = operand_list;
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
  | JMP (br1_addr, br2_addr) ->
      (* update instruction pointer based on result *)
      let new_addr = match !flag with true -> br1_addr | false -> br2_addr in
      inst_ptr := new_addr;
      None
  | RET -> (
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

let run_program (byte_stream : opcode list) : unit =
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

  (* begin execution *)
  let rec process_program (byte_stream : opcode list) : unit =
    (* get instruction using instruction ptr *)
    let inst = List.nth byte_stream !inst_ptr in

    (* execute instruction *)
    match execute_operation inst with
    | Some value -> (
        (* execute_operation function only returns value when program has finished execution; display result *)
        match value with
        | INT i -> Printf.printf "Main function returned INT(%d)!\n" i
        | BOOL b -> Printf.printf "Main function returned BOOL(%b)!\n" b)
    | None -> process_program byte_stream (* continue processing program *)
  in
  process_program byte_stream
