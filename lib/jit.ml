(* --EXCEPTIONS-- *)
exception Jit_error

(* --TYPE DECLARATIONS-- *)
type func_binary = { init : bytes; body : bytes; epilogue : bytes }

(* --JIT IMPLEMENTATION-- *)
(* Registers for our virtual machine components *)
(* operand stack *)
let operand_stack_addr = 0 (* base address for operand stack *)
let operand_stack_ptr = ref 0 (* points to top of stack *)
let increment_sp () : unit = operand_stack_ptr := !operand_stack_ptr + 1
let decrement_sp () : unit = operand_stack_ptr := !operand_stack_ptr - 1
let operand_stack_reg = Emit.REG 5 (* stores stack ptr *)

(* local variables *)
let local_var_addr = 104 (* base address for local variables *)
let local_var_reg = Emit.REG 4

(* canonical *)
let return_register = Emit.REG 0
let stack_pointer = Emit.REG 31
let stack_size = 512 (* size of stack frame allocated *)
let operand_size = 8 (* bytes *)

(* flags *)
let bool_flag = Emit.REG 10
let opt_flag = Emit.REG 11

(* general *)
let temp = Emit.REG 3 (* scratchpad *)
let operand2 = Emit.REG 2 (* stores second operand for binary operations *)
let operand1 = Emit.REG 1 (* stores first operand for binary operations *)

let asm_instruction_count (instruction : Opcode.opcode) : int =
  match instruction with
  | PUSH_CONST _ -> 2
  | STORE_LOCAL _ | LOAD_LOCAL _ -> 2
  | SET_OPT_FLAG _ -> 1
  | JMP_OPT _ -> 3
  | UNCONDITIONAL_JMP _ -> 1
  | JMP _ -> 3
  | LOGICAL_COMBINATION _ -> 5
  | BOOLEAN _ -> 4
  | ARITHMETIC op -> ( match op with MOD -> 6 | POW -> 5 (* TODO: *) | _ -> 4)
  | RET -> 4
  | CALL _ -> 0 (* TODO: *)
  | RET_VOID -> 0 (* TODO: *)

let get_jmp_asm_offsets (inst_idx : int) (current_offsets : int list)
    (bytecode : Opcode.opcode list) : int list =
  let instruction_counts =
    List.map (fun op -> asm_instruction_count op) bytecode
  in
  (* starts[k] = number of asm instructions emitted before bytecode k *)
  let starts = 0 :: Utils.list_cumsum instruction_counts in
  let start_of k = List.nth starts k in
  (* position of each branch instruction within this bytecode inst's asm block:
     JMP emits cmp; b.eq; b  -> b.eq at 1, b at 2
     UNCONDITIONAL_JMP emits a single b at 0 *)
  let branch_positions =
    match List.nth bytecode inst_idx with
    | Opcode.JMP _ -> [ 1; 2 ]
    | _ -> [ 0 ]
  in
  List.map2
    (fun offset pos -> start_of (inst_idx + offset) - (start_of inst_idx + pos))
    current_offsets branch_positions

let compile_opcode_to_asm (instruction : Opcode.opcode) : bytes =
  (* encodes single VM instruction into equivalent ARM64 bytes *)
  match instruction with
  (* ALU *)
  | ARITHMETIC op ->
      (* pop second operand from the stack *)
      decrement_sp ();
      let inst1 =
        Emit.load operand2 operand_stack_reg (!operand_stack_ptr * operand_size)
      in
      (* pop first operand from the stack *)
      decrement_sp ();
      let inst2 =
        Emit.load operand1 operand_stack_reg (!operand_stack_ptr * operand_size)
      in

      (* perform operation *)
      let inst3 =
        match op with
        | ADD -> Emit.add temp operand1 operand2
        | SUBTRACT -> Emit.sub temp operand1 operand2
        | DIVIDE -> Emit.div_signed temp operand1 operand2
        | MULTIPLY -> Emit.mul temp operand1 operand2
        | MOD ->
            Bytes.concat Bytes.empty
              [
                (* divide *)
                Emit.div_signed temp operand1 operand2;
                (* get difference *)
                Emit.mul temp temp operand2;
                (* get remainder *)
                Emit.sub temp operand1 temp;
              ]
        | POW ->
            Bytes.concat Bytes.empty
              [
                (* multiply till operand2 becomes 0 *)
                Emit.mul temp operand1 operand2;
                Emit.sub_imm operand2 operand2 1;
                Emit.cmp_imm operand2 0;
                Emit.conditional_set temp Ast.EQUALS;
                (* TODO: *)
                Emit.unconditional_jmp (-5);
              ]
      in
      (* store result on operand stack *)
      let inst4 =
        Emit.store temp operand_stack_reg (!operand_stack_ptr * operand_size)
      in
      increment_sp ();
      Bytes.concat Bytes.empty
        [
          (* load two operands from stack operand *)
          inst1;
          inst2;
          (* store temp register value into stack operand *)
          inst3;
          inst4;
        ]
  | BOOLEAN op ->
      (* pop second operand from the stack *)
      decrement_sp ();
      let inst1 =
        Emit.load operand2 operand_stack_reg (!operand_stack_ptr * operand_size)
      in
      (* pop first operand from the stack *)
      decrement_sp ();
      let inst2 =
        Emit.load operand1 operand_stack_reg (!operand_stack_ptr * operand_size)
      in

      Bytes.concat Bytes.empty
        [
          inst1;
          inst2;
          (* perform comparison *)
          Emit.cmp operand1 operand2;
          (* set flag *)
          Emit.conditional_set bool_flag op;
        ]
  | LOGICAL_COMBINATION op ->
      (* pop second operand from the stack *)
      decrement_sp ();
      let inst1 =
        Emit.load operand2 operand_stack_reg (!operand_stack_ptr * operand_size)
      in
      (* pop first operand from the stack *)
      decrement_sp ();
      let inst2 =
        Emit.load operand1 operand_stack_reg (!operand_stack_ptr * operand_size)
      in

      Bytes.concat Bytes.empty
        [
          inst1;
          inst2;
          (* perform combination *)
          Emit.bitwise temp operand1 operand2 op;
          Emit.cmp_imm temp 1;
          (* set flag if result is 1 *)
          Emit.conditional_set bool_flag Ast.EQUALS;
        ]
  (* functions *)
  | CALL (addr, args) ->
      (* TODO: function calls in function being compiled aren't supported *)
      raise Jit_error
  | RET ->
      Bytes.concat Bytes.empty
        (decrement_sp ();
         [
           (* pop operand from stack into temp register *)
           Emit.load temp operand_stack_reg (!operand_stack_ptr * operand_size);
           (* store temp register value into return register x0 *)
           Emit.mov_reg return_register temp;
           (* free stack frame *)
           Emit.add_imm stack_pointer stack_pointer stack_size;
           (* return *)
           Emit.ret ();
         ])
  | RET_VOID ->
      (* TODO: function calls in function being compiled aren't supported *)
      raise Jit_error
  (* ctrl flow *)
  | JMP (br1_rel_addr, br2_rel_addr) ->
      Bytes.concat Bytes.empty
        [
          (* check if bool_flag is set *)
          Emit.cmp_imm bool_flag 1;
          (* if bool_flag is set, jump to br1_rel_addr *)
          Emit.conditional_jmp Ast.EQUALS br1_rel_addr;
          (* otherwise jump to br2_rel_addr *)
          Emit.unconditional_jmp br2_rel_addr;
        ]
  | UNCONDITIONAL_JMP addr -> Emit.unconditional_jmp addr
  (* optional types *)
  | JMP_OPT (br1_rel_addr, br2_rel_addr) ->
      Bytes.concat Bytes.empty
        [
          (* check if opt_flag is set *)
          Emit.cmp_imm opt_flag 1;
          (* if opt_flag is set, jump to br1_rel_addr *)
          Emit.conditional_jmp Ast.EQUALS br1_rel_addr;
          (* otherwise jump to br2_rel_addr *)
          Emit.unconditional_jmp br2_rel_addr;
        ]
  | SET_OPT_FLAG value -> Emit.mov_imm opt_flag 1
  (* data *)
  | LOAD_LOCAL offset ->
      let sp = !operand_stack_ptr in
      increment_sp ();
      Bytes.concat Bytes.empty
        [
          (* load local var into temp register *)
          Emit.load temp local_var_reg (offset * operand_size);
          (* store temp register value into stack operand *)
          Emit.store temp operand_stack_reg (sp * operand_size);
        ]
  | STORE_LOCAL offset ->
      decrement_sp ();
      Bytes.concat Bytes.empty
        [
          (* load operand stack value into temp register *)
          Emit.load temp operand_stack_reg (!operand_stack_ptr * operand_size);
          (* store temp register value into local vars *)
          Emit.store temp local_var_reg (offset * operand_size);
        ]
  | PUSH_CONST (INT i) ->
      let sp = !operand_stack_ptr in
      increment_sp ();
      Bytes.concat Bytes.empty
        [
          (* move immediate into temp *)
          Emit.mov_imm temp i;
          (* store temp register value into stack operand *)
          Emit.store temp operand_stack_reg (sp * operand_size);
        ]
  | _ -> Bytes.empty

let compile_arguments_to_asm (func_args : Opcode.opcode list list) : bytes =
  (* generates ARM64 bytes to store function arguments as local variables in stack frame *)
  operand_stack_ptr := 0;
  let process_arg (arg : Opcode.opcode list) : bytes list =
    List.rev
      (List.fold_left
         (fun acc inst -> compile_opcode_to_asm inst :: acc)
         [] arg)
  in

  func_args
  |> List.mapi (fun idx arg ->
      (* generate assembly for the argument *)
      let inst = process_arg arg in
      (* evaluated value for the argument will be stored on operand stack; pop it *)
      decrement_sp ();
      Bytes.concat Bytes.empty
        (inst
        @ [
            (* pop operand from stack into temp register *)
            Emit.load temp operand_stack_reg (!operand_stack_ptr * operand_size);
            (* store temp register value into local vars *)
            Emit.store temp local_var_reg (idx * operand_size);
          ]))
  |> Bytes.concat Bytes.empty

let compile_hot_function (func : Opcode.opcode list) : func_binary =
  (* encodes VM bytecode into ARM64 binary *)
  operand_stack_ptr := 0;
  let init =
    Bytes.concat Bytes.empty
      [
        (* allocate space for the stack frame *)
        Emit.sub_imm stack_pointer stack_pointer stack_size;
        (* operand stack begins at the start of the stack frame *)
        Emit.add_imm operand_stack_reg stack_pointer 0;
        (* local variables are stored at an offset of "local_var_addr" from the start of the stack frame *)
        Emit.add_imm local_var_reg stack_pointer local_var_addr;
      ]
  in

  (* encode function body *)
  let body =
    Bytes.concat Bytes.empty
      (List.rev
         (List.fold_left
            (fun acc inst -> compile_opcode_to_asm inst :: acc)
            [] func))
  in
  let epilogue =
    (* final value will be stored on the operand stack; pop it *)
    Bytes.concat Bytes.empty
      (decrement_sp ();
       [
         (* pop operand from stack into temp register *)
         Emit.load temp operand_stack_reg (!operand_stack_ptr * operand_size);
         (* store temp register value into return register x0 *)
         Emit.mov_reg return_register temp;
         (* free stack frame *)
         Emit.add_imm stack_pointer stack_pointer stack_size;
         (* return *)
         Emit.ret ();
       ])
  in
  { init; body; epilogue }
