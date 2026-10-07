(* --EXCEPTIONS-- *)
exception Jit_error

(* --TYPE DECLARATIONS-- *)
type basic_block_position = { blk_start : int; blk_end : int }

(* --HELPER FUNCTIONS-- *)
let rec get_basic_block_containing_inst (inst_idx : int)
    (basic_block_pos : basic_block_position list) : basic_block_position option
    (* returns the basic block containing the instruction, None otherwise *) =
  match basic_block_pos with
  | { blk_start; blk_end } :: rem ->
      (* check whether instruction lies within the basic block *)
      if inst_idx >= blk_start && inst_idx <= blk_end then
        Some { blk_start; blk_end }
      else
        (* continue looking through the other basic blocks *)
        get_basic_block_containing_inst inst_idx rem
  | _ -> None

let get_basic_blocks_position (bytecode : Vm.opcode list) :
    basic_block_position list =
  (* returns starting position of each basic block in the program *)
  let rec get_basic_blocks (code : Vm.opcode list) (blk_start_idx : int)
      (cur_inst_idx : int) (basic_blocks : basic_block_position list) :
      basic_block_position list =
    match code with
    | JMP _ :: rem | JMP_OPT _ :: rem | RET :: rem | RET_VOID :: rem ->
        (* end of current basic block *)
        get_basic_blocks rem (cur_inst_idx + 1) (cur_inst_idx + 1)
          ({ blk_start = blk_start_idx; blk_end = cur_inst_idx } :: basic_blocks)
    | op :: rem ->
        (* still in current basic block *)
        get_basic_blocks rem blk_start_idx (cur_inst_idx + 1) basic_blocks
    | [] -> List.rev basic_blocks
  in
  get_basic_blocks bytecode 0 0 []

(* --JIT IMPLEMENTATION-- *)
let get_program_hotspots (bytecode : Vm.opcode list) (main_offset : int)
    (threshold : float) : basic_block_position list =
  (* returns the basic blocks in the program which are hotspots *)
  let bb = get_basic_blocks_position bytecode in
  let line_counter = Vm.run_program bytecode main_offset false in

  (* get total number of instructions in the program *)
  let total_count =
    Hashtbl.fold (fun idx cnt acc -> acc + cnt) line_counter 0
  in

  (* determine hotspots at instruction level in the program *)
  let inst_hotspots =
    List.rev
      (Hashtbl.fold
         (fun inst_idx isnt_count acc ->
           (* if execution of instruction >= threshold, add it to list of hotspots *)
           if Float.of_int isnt_count /. Float.of_int total_count >= threshold
           then inst_idx :: acc
           else acc)
         line_counter [])
  in

  let rec process_hotspots (inst_indices : int list)
      (basic_blk_pos : basic_block_position list)
      (basic_blk_hotspots : basic_block_position list) :
      basic_block_position list =
    (* convert finer instruction level hotspots into coarser basic block level hotspots *)
    match inst_indices with
    | idx :: rem -> (
        (* identify which basic block instruction belongs to *)
        match get_basic_block_containing_inst idx basic_blk_pos with
        | Some pos -> (
            (* check whether basic block is already presented in hotspots accumulated so far *)
            match List.mem pos basic_blk_hotspots with
            | true -> process_hotspots rem basic_blk_pos basic_blk_hotspots
            | false ->
                process_hotspots rem basic_blk_pos (pos :: basic_blk_hotspots))
        | None -> raise Jit_error)
    | [] -> List.rev basic_blk_hotspots
  in
  process_hotspots inst_hotspots bb []

(* --JIT BACKEND IMPLEMENTATION-- *)
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

(* general *)
let temp = Emit.REG 3 (* scratchpad *)

let arithmetic_op2 =
  Emit.REG 2 (* store second operand for binary arithmetic operations *)

let arithmetic_op1 =
  Emit.REG 1 (* store first operand for binary arithmetic operations *)

let compile_opcode_to_asm (instruction : Vm.opcode) : bytes =
  (* encodes single VM instruction into equivalent ARM64 bytes *)
  match instruction with
  | ARITHMETIC ADD ->
      (* pop second operand from the stack *)
      decrement_sp ();
      let inst1 =
        Emit.load arithmetic_op2 operand_stack_reg
          (!operand_stack_ptr * operand_size)
      in
      (* pop first operand from the stack *)
      decrement_sp ();
      let inst2 =
        Emit.load arithmetic_op1 operand_stack_reg
          (!operand_stack_ptr * operand_size)
      in

      (* perform operation *)
      let inst3 = Emit.add temp arithmetic_op1 arithmetic_op2 in
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

let compile_arguments_to_asm (func_args : Vm.opcode list list) : bytes =
  (* generates ARM64 bytes to store function arguments as local variables in stack frame *)
  let process_arg (arg : Vm.opcode list) : bytes list =
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

let compile_hot_function (func : Vm.opcode list)
    (func_args : Vm.opcode list list) (debug : bool) : bytes =
  (* encodes VM bytecode into ARM64 binary *)
  let init =
    [
      (* allocate space for the stack frame *)
      Emit.sub_imm stack_pointer stack_pointer stack_size;
      (* operand stack begins at the start of the stack frame *)
      Emit.add_imm operand_stack_reg stack_pointer 0;
      (* local variables are stored at an offset of "local_var_addr" from the start of the stack frame *)
      Emit.add_imm local_var_reg stack_pointer local_var_addr;
    ]
  in
  (* encode arguments *)
  let args = compile_arguments_to_asm func_args in
  (* encode function body *)
  let assembly =
    List.rev
      (List.fold_left
         (fun acc inst -> compile_opcode_to_asm inst :: acc)
         [] func)
  in
  let epilogue =
    (* final value will be stored on the operand stack; pop it *)
    decrement_sp ();
    [
      (* pop operand from stack into temp register *)
      Emit.load temp operand_stack_reg (!operand_stack_ptr * operand_size);
      (* store temp register value into return register x0 *)
      Emit.mov_reg return_register temp;
      (* free stack frame *)
      Emit.add_imm stack_pointer stack_pointer stack_size;
      (* return *)
      Emit.ret ();
    ]
  in
  (* create binary by stringing together assembly encodings *)
  let binary =
    Bytes.concat Bytes.empty (init @ [ args ] @ assembly @ epilogue)
  in
  (* store binary for debugging purposes *)
  if debug then (
    let oc = open_out_bin "backend/test/program" in
    output_bytes oc binary;
    close_out oc);
  binary
