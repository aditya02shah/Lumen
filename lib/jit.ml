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

(* general *)
let temp = Emit.REG 3 (* scratchpad *)

let arithmetic_op2 =
  Emit.REG 2 (* store second operand for binary arithmetic operations *)

let arithmetic_op1 =
  Emit.REG 1 (* store first operand for binary arithmetic operations *)

let compile_opcode_to_asm (instruction : Opcode.opcode) : bytes =
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

let compile_arguments_to_asm (func_args : Opcode.opcode list list) : bytes =
  (* generates ARM64 bytes to store function arguments as local variables in stack frame *)
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
