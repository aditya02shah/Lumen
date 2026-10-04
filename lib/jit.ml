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
