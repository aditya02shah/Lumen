(* --TYPE DECLARATIONS-- *)
type var_indices =
  (Ast.ident, int) Hashtbl.t (* maps variable name to indices *)

(* --GLOBAL VARS-- *)
let hashtable_size = 50

(* stores the instruction offset where each function begins *)
let func_details : (Ast.ident, int) Hashtbl.t = Hashtbl.create hashtable_size

(* --BYTECODE IMPLEMENTATION-- *)
let process_value ((value, pos) : Ast.value) (env : var_indices) :
    Vm.opcode list =
  match value with
  | IDENTIFIER ident ->
      let idx = Hashtbl.find env ident in
      [ Vm.LOAD_LOCAL idx ]
  | NUMBER i -> [ Vm.PUSH_CONST (INT i) ]

let rec process_cond_expr (cond_expr : Ast.cond_expr) (env : var_indices) :
    Vm.opcode list =
  match cond_expr with
  | BOOL_VAL (bv, _) -> (
      match bv with
      | BOOL b -> [ Vm.PUSH_CONST (BOOL b) ]
      | VAL_CMP (cmp, v1, v2) ->
          process_value v1 env @ process_value v2 env @ [ Vm.BOOLEAN cmp ])
  | COMBINATION ((comb, ce1, ce2), _) ->
      process_cond_expr ce1 env @ process_cond_expr ce2 env
      @ [ Vm.LOGICAL_COMBINATION comb ]

let rec process_expr ((expr, pos) : Ast.expr) (env : var_indices) :
    Vm.opcode list =
  (* final result of expr should be on operand stack *)
  match expr with
  | INT i -> [ Vm.PUSH_CONST (INT i) ]
  | VAR var ->
      let var_idx = Hashtbl.find env var in
      [ Vm.LOAD_LOCAL var_idx ]
  | INCREMENT var ->
      let var_idx = Hashtbl.find env var in
      [
        Vm.LOAD_LOCAL var_idx;
        Vm.PUSH_CONST (INT 1);
        Vm.ARITHMETIC ADD;
        Vm.STORE_LOCAL var_idx;
        Vm.LOAD_LOCAL var_idx;
      ]
  | FUNC_CALL (fc, _) ->
      (* determine instruction address where control should be transferred *)
      let offset = Hashtbl.find func_details fc.func_name in
      (* convert each argument expression into instructions *)
      let args = List.map (fun arg -> process_expr arg env) fc.args in
      [ CALL (offset, args) ]
  | BINARY (op, exp1, exp2) ->
      process_expr exp1 env @ process_expr exp2 env @ [ Vm.ARITHMETIC op ]
  | COND ce -> process_cond_expr ce env

let process_ret_expr ((ret_expr, pos) : Ast.ret_expr) (env : var_indices) :
    Vm.opcode list =
  match ret_expr with
  | EXPR e -> process_expr e env @ [ Vm.RET ]
  | SOME_EXPR e -> [] (* TODO: *)
  | NONE -> [] (* TODO: *)

let rec process_stmt (stmt : Ast.stmt) (env : var_indices) : Vm.opcode list =
  match stmt with
  | VARDECL vd -> (
      match vd with
      (* store all the vars declared *)
      | DECL (d, _) ->
          (* store all the variables declared *)
          List.iter
            (fun var_name -> Hashtbl.add env var_name (Hashtbl.length env))
            d.vars;
          []
      | DECL_ASST (da, _) -> (
          (* store var *)
          match da.asst with
          | at, _ -> (
              (* evaluate expr assigned to var & update env *)
              let idx = Hashtbl.length env in
              Hashtbl.add env at.name idx;
              match process_expr at.expr env with
              | [] -> raise Semantic_check.Internal_error
              | es -> es @ [ Vm.STORE_LOCAL idx ])))
  | ASST (at, _) -> (
      (* evaluate expr assigned to var & update env *)
      let var_idx = Hashtbl.find env at.name in
      match process_expr at.expr env with
      | [] -> raise Semantic_check.Internal_error
      | es -> es @ [ Vm.STORE_LOCAL var_idx ])
  | EXP expr -> process_expr expr env
  | RETURN ret_expr -> process_ret_expr ret_expr env
  | LOOP lp -> [] (* TODO: *)
  | IF (ce, blk) ->
      let blk_instn = process_block blk (Hashtbl.copy env) in
      process_cond_expr ce env
      @ [ Vm.JMP (1, 1 + List.length blk_instn) ]
      @ blk_instn
  | IF_ELSE ((ce, if_blk), else_blk) ->
      let if_blk_instn = process_block if_blk (Hashtbl.copy env) in
      let else_blk_instn = process_block else_blk (Hashtbl.copy env) in
      process_cond_expr ce env
      @ [ Vm.JMP (1, 1 + List.length if_blk_instn) ]
      @ if_blk_instn @ else_blk_instn
  | LET (({ var; func_call }, if_blk), else_blk) -> [] (* TODO: *)
  | EMPTY -> []

and process_block ((blk, pos) : Ast.block) (env : var_indices) : Vm.opcode list
    =
  (* process each statement in block *)
  let stmts = match blk with STMTS st -> st in
  List.fold_left (fun acc st -> acc @ process_stmt st env) [] stmts

let process_func_decl ((func_decl, pos) : Ast.func_decl) (instn_offset : int) :
    Vm.opcode list =
  (* create local env for each function, to keep track of its local variables *)
  let env = Hashtbl.create hashtable_size in

  (* add parameters as local variables *)
  ignore
    (List.iter
       (fun (ptype, pname) -> Hashtbl.add env pname (Hashtbl.length env))
       func_decl.params);

  (* store the instruction offset where each function begins *)
  Hashtbl.add func_details func_decl.func_name instn_offset;
  (* process function body *)
  process_block func_decl.body env

let process_program (prog : Ast.prog) : Vm.opcode list =
  (* process each func decl *)
  List.fold_left
    (fun acc fd -> acc @ process_func_decl fd (List.length acc))
    [] prog

let convert_ast_to_bytecode (prog : Ast.prog) : Vm.opcode list * int =
  (* returns byte code, along with index of starting instruction *)
  let bytecode = process_program prog in
  let main_func_start = Hashtbl.find func_details (Ast.IDENT "main") in
  (bytecode, main_func_start)
