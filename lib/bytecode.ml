(* --TYPE DECLARATIONS-- *)
type var_indices = (Ast.ident, int) Hashtbl.t

(* --GLOBAL VARS-- *)
let hashtable_size = 50

(* --BYTECODE IMPLEMENTATION-- *)
let rec process_cond_expr (cond_expr : Ast.cond_expr) (env : var_indices) :
    Vm.opcode list =
  (* TODO: *)
  []

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
  | FUNC_CALL fc -> [] (* TODO: *)
  | BINARY (op, exp1, exp2) ->
      process_expr exp1 env @ process_expr exp2 env @ [ Vm.ARITHMETIC op ]
  | COND ce -> process_cond_expr ce env

let process_ret_expr ((ret_expr, pos) : Ast.ret_expr) (env : var_indices) :
    Vm.opcode list =
  match ret_expr with
  | EXPR e -> process_expr e env @ [ Vm.RET ]
  | SOME_EXPR e -> [] (* TODO: *)
  | NONE -> [] (* TODO: *)

let process_stmt (stmt : Ast.stmt) (env : var_indices) : Vm.opcode list =
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
  | IF (ce, blk) -> [] (* TODO: *)
  | IF_ELSE ((ce, if_blk), else_blk) -> [] (* TODO: *)
  | LET (({ var; func_call }, if_blk), else_blk) -> [] (* TODO: *)
  | EMPTY -> []

let process_block ((blk, pos) : Ast.block) (env : var_indices) : Vm.opcode list
    =
  let stmts = match blk with STMTS st -> st in
  List.fold_left (fun acc st -> acc @ process_stmt st env) [] stmts

let process_func_decl ((func_decl, pos) : Ast.func_decl) : Vm.opcode list =
  let env = Hashtbl.create 20 in
  ignore
    (List.iter
       (fun (ptype, pname) -> Hashtbl.add env pname (Hashtbl.length env))
       func_decl.params);
  process_block func_decl.body env

let process_program (prog : Ast.prog) : Vm.opcode list =
  List.fold_left (fun acc fd -> acc @ process_func_decl fd) [] prog

let convert_ast_to_bytecode (prog : Ast.prog) : Vm.opcode list =
  process_program prog
