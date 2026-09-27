(* --EXCEPTIONS-- *)
exception Negative_exponent (* negative exponents *)

(* --TYPE DECLARATIONS-- *)
type parent_scope = SCOPE of Semantic_check.scope_vars | NONE
type eval_result = VALUE of Semantic_check.eval_value | RETURN_NONE

(* --HELPER FUNCTIONS-- *)
let rec get_var_value (var : Ast.ident) (env : Semantic_check.scope_vars) :
    (* extracts value from variable *)
    Semantic_check.eval_value =
  match (Hashtbl.find env var).assigned_val with
  | EXPR e -> (
      match eval_expr e env with
      | Some (VALUE v) -> v
      | _ -> raise Semantic_check.Internal_error)
  | VAL v -> v
  | _ -> raise Semantic_check.Internal_error

(* --INTERPRETER IMPLEMENTATION-- *)
and eval_cond_expr (cond_expr : Ast.cond_expr) (env : Semantic_check.scope_vars)
    : bool =
  (* evaluate conditional expression *)
  match cond_expr with
  | BOOL_VAL (bv, _) -> (
      match bv with
      | BOOL b -> b
      | VAL_CMP (op, (val1, _), (val2, _)) -> (
          (* extract value from operands *)
          let op1, op2 =
            ( (match val1 with
              | IDENTIFIER v -> get_var_value v env
              | NUMBER i -> INT i),
              match val2 with
              | IDENTIFIER v -> get_var_value v env
              | NUMBER i -> INT i )
          in
          (* compare values *)
          match (op, op1, op2) with
          | LESS_THAN, INT i1, INT i2 -> i1 < i2
          | LESS_THAN_EQUALS, INT i1, INT i2 -> i1 <= i2
          | GREATER_THAN, INT i1, INT i2 -> i1 > i2
          | GREATER_THAN_EQUALS, INT i1, INT i2 -> i1 >= i2
          | EQUALS, INT i1, INT i2 -> i1 = i2
          | NOT_EQUALS, INT i1, INT i2 -> i1 <> i2
          | _ -> raise Semantic_check.Internal_error))
  | COMBINATION ((op, ce1, ce2), _) -> (
      (* apply combination *)
      match (op, eval_cond_expr ce1 env, eval_cond_expr ce2 env) with
      | LOGICAL_AND, true, true -> true
      | LOGICAL_AND, _, _ -> false
      | LOGICAL_OR, false, false -> false
      | LOGICAL_OR, _, _ -> true)

and eval_expr ((expr, pos) : Ast.expr) (env : Semantic_check.scope_vars) :
    eval_result option =
  (* evaluate expression *)
  match expr with
  | INT n -> Some (VALUE (INT n))
  | VAR x -> (
      (* find var entry in env *)
      match (Hashtbl.find env x).assigned_val with
      | EXPR e -> eval_expr e env (* evaluate expr assigned to var *)
      | VAL v -> Some (VALUE v) (* var value has already been evaluated *)
      | IS_PARAM -> raise Semantic_check.Internal_error
      | NONE -> raise Semantic_check.Internal_error)
  | INCREMENT x -> (
      (* find var entry in env *)
      let entry = Hashtbl.find env x in
      match entry.assigned_val with
      | EXPR e -> (
          (* evaluate expr assigned to var & increment it *)
          match eval_expr e env with
          | Some (VALUE (INT i)) ->
              (* update value *)
              Hashtbl.replace env x
                {
                  mut = entry.mut;
                  dtype = entry.dtype;
                  assigned_val = VAL (INT (i + 1));
                };
              (* return it *)
              Some (VALUE (INT (i + 1)))
          | Some (VALUE (BOOL _)) -> raise Semantic_check.Internal_error
          | _ -> raise Semantic_check.Internal_error)
      | VAL v -> (
          (* var value has already been evaluated; increment it *)
          match v with
          | INT i ->
              (* update value *)
              Hashtbl.replace env x
                {
                  mut = entry.mut;
                  dtype = entry.dtype;
                  assigned_val = VAL (INT (i + 1));
                };
              (* return it *)
              Some (VALUE (INT (i + 1)))
          | BOOL _ -> raise Semantic_check.Internal_error)
      | IS_PARAM -> raise Semantic_check.Internal_error
      | NONE -> raise Semantic_check.Internal_error)
  | FUNC_CALL f -> eval_func_call f env
  | BINARY (op, exp1, exp2) -> (
      (* evaluate arithmetic expression *)
      match (eval_expr exp1 env, eval_expr exp2 env) with
      | Some (VALUE (INT e1)), Some (VALUE (INT e2)) -> (
          match op with
          | ADD -> Some (VALUE (INT (e1 + e2)))
          | SUBTRACT -> Some (VALUE (INT (e1 - e2)))
          | MULTIPLY -> Some (VALUE (INT (e1 * e2)))
          | DIVIDE -> Some (VALUE (INT (e1 / e2)))
          | MOD -> Some (VALUE (INT (e1 mod e2)))
          | POW -> Some (VALUE (INT (Utils.pow e1 e2))))
      | _, _ -> raise Semantic_check.Internal_error)
  | COND ce -> Some (VALUE (BOOL (eval_cond_expr ce env)))

and eval_ret_expr ((ret_expr, pos) : Ast.ret_expr)
    (env : Semantic_check.scope_vars) : eval_result option =
  match ret_expr with
  | EXPR e | SOME_EXPR e -> eval_expr e env
  | NONE -> Some RETURN_NONE

and process_stmt (stmt : Ast.stmt) (env : Semantic_check.scope_vars)
    (already_processed : bool) : eval_result option =
  match stmt with
  | VARDECL vd -> (
      match vd with
      (* store all the vars declared *)
      | DECL (d, _) ->
          if already_processed then None
          else
            let decl_mut, decl_type = (d.mut, d.dtype) in
            (* store all the variables declared *)
            List.iter
              (fun var_name ->
                Hashtbl.replace env var_name
                  { mut = decl_mut; dtype = decl_type; assigned_val = NONE })
              d.vars;
            None
      | DECL_ASST (da, _) -> (
          (* store var *)
          match da.asst with
          | at, _ -> (
              (* evaluate expr assigned to var & update env *)
              match eval_expr at.expr env with
              | Some (VALUE v) ->
                  Hashtbl.replace env at.name
                    { mut = da.mut; dtype = da.dtype; assigned_val = VAL v };
                  None
              | _ -> raise Semantic_check.Internal_error)))
  | ASST (at, _) -> (
      let varname = at.name in
      let entry = Hashtbl.find env varname in
      (* evaluate expr assigned to var & update env *)
      match eval_expr at.expr env with
      | Some (VALUE v) ->
          Hashtbl.replace env at.name
            { mut = entry.mut; dtype = entry.dtype; assigned_val = VAL v };
          None
      | _ -> raise Semantic_check.Internal_error)
  | LOOP lp -> process_loop lp env already_processed
  | IF (ce, blk) ->
      (* evaluate condition *)
      let cond = eval_cond_expr ce env in
      (* create new scope for block *)
      let block_scope = Hashtbl.copy env in
      (* if condition is true process block *)
      if cond then process_block blk block_scope false (SCOPE env) else None
  | IF_ELSE ((ce, if_blk), else_blk) ->
      (* evaluate condition *)
      let cond = eval_cond_expr ce env in
      (* create new scope for block *)
      let if_scope, else_scope = (Hashtbl.copy env, Hashtbl.copy env) in
      (* if condition is true process if_blk, otherwise process else_blk *)
      if cond then process_block if_blk if_scope false (SCOPE env)
      else process_block else_blk else_scope false (SCOPE env)
  | LET (({ var; func_call }, if_blk), else_blk) -> (
      match func_call with
      | fc, pos -> (
          match eval_func_call func_call env with
          (* evaluate function call *)
          | Some (VALUE v) -> (
              (* function return some value, and is of optional type; thus, we process if block *)
              let fname = fc.func_name in
              let fentry = Hashtbl.find Semantic_check.func_decls fname in
              let ret_type = match fentry.ret_type with rt, pos -> rt in

              (* add var to if block scope *)
              match ret_type with
              | DATATYPE dt ->
                  let env_with_var = Hashtbl.copy env in
                  Hashtbl.replace env_with_var var
                    { mut = false; dtype = dt; assigned_val = VAL v };
                  (* if ret expr some, process if. else else *)
                  process_block if_blk env_with_var false (SCOPE env)
              | VOID -> raise Semantic_check.Internal_error)
          | _ -> process_block else_blk (Hashtbl.copy env) false (SCOPE env)))
  | RETURN ret_expr -> None
  | EXP expr ->
      (* execute expression to introduce intended side-effects *)
      ignore (eval_expr expr env);
      None
  | EMPTY -> None

and process_block ((blk, pos) : Ast.block)
    (block_scope : Semantic_check.scope_vars) (already_processed : bool)
    (parent_scope : parent_scope) : eval_result option =
  (* collect statements in block *)
  let stmts = match blk with STMTS st -> st in

  (* process each statement in order *)
  let rec process_func_body statements =
    match statements with
    (* if return statement found, evaluate return expression & return it *)
    | Ast.RETURN ret_expr :: st -> eval_ret_expr ret_expr block_scope
    | s :: st -> (
        (* process statement *)
        match process_stmt s block_scope already_processed with
        | Some ret_val -> Some ret_val (* found a return value *)
        | None -> process_func_body st (* continue executing statements *))
    | [] -> (
        (* block has been processed; propagate mutated variables to outer scope where applicable *)
        match parent_scope with
        | SCOPE s ->
            ignore (update_parent_scope block_scope s);
            None
        | NONE -> None)
  in
  process_func_body stmts

and update_parent_scope (child_scope : Semantic_check.scope_vars)
    (parent_scope : Semantic_check.scope_vars) : unit =
  (* propagate mutated variables in child scope to parent scope *)

  (* iterate through all variable declarations in child scope *)
  Hashtbl.iter
    (fun var_name (child_entry : Semantic_check.var_details) ->
      (* check if variable also exists in parent scope *)
      let parent_entry = Hashtbl.find_opt parent_scope var_name in
      match parent_entry with
      | Some pe -> (
          match (pe.assigned_val, child_entry.assigned_val) with
          | _, VAL v ->
              (* update value of variable in parent scope *)
              Hashtbl.replace parent_scope var_name
                { pe with assigned_val = VAL v }
          | _ -> ())
      | _ -> ())
    child_scope

and process_loop ((loop, pos) : Ast.loop)
    (outer_env : Semantic_check.scope_vars) (already_processed : bool) :
    eval_result option =
  match loop with
  | WHILE { cond; body } ->
      let rec process_while (c : Ast.cond_expr) (b : Ast.block)
          (scope : Semantic_check.scope_vars) : eval_result option =
        (* evaluate condition *)
        if eval_cond_expr c scope then
          (* if condition is true, then process while body *)
          match process_block b scope already_processed NONE with
          (* found return value so return it *)
          | Some v -> Some v
          (* repeat *)
          | None -> process_while c b scope
        (* condition is false, so return *)
          else None
      in
      process_while cond body outer_env
  | FOR { init; cond; update; body } ->
      (* process initialization *)
      ignore
        (process_stmt (Ast.VARDECL (Ast.DECL_ASST init)) outer_env
           already_processed);

      let rec process_for (c : Ast.cond_expr) (u : Ast.var_update)
          (b : Ast.block) (scope : Semantic_check.scope_vars) :
          eval_result option =
        (* check cond *)
        if eval_cond_expr c scope then (
          (* process body *)
          (* if cond is true, process body & update *)
          match process_block b scope already_processed NONE with
          | Some v -> Some v (* found return value so return it *)
          | None ->
              (* apply update *)
              ignore
                (match u with
                | INC i, p -> eval_expr (Ast.INCREMENT i, p) scope
                | UPDATE_ASST at, _ ->
                    process_stmt (Ast.ASST at) scope already_processed);
              (* repeat *)
              process_for c u b scope)
        else None
      in
      process_for cond update body outer_env

and eval_func_call ((func_call, pos) : Ast.func_call)
    (caller_env : Semantic_check.scope_vars) : eval_result option =
  let func_entry = Hashtbl.find Semantic_check.func_decls func_call.func_name in
  (* create copy of scope so evaluated values are dropped when we go out of scope *)
  let scope = Hashtbl.copy func_entry.env in

  (* evaluate arguments *)
  List.iter2
    (fun (param_dtype, param_name) asst_expr ->
      match eval_expr asst_expr caller_env with
      | Some (VALUE v) ->
          Hashtbl.replace scope param_name
            {
              mut = true;
              (* parameter can be modified *)
              dtype = param_dtype;
              assigned_val = VAL v;
            }
      | _ -> raise Semantic_check.Internal_error)
    func_entry.params func_call.args;

  (* process function body by executing each statement in it *)
  process_block func_entry.body scope true NONE

let eval_program (prog : Ast.prog) : Semantic_check.eval_value option =
  (* find main function *)
  let main_func = Ast.IDENT "main" in
  let main_func_call : Ast.func_call =
    ({ func_name = main_func; args = [] }, 0)
  in
  match Hashtbl.find_opt Semantic_check.func_decls main_func with
  | Some entry -> (
      match eval_func_call main_func_call entry.env with
      | Some (VALUE v) -> Some v
      | _ -> None)
  | None -> None
