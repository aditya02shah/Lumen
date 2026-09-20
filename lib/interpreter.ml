(* --EXCEPTIONS-- *)
exception Internal_error (* should have been handled by the checker *)
exception Negative_exponent (* negative exponents *)

(* --INTERPRETER IMPLEMENTATION-- *)
let rec pow (base : int) (exp : int) : int =
  (* simple power function *)
  if exp < 0 then raise Negative_exponent
  else if exp = 0 then 1
  else base * pow base (exp - 1)

let rec eval_cond_expr (cond_expr : Ast.cond_expr) : bool =
  (* evaluate conditional expression *)
  match cond_expr with
  | BOOL_VAL (bv, _) -> (
      match bv with
      | BOOL b -> b
      | VAL_CMP (op, val1, val2) -> (
          (* compare values *)
          match op with
          | LESS_THAN -> val1 < val2
          | LESS_THAN_EQUALS -> val1 <= val2
          | GREATER_THAN -> val1 > val2
          | GREATER_THAN_EQUALS -> val1 >= val2
          | EQUALS -> val1 = val2
          | NOT_EQUALS -> val1 <> val2))
  | COMBINATION ((op, ce1, ce2), _) -> (
      (* apply combination *)
      match (op, eval_cond_expr ce1, eval_cond_expr ce2) with
      | LOGICAL_AND, true, true -> true
      | LOGICAL_AND, _, _ -> false
      | LOGICAL_OR, false, false -> false
      | LOGICAL_OR, _, _ -> true)

let rec eval_expr ((expr, pos) : Ast.expr) (env : Semantic_check.scope_vars) :
    Semantic_check.eval_value option =
  (* evaluate expression *)
  match expr with
  | INT n -> Some (INT n)
  | VAR x -> (
      (* find var entry in env *)
      match (Hashtbl.find env x).assigned_val with
      | EXPR e -> eval_expr e env (* evaluate expr assigned to var *)
      | VAL v -> Some v (* var value has already been evaluated *)
      | IS_PARAM -> raise Internal_error
      | NONE -> raise Internal_error)
  | INCREMENT x -> (
      (* find var entry in env *)
      match (Hashtbl.find env x).assigned_val with
      | EXPR e -> (
          (* evaluate expr assigned to var & increment it *)
          match eval_expr e env with
          | Some (INT i) -> Some (INT (i + 1))
          | Some (BOOL _) -> raise Internal_error
          | None -> raise Internal_error)
      | VAL v -> (
          (* var value has already been evaluated; increment it *)
          match v with
          | INT i -> Some (INT (i + 1))
          | BOOL _ -> raise Internal_error)
      | IS_PARAM -> raise Internal_error
      | NONE -> raise Internal_error)
  | FUNC_CALL f -> eval_func_call f
  | BINARY (op, exp1, exp2) -> (
      (* evaluate arithmetic expression *)
      match (eval_expr exp1 env, eval_expr exp2 env) with
      | Some (INT e1), Some (INT e2) -> (
          match op with
          | ADD -> Some (INT (e1 + e2))
          | SUBTRACT -> Some (INT (e1 - e2))
          | MULTIPLY -> Some (INT (e1 * e2))
          | DIVIDE -> Some (INT (e1 / e2))
          | MOD -> Some (INT (e1 mod e2))
          | POW -> Some (INT (pow e1 e2)))
      | _, _ -> raise Internal_error)
  | COND ce -> Some (BOOL (eval_cond_expr ce))

and eval_ret_expr ((ret_expr, pos) : Ast.ret_expr)
    (env : Semantic_check.scope_vars) : Semantic_check.eval_value option =
  match ret_expr with
  | EXPR e -> eval_expr e env
  | SOME_EXPR e -> eval_expr e env (* TODO: *)
  | NONE -> raise Internal_error
(* TODO: *)

and process_stmt (stmt : Ast.stmt) (env : Semantic_check.scope_vars) : unit =
  match stmt with
  | VARDECL vd -> (
      match vd with
      (* store all the vars declared *)
      | DECL (d, _) -> ()
      | DECL_ASST (da, _) -> (
          (* store var *)
          match da.asst with
          | at, _ -> (
              let varname = at.name in
              let entry = Hashtbl.find env varname in
              (* evaluate expr assigned to var & update env *)
              match eval_expr at.expr env with
              | Some v ->
                  Hashtbl.replace env at.name
                    {
                      mut = entry.mut;
                      dtype = entry.dtype;
                      assigned_val = VAL v;
                    };
                  ()
              | None -> raise Internal_error)))
  | ASST (at, _) -> (
      let varname = at.name in
      let entry = Hashtbl.find env varname in
      (* evaluate expr assigned to var & update env *)
      match entry.assigned_val with
      | EXPR e -> (
          match eval_expr e env with
          | Some v ->
              Hashtbl.replace env at.name
                { mut = entry.mut; dtype = entry.dtype; assigned_val = VAL v };
              ()
          | None -> raise Internal_error)
      | VAL _ -> () (* expr has already been evaluated *)
      | _ -> raise Internal_error)
  | LOOP lp -> ()
  | IF (ce, blk) -> ()
  | IF_ELSE ((ce, if_blk), else_blk) -> ()
  | RETURN ret_expr -> ()
  | EXP expr ->
      (* execute expression to introduce intended side-effects *)
      ignore (eval_expr expr env);
      ()
  | EMPTY -> ()

and eval_func_call ((func_call, pos) : Ast.func_call) :
    Semantic_check.eval_value option =
  let func_entry = Hashtbl.find Semantic_check.func_decls func_call.func_name in

  let stmts = match func_entry.body with STMTS st, _ -> st in

  (* process function body by executing each statement in it *)
  let rec process_func_body statements =
    match statements with
    (* if return statement found, evaluate return expression *)
    | Ast.RETURN ret_expr :: st -> eval_ret_expr ret_expr func_entry.env
    | s :: st ->
        (* otherwise process statement *)
        process_stmt s func_entry.env;
        (* move on to the other statements *)
        process_func_body st
    | [] -> None
  in
  process_func_body stmts
