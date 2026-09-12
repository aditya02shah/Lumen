type semantic_errors = string list
type asst_val = NONE | EXPR of Parser.expr
type var_details = { mut : bool; dtype : Lexer.dtype; assigned_val : asst_val }

type func_details = {
  is_optional_type : bool;
  ret_type : Parser.ret_type;
  params : Parser.param_list;
}

let var_env = Hashtbl.create 100 (* maps var -> var_details *)
let func_env = Hashtbl.create 100 (* maps func -> func_details *)

(* and func_call = { func_name : ident; args : arg_list } *)

let rec check_cond_expr (cond_expr : Parser.cond_expr) : semantic_errors =
  match cond_expr with
  | BOOL_VAL (VAL_CMP (op, val1, val2)) -> (
      (* condition: logical_compare only supports integer/integer variable operands *)
      match (val1, val2) with
      | NUMBER n1, NUMBER n2 -> []
      | NUMBER n, IDENTIFIER var | IDENTIFIER var, NUMBER n -> (
          (* check whether var is declared *)
          match Hashtbl.find_opt var_env var with
          | Some ent -> (
              (* check whether var is assigned a value *)
              match ent.assigned_val with
              | NONE ->
                  [ "Conditional Expression compares unassigned variables!" ]
              | _ -> (
                  match ent.dtype with
                  | INT -> []
                  | _ -> [ "Conditional Expression requires integer operands" ])
              )
          | _ -> [ "Conditional Expression compares undefined variable!" ])
      | IDENTIFIER var1, IDENTIFIER var2 -> (
          (* condition: both variables should be defined and assigned a value *)

          (* check whether the variables have been defined *)
          match
            (Hashtbl.find_opt var_env var1, Hashtbl.find_opt var_env var2)
          with
          | Some ent1, Some ent2 -> (
              (* check whether the variables have been assigned values *)
              match (ent1.assigned_val, ent2.assigned_val) with
              | EXPR e1, EXPR e2 -> (
                  (* condition: both variables should have matching types *)
                  match (ent1.dtype, ent2.dtype) with
                  | BOOL, BOOL -> []
                  | INT, INT -> []
                  | _, _ ->
                      [
                        "Conditional Expression cannot compare values of \
                         different types!";
                      ])
              | _ -> [ "Conditional Expression compares unassigned variables!" ]
              )
          | _ -> [ "Conditional Expression compares undefined variables!" ])
      | _ -> [ "Conditional Expression requires integer operands" ])
  | COMBINATION (cm, ce1, ce2) -> check_cond_expr ce1 @ check_cond_expr ce2
  | _ -> []

(* check whether undeclared vars being used *)
let rec check_expr (expr : Parser.expr) : semantic_errors =
  match expr with
  | VAR var -> (
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt var_env var with
      | Some ent -> (
          match ent.assigned_val with
          | EXPR e -> []
          | NONE -> [ "Expression has an unassigned variable!" ])
      | _ -> [ "Expression has an undefined variable!" ])
  (* check increment *)
  | INCREMENT var -> (
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt var_env var with
      | Some ent -> (
          match ent.assigned_val with
          (* condition: var must be an integer variable *)
          | EXPR e -> (
              match ent.dtype with
              | INT -> []
              | _ -> [ "Increment operation requires an integer variable!" ])
          | NONE -> [ "Expression has an unassigned variable!" ])
      | _ -> [ "Expression has an undefined variable!" ])
  (* check conditional expression *)
  | COND ce -> check_cond_expr ce
  (* check function call *)
  | FUNC_CALL fc -> check_func_call fc
  (* check arithmetic expression *)
  | BINARY (arithmetic_op, exp1, exp2) -> (
      match (exp1, exp2) with
      | VAR var1, VAR var2 -> (
          (* condition: vars must be defined and assigned a value *)
          match
            (Hashtbl.find_opt var_env var1, Hashtbl.find_opt var_env var2)
          with
          | Some ent1, Some ent2 -> (
              (* check whether the variables have been assigned values *)
              match (ent1.assigned_val, ent2.assigned_val) with
              | EXPR e1, EXPR e2 -> (
                  (* condition: both variables should have matching types *)
                  (* condition: variable operands cannot be boolean *)
                  match (ent1.dtype, ent2.dtype) with
                  | INT, INT -> []
                  | _, _ ->
                      [
                        "Arithmetic Expression cannot compare non-boolean \
                         types & variables of different types !";
                      ])
              | _ -> [ "Expression compares unassigned variables!" ])
          | _, _ -> [ "Expression compares undefined variables!" ])
      | VAR var, e | e, VAR var -> (
          (* check whether var is declared *)
          match Hashtbl.find_opt var_env var with
          | Some ent -> (
              (* check whether var is assigned a value *)
              match ent.assigned_val with
              | EXPR e -> (
                  match ent.dtype with
                  | INT -> check_expr e
                  | _ -> "Expression requires integer operands" :: check_expr e)
              | NONE ->
                  "Expression compares unassigned variables!" :: check_expr e)
          | _ -> "Expression compares undefined variable!" :: check_expr e)
      (* check func_call *)
      | FUNC_CALL f, e | e, FUNC_CALL f -> (
          (* condition: function used in arithmetic expression should return an integer *)

          (* check return type of function *)
          match Hashtbl.find_opt func_env f.func_name with
          | Some ent -> (
              match ent.ret_type with
              | DATATYPE INT -> []
              | _ ->
                  "Only function of return type INT can be used as \
                   subexpression" :: check_expr e)
          | _ -> "Undefined function used in expression! " :: check_expr e)
      | COND ce, _ | _, COND ce ->
          [
            "Arithmetic operations cannot be performed on conditional \
             expressions!";
          ]
      (* check both operands *)
      | _ -> check_expr exp1 @ check_expr exp2)
  | _ -> []

and check_func_call (func_call : Parser.func_call) : semantic_errors =
  (* check if function has been defined *)
  match Hashtbl.find_opt func_env func_call.func_name with
  | Some ent -> (
      (* condition: check each expression in list of arguments *)
      let errs =
        List.fold_left (fun acc exp -> acc @ check_expr exp) [] func_call.args
      in
      match errs with
      (* compare parameters & arguments *)
      | [] -> compare_params_and_args ent.params func_call.args
      | errors -> errors)
  | None -> [ "Call to undefined function!" ]

and compare_params_and_args (params : Parser.param_list)
    (args : Parser.arg_list) : semantic_errors =
  (* helper function which compares parameters & arguments *)
  (* Note: each argument should already be checked *)
  match (params, args) with
  | (param_type, _) :: rem_param, arg :: rem_args -> (
      (* condition: argument type must match parameter type *)
      match (param_type, get_expr_type arg) with
      | INT, Some INT -> []
      | BOOL, Some BOOL -> []
      | _, _ -> [ "Argument type doesn't match parameter type!" ])
  | [], [] -> []
  (* condition: argument count must match parameter count *)
  | _ -> [ "Argument count must match parameter count in function call!" ]

and get_expr_type (expr : Parser.expr) : Lexer.dtype option =
  (* NOTE: assumes expr has already been checked *)
  match expr with
  | INT _ -> Some INT
  | VAR v -> (
      match Hashtbl.find_opt var_env v with
      | Some ent -> Some ent.dtype
      | None -> None)
  | INCREMENT x -> Some INT
  | FUNC_CALL f -> (
      match Hashtbl.find_opt func_env f.func_name with
      | Some ent -> (
          match ent.ret_type with DATATYPE d -> Some d | VOID -> None)
      | None -> None)
  | BINARY (_, _, _) -> Some INT
  | COND _ -> Some BOOL
  | _ -> Some INT

let check_asst (asst : Parser.asst) : semantic_errors =
  (* condition: check if var is defined *)
  match Hashtbl.find_opt var_env asst.name with
  | Some ent -> (
      (* condition: check if var is mutable *)
      match ent.mut with
      | true -> (
          (* check expr being assigned *)
          match check_expr asst.expr with
          | [] -> (
              match (ent.dtype, asst.expr) with
              | INT, INT _ -> []
              | INT, INCREMENT v -> []
              | BOOL, COND ce -> []
              | INT, BINARY (op, exp1, exp2) -> []
              | _, VAR v -> (
                  (* condition: variable types should match *)
                  match Hashtbl.find_opt var_env v with
                  | Some varent -> (
                      match (ent.dtype, varent.dtype) with
                      | INT, INT -> []
                      | BOOL, BOOL -> []
                      | _, _ ->
                          [ "Cannot assign to variable of different type!" ])
                  | None -> [])
              | _, FUNC_CALL f -> (
                  (* condition: variable type and function type should match *)
                  match Hashtbl.find_opt func_env f.func_name with
                  | Some fent -> (
                      match (ent.dtype, fent.ret_type) with
                      | INT, DATATYPE INT -> []
                      | BOOL, DATATYPE BOOL -> []
                      | _, _ ->
                          [ "Cannot assign to function of different type!" ])
                  | None -> [])
              | _ -> [ "Assigning variable to invalid expression!" ])
          | errs -> "Assigning to invalid expression!" :: errs)
      | false -> [ "Cannot assigned to immutable variable" ])
  | _ -> [ "Cannot assigned to undefined variable" ]

let check_decl_asst (decl_asst : Parser.decl_asst) : semantic_errors =
  (* store declaration *)
  Hashtbl.replace var_env decl_asst.asst.name
    {
      mut = decl_asst.mut;
      dtype = decl_asst.dtype;
      assigned_val = EXPR decl_asst.asst.expr;
    };
  (* check asst *)
  check_asst decl_asst.asst

let check_var_decl (var_decl : Parser.var_decl) : semantic_errors =
  match var_decl with
  | DECL d ->
      (* store declaration details for each variable declared *)
      let decl_mut, decl_type = (d.mut, d.dtype) in
      List.iter
        (fun var_name ->
          Hashtbl.replace var_env var_name
            { mut = decl_mut; dtype = decl_type; assigned_val = NONE })
        d.vars;
      [] (* return [] since no errors found *)
  | DECL_ASST da -> check_decl_asst da

let rec check_loop (loop : Parser.loop) : semantic_errors =
  match loop with
  (* check while loop *)
  | WHILE while_lp -> check_cond_expr while_lp.cond @ check_block while_lp.body
  (* check for loop *)
  | FOR for_lp ->
      let init =
        match for_lp.init with
        | ASST at -> check_asst at
        | DECL_ASST da -> check_decl_asst da
      in
      init
      @ check_cond_expr for_lp.cond
      @ check_expr for_lp.update @ check_block for_lp.body

and check_stmt (stmt : Parser.stmt) : semantic_errors =
  (* check stmt *)
  match stmt with
  | VARDECL vd -> check_var_decl vd
  | ASST at -> check_asst at
  | LOOP lp -> check_loop lp
  | IF blk -> check_block blk
  | IF_ELSE (if_blk, else_blk) -> check_block if_blk @ check_block else_blk
  | RETURN ret_expr -> (
      match ret_expr with EXPR e | SOME_EXPR e -> check_expr e | NONE -> [])
  | EXP e -> check_expr e
  | EMPTY -> []

and check_block (block : Parser.block) : semantic_errors =
  (* check each statement in block *)
  match block with
  | STMTS stmts -> List.fold_left (fun acc st -> acc @ check_stmt st) [] stmts

let check_ret_expr (ret_expr : Parser.ret_expr) : semantic_errors =
  match ret_expr with EXPR e | SOME_EXPR e -> check_expr e | NONE -> []

let get_ret_expr_in_blk (func_blk : Parser.block) : Parser.ret_expr list =
  (* returns all the return expressions present in a block *)
  (* Note: excludes return expression sub-blocks *)
  match func_blk with
  | STMTS stmts ->
      List.fold_left
        (fun acc st -> match st with Parser.RETURN re -> re :: acc | _ -> acc)
        [] stmts

let rec check_func_block (block : Parser.block) : semantic_errors =
  (* check that each block should contain a maximum of 1 return expression *)
  let rec check_sub_blocks (stmts : Parser.stmt list) : semantic_errors =
    match stmts with
    | [] -> []
    | s :: st -> (
        match s with
        | IF blk ->
            (* check sub_block *)
            check_func_block blk @ check_sub_blocks st
        | IF_ELSE (if_blk, else_blk) ->
            (* check both sub-blocks *)
            check_func_block if_blk @ check_func_block else_blk
        | _ -> check_sub_blocks st)
  in
  match get_ret_expr_in_blk block with
  (* a block can contain upto 1 return expressions *)
  | [] -> ( match block with STMTS stmt -> check_sub_blocks stmt)
  | re :: [] -> ( match block with STMTS stmt -> check_sub_blocks stmt)
  | _ ->
      [
        "each block inside function cannot contain more than 1 return \
         expression ";
      ]

let check_func_decl (func : Parser.func_decl) : semantic_errors =
  (* condition: each block should contain a maximum of 1 return expression (not including sub-blocks) *)
  match check_func_block func.body with
  | [] -> (
      (* condition: void function should not contain return expression *)
      let ret_stmts = get_ret_expr_in_blk func.body in
      match func.return_type with
      | DATATYPE d -> (
          match ret_stmts with
          | re :: [] -> (
              (* check return expression *)
              match check_ret_expr re with
              | [] -> (
                  (* condition: return type of function should match the type of the return expression *)
                  match (re, func.is_optional_type) with
                  (* condition: optional type return functions should use Some & None *)
                  | EXPR e, false | SOME_EXPR e, true -> (
                      match (d, get_expr_type e) with
                      | INT, Some INT ->
                          Hashtbl.replace func_env func.func_name
                            {
                              is_optional_type = func.is_optional_type;
                              ret_type = func.return_type;
                              params = func.params;
                            };
                          []
                      | BOOL, Some BOOL ->
                          Hashtbl.replace func_env func.func_name
                            {
                              is_optional_type = func.is_optional_type;
                              ret_type = func.return_type;
                              params = func.params;
                            };
                          []
                      | _, _ ->
                          [ "Function contains invalid return expression!" ])
                  | NONE, true -> []
                  | _ ->
                      [
                        "Cannot use optional type return expressions in \
                         non-optional function declarations";
                      ])
              | errs -> errs)
          | _ ->
              [
                "Non-Void function contains multiple return expressions in the \
                 same block!";
              ])
      | VOID -> (
          match ret_stmts with
          | [] -> []
          | _ -> [ "Void function contains return expressions!" ]))
  | err -> err

let rec check_prog (program : Parser.prog) : semantic_errors =
  (* check all the functions in the program *)
  match program with
  | [] -> []
  | func :: rem_funcs ->
      List.fold_left
        (fun acc fn_decl -> acc @ check_func_decl fn_decl)
        [] (func :: rem_funcs)
