(* --TYPE DECLARATIONS-- *)
type semantic_errors = string list

(* Variables *)
type asst_val = NONE | EXPR of Parser.expr
type var_details = { mut : bool; dtype : Lexer.dtype; assigned_val : asst_val }

(* Functions *)
type func_details = {
  is_optional_type : bool;
  ret_type : Parser.ret_type;
  params : Parser.param_list;
}

(* --GLOBAL VARS-- *)
let var_env = Hashtbl.create 100 (* maps var -> var_details *)
let func_env = Hashtbl.create 100 (* maps func -> func_details *)

(* --HELPER FUNCTIONS-- *)
let format_msg (pos : Lexer.position) (msg : string) : string =
  "Line (" ^ string_of_int pos ^ ") - " ^ msg

(* --SEMANTIC CHECKER IMPLEMENTATION-- *)
(* Variables *)
(* variable declaration *)
let rec check_var_decl (var_decl : Parser.var_decl) : semantic_errors =
  match var_decl with
  | DECL (d, _) ->
      (* store variable declaration *)
      let decl_mut, decl_type = (d.mut, d.dtype) in
      List.iter
        (fun var_name ->
          Hashtbl.replace var_env var_name
            { mut = decl_mut; dtype = decl_type; assigned_val = NONE })
        d.vars;
      []
  | DECL_ASST da -> check_decl_asst da

and check_decl_asst (decl_asst : Parser.decl_asst) : semantic_errors =
  (* store declaration *)
  match decl_asst with
  | da, _ -> (
      match da.asst with
      | at, _ ->
          Hashtbl.replace var_env at.name
            { mut = da.mut; dtype = da.dtype; assigned_val = EXPR at.expr };
          (* check asst *)
          check_asst da.asst true)

(* variable assignment *)
and check_asst (asst : Parser.asst) (is_decl_asst : bool) : semantic_errors =
  (* condition: var must be defined *)
  match asst with
  | at, pos -> (
      match Hashtbl.find_opt var_env at.name with
      | Some ent ->
          if
            (* condition: var must be mutable *)
            is_decl_asst = true || ent.mut = true
          then
            (* check the expr variable is being assigned to *)
            match check_expr at.expr with
            | [] -> (
                match (ent.dtype, get_expr_type at.expr) with
                | INT, Some INT -> []
                | BOOL, Some BOOL -> []
                | _ ->
                    [
                      format_msg pos
                        "Assignment: variable type and expression type do not \
                         match!";
                    ])
            | errs ->
                format_msg pos "Assignment: contains invalid expression!"
                :: errs
          else
            [
              format_msg pos
                "Assignment: cannot assign value to immutable variable!";
            ]
      | _ -> [ format_msg pos "Assignment: contains undefined variable!" ])

(* Expression *)
and check_expr (expr : Parser.expr) : semantic_errors =
  match expr with
  | VAR var, pos -> (
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt var_env var with
      | Some ent -> (
          match ent.assigned_val with
          | EXPR e -> []
          | NONE ->
              [ format_msg pos "Expression: contains unassigned variable!" ])
      | _ -> [ "Expression: contains undefined variable!" ])
  (* check increment *)
  | INCREMENT var, pos -> (
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt var_env var with
      | Some ent -> (
          (* condition: var must be mutable *)
          match ent.mut with
          | true -> (
              match ent.assigned_val with
              (* condition: var must be an integer variable *)
              | EXPR e -> (
                  match ent.dtype with
                  | INT -> []
                  | _ ->
                      [
                        format_msg pos
                          "Expression: Increment operation requires an integer \
                           variable!";
                      ])
              | NONE ->
                  [ format_msg pos "Expression: contains unassigned variable!" ]
              )
          | false ->
              [
                format_msg pos
                  "Expression: cannot increment immutable variable!";
              ])
      | _ -> [ format_msg pos "Expression: contains undefined variable!" ])
  (* check conditional expression *)
  | COND ce, _ -> check_cond_expr ce
  (* check function call *)
  | FUNC_CALL fc, _ -> check_func_call fc
  (* check arithmetic expression *)
  | BINARY (arithmetic_op, exp1, exp2), pos -> (
      match (exp1, exp2) with
      | (VAR var1, _), (VAR var2, _) -> (
          (* condition: variables must be defined and assigned a value *)
          match
            (Hashtbl.find_opt var_env var1, Hashtbl.find_opt var_env var2)
          with
          | Some ent1, Some ent2 -> (
              (* check whether the variables have been assigned values *)
              match (ent1.assigned_val, ent2.assigned_val) with
              | EXPR e1, EXPR e2 -> (
                  (* condition: both the variables must have matching types *)
                  (* condition: the variable operands must not be boolean *)
                  match (ent1.dtype, ent2.dtype) with
                  | INT, INT -> []
                  | _, _ ->
                      [
                        format_msg pos
                          "Expression: arithmetic expression cannot compare \
                           non-boolean types or variables of different types !";
                      ])
              | _ ->
                  [
                    format_msg pos "Expression: contains unassigned variables!";
                  ])
          | _, _ ->
              [ format_msg pos "Expression: contains undefined variables!" ])
      | (VAR var, pos), e | e, (VAR var, pos) -> (
          (* check whether var is defined *)
          match Hashtbl.find_opt var_env var with
          | Some ent -> (
              (* check whether var is assigned a value *)
              match ent.assigned_val with
              | EXPR e -> (
                  match ent.dtype with
                  | INT -> check_expr e
                  | _ ->
                      format_msg pos
                        "Expression: arithmetic expressions requires integer \
                         operands!"
                      :: check_expr e)
              | NONE ->
                  format_msg pos "Expression: contains unassigned variables!"
                  :: check_expr e)
          | _ ->
              format_msg pos "Expression: contains undefined variable!"
              :: check_expr e)
      (* check func_call *)
      | (FUNC_CALL (f, _), pos), e | e, (FUNC_CALL (f, _), pos) -> (
          (* condition: only integer functions must be used in arithmetic expressions *)

          (* check return type of function *)
          match Hashtbl.find_opt func_env f.func_name with
          | Some ent -> (
              match ent.ret_type with
              | DATATYPE INT, _ -> []
              | _ ->
                  format_msg pos
                    "Expression: non-integer functions cannot be used in \
                     arithmetic expressions!"
                  :: check_expr e)
          | _ ->
              format_msg pos
                "Expression: undefined function used in expression! "
              :: check_expr e)
      | (COND ce, pos), _ | _, (COND ce, pos) ->
          [
            format_msg pos
              "Expression: arithmetic expressions cannot be performed on \
               conditional expressions!";
          ]
      (* check both operands *)
      | _ -> check_expr exp1 @ check_expr exp2)
  | _ -> []

(* conditional expression *)
and check_cond_expr (cond_expr : Parser.cond_expr) : semantic_errors =
  match cond_expr with
  | BOOL_VAL (VAL_CMP (op, val1, val2), pos) -> (
      (* condition: logical_compare operands must be integer literals/variables *)
      match (val1, val2) with
      | (NUMBER n1, _), (NUMBER n2, _) -> []
      | (NUMBER n, _), (IDENTIFIER var, _) | (IDENTIFIER var, _), (NUMBER n, _)
        -> (
          (* check whether var is declared *)
          match Hashtbl.find_opt var_env var with
          | Some ent -> (
              (* check whether var is assigned a value *)
              match ent.assigned_val with
              | NONE ->
                  [
                    format_msg pos
                      "Conditional Expression: comparing unassigned variables!";
                  ]
              | _ -> (
                  match ent.dtype with
                  | INT -> []
                  | _ ->
                      [
                        format_msg pos
                          "Conditional Expression: comparison requires integer \
                           operands!";
                      ]))
          | _ ->
              [
                format_msg pos
                  "Conditional Expression: comparing undefined variable!";
              ])
      | (IDENTIFIER var1, pos), (IDENTIFIER var2, _) -> (
          (* condition: both the variables must be defined and assigned a value *)
          match
            (Hashtbl.find_opt var_env var1, Hashtbl.find_opt var_env var2)
          with
          | Some ent1, Some ent2 -> (
              (* check whether the variables have been assigned values *)
              match (ent1.assigned_val, ent2.assigned_val) with
              | EXPR e1, EXPR e2 -> (
                  (* condition: both the variables must have matching types *)
                  match (ent1.dtype, ent2.dtype) with
                  | BOOL, BOOL -> []
                  | INT, INT -> []
                  | _, _ ->
                      [
                        format_msg pos
                          "Conditional Expression: cannot compare values of \
                           different types!";
                      ])
              | _ ->
                  [
                    format_msg pos
                      "Conditional Expression: comparing unassigned variables!";
                  ])
          | _ ->
              [
                format_msg pos
                  "Conditional Expression: comparing undefined variables!";
              ])
      | _ ->
          [
            format_msg pos
              "Conditional Expression: comparing undefined variables!";
          ])
  | COMBINATION ((cm, ce1, ce2), _) -> check_cond_expr ce1 @ check_cond_expr ce2
  | _ -> []

and get_expr_type (expr : Parser.expr) : Lexer.dtype option =
  (* returns the type for an expression if it can be determined, None otherwise *)
  (* NOTE: assumes expr has already been checked *)
  match expr with
  | INT _, _ -> Some INT
  | VAR v, _ -> (
      (* determine var type *)
      match Hashtbl.find_opt var_env v with
      | Some ent -> Some ent.dtype
      | None -> None)
  | INCREMENT x, _ -> Some INT
  | FUNC_CALL (f, _), _ -> (
      (* determine func return type *)
      match Hashtbl.find_opt func_env f.func_name with
      | Some ent -> (
          match ent.ret_type with DATATYPE d, _ -> Some d | VOID, _ -> None)
      | None -> None)
  | BINARY (_, _, _), _ -> Some INT
  | COND _, _ -> Some BOOL

and get_ret_expr_in_blk (func_blk : Parser.block) (include_sub_blocks : bool) :
    Parser.ret_expr list =
  (* returns all the return expressions present in a block *)
  if include_sub_blocks then
    (* check all sub-blocks *)
    match func_blk with
    | STMTS stmts, _ ->
        List.fold_left
          (fun acc st ->
            match st with
            | Parser.RETURN re -> acc @ [ re ]
            | Parser.IF blk -> acc @ get_ret_expr_in_blk blk true
            | Parser.IF_ELSE (if_blk, else_blk) ->
                (acc @ get_ret_expr_in_blk if_blk true)
                @ get_ret_expr_in_blk else_blk true
            | Parser.LOOP lp -> (
                match lp with
                | WHILE lp, _ -> acc @ get_ret_expr_in_blk lp.body true
                | FOR lp, _ -> acc @ get_ret_expr_in_blk lp.body true)
            | _ -> acc)
          [] stmts
  else
    (* dont check sub-blocks *)
    match func_blk with
    | STMTS stmts, _ ->
        List.rev
          (List.fold_left
             (fun acc st ->
               match st with Parser.RETURN re -> re :: acc | _ -> acc)
             [] stmts)

and check_ret_expr (ret_expr : Parser.ret_expr) : semantic_errors =
  (* checks expression present in return expression statement *)
  match ret_expr with
  | EXPR e, _ | SOME_EXPR e, _ -> check_expr e
  | NONE, _ -> []

(* Functions *)
(* function declaration *)
and check_func_decl (func_decl : Parser.func_decl) : semantic_errors =
  (* store func_decl *)
  match func_decl with
  | func, f_pos -> (
      Hashtbl.replace func_env func.func_name
        {
          is_optional_type = func.is_optional_type;
          ret_type = func.return_type;
          params = func.params;
        };

      (* condition: each block must contain a maximum of 1 return expression (not including sub-blocks) *)
      match check_func_block func.body with
      | [] -> (
          (* check block *)
          match check_block func.body with
          | [] -> (
              (* condition: void function must not contain return expression *)
              let ret_stmts = get_ret_expr_in_blk func.body true in
              match func.return_type with
              | DATATYPE d, _ ->
                  (* check each return expression and gather errors *)
                  List.flatten
                    (List.map
                       (fun re ->
                         (* check return expression *)
                         match check_ret_expr re with
                         | [] -> (
                             (* condition: return type of function must match return expression's type *)
                             (* condition: return expressions in optional functions must use "Some" & "None" *)
                             match (re, func.is_optional_type) with
                             | (EXPR e, e_pos), false
                             | (SOME_EXPR e, e_pos), true -> (
                                 match (d, get_expr_type e) with
                                 | INT, Some INT -> []
                                 | BOOL, Some BOOL -> []
                                 | _, _ ->
                                     [
                                       format_msg e_pos
                                         "Function: contains invalid return \
                                          expression!";
                                     ])
                             | (NONE, e_pos), true -> []
                             | (_, e_pos), _ ->
                                 [
                                   format_msg e_pos
                                     "Function: cannot use optional type \
                                      return expressions in non-optional \
                                      function declarations!";
                                 ])
                         | _ ->
                             [
                               format_msg f_pos
                                 "Function: non-void function contains \
                                  multiple return expressions in the same \
                                  block!";
                             ])
                       ret_stmts)
              | VOID, _ -> (
                  (* void function shouldn't contain return expressions *)
                  match ret_stmts with
                  | [] -> []
                  | _ ->
                      [
                        format_msg f_pos
                          "Function: void function cannot return value!";
                      ]))
          | es -> es)
      | err -> err)

(* function call *)
and check_func_call (func_call : Parser.func_call) : semantic_errors =
  match func_call with
  | func, pos -> (
      (* check if function has been defined *)
      match Hashtbl.find_opt func_env func.func_name with
      | Some ent -> (
          (* check each expression in the list of arguments *)
          let errs =
            List.fold_left (fun acc exp -> acc @ check_expr exp) [] func.args
          in
          match errs with
          (* compare parameters with arguments *)
          | [] -> compare_params_with_args ent.params func.args pos
          | errors -> errors)
      | None -> [ format_msg pos "Function Call: call to undefined function!" ])

and check_func_block (block : Parser.block) : semantic_errors =
  (* condition: each block should contain a maximum of 1 return expression *)
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
        | LOOP lp -> (
            (* check loop *)
            match lp with
            | WHILE lp, _ -> check_func_block lp.body
            | FOR lp, _ -> check_func_block lp.body)
        | _ -> check_sub_blocks st)
  in
  match block with
  | _, pos -> (
      match get_ret_expr_in_blk block false with
      (* a block can contain upto 1 return expressions *)
      | [] -> ( match block with STMTS stmt, _ -> check_sub_blocks stmt)
      | re :: [] -> ( match block with STMTS stmt, _ -> check_sub_blocks stmt)
      | _ ->
          [
            format_msg pos
              "Function: each block inside function cannot contain more than 1 \
               return expression ";
          ])

and compare_params_with_args (params : Parser.param_list)
    (args : Parser.arg_list) (pos : Lexer.position) : semantic_errors =
  (* helper function which compares parameters & arguments *)
  (* Note: each argument should already be checked *)
  match (params, args) with
  | (param_type, _) :: rem_param, arg :: rem_args -> (
      (* condition: argument type must match parameter type *)
      match (param_type, get_expr_type arg) with
      | INT, Some INT -> []
      | BOOL, Some BOOL -> []
      | _, _ ->
          [
            format_msg pos
              "Function Call: argument type doesn't match parameter type!";
          ])
  | [], [] -> []
  (* condition: argument count must match parameter count *)
  | _ ->
      [
        format_msg pos
          "Function Call: argument count must match parameter count in \
           function call!";
      ]

(* Statement *)
and check_stmt (stmt : Parser.stmt) : semantic_errors =
  (* check stmt *)
  match stmt with
  | VARDECL vd -> check_var_decl vd
  | ASST at -> check_asst at false
  | LOOP lp -> check_loop lp
  | IF blk -> check_block blk
  | IF_ELSE (if_blk, else_blk) -> check_block if_blk @ check_block else_blk
  | RETURN ret_expr -> (
      match ret_expr with
      | EXPR e, _ | SOME_EXPR e, _ -> check_expr e
      | NONE, _ -> [])
  | EXP e -> check_expr e
  | EMPTY -> []

(* Block *)
and check_block (block : Parser.block) : semantic_errors =
  (* check each statement in block *)
  match block with
  | STMTS stmts, _ ->
      List.fold_left (fun acc st -> acc @ check_stmt st) [] stmts

(* Loops *)
and check_loop (loop : Parser.loop) : semantic_errors =
  match loop with
  (* check while loop *)
  | WHILE while_lp, _ ->
      check_cond_expr while_lp.cond @ check_block while_lp.body
  (* check for loop *)
  | FOR for_lp, _ ->
      let init =
        match for_lp.init with
        | ASST at, _ -> check_asst at false
        | DECL_ASST da, _ -> check_decl_asst da
      in
      init
      @ check_cond_expr for_lp.cond
      @ check_expr for_lp.update @ check_block for_lp.body

(* --PUBLIC INTERFACE-- *)
(* Program *)
let rec check_prog (program : Parser.prog) : semantic_errors =
  (* check all the functions in the program *)
  match program with
  | [] -> []
  | func :: rem_funcs ->
      List.fold_left
        (fun acc fn_decl -> acc @ check_func_decl fn_decl)
        [] (func :: rem_funcs)
