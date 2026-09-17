(* --TYPE DECLARATIONS-- *)
type semantic_errors = string list

(* Variables *)
type asst_val = NONE | EXPR of Parser.expr
type var_details = { mut : bool; dtype : Lexer.dtype; assigned_val : asst_val }

(* Expressions *)
type expr_type = DTYPE of Lexer.dtype | OPTION_DTYPE of Lexer.dtype | VOID

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
let format_expr_type (etype : expr_type) =
  match etype with
  | DTYPE d -> Lexer.format_dtype d
  | OPTION_DTYPE d -> (
      match d with
      | INT -> "dtype('Option<Int>')"
      | BOOL -> "dtype('Option<Bool>')")
  | VOID -> "dtype('void')"

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
      let var_name = match at.name with IDENT vname -> vname in
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
                | INT, Some (DTYPE INT) -> []
                | BOOL, Some (DTYPE INT) -> []
                | dt1, Some dt2 ->
                    [
                      Utils.format_msg pos
                        (Printf.sprintf
                           "Assignment: variable %s and expression %s do not \
                            match!"
                           (Lexer.format_dtype dt1) (format_expr_type dt2));
                    ]
                | dt, None ->
                    [
                      Utils.format_msg pos
                        (Printf.sprintf
                           "Assignment: variable %s and expression \
                            dtype('none') do not match!"
                           (Lexer.format_dtype dt));
                    ])
            | expr_errs ->
                Utils.format_msg pos "Assignment: contains invalid expression!"
                :: expr_errs
          else
            [
              Utils.format_msg pos
                (Printf.sprintf
                   "Assignment: cannot assign value to immutable variable '%s'!"
                   var_name);
            ]
      | _ ->
          [
            Utils.format_msg pos
              (Printf.sprintf "Assignment: contains an undefined variable '%s'!"
                 var_name);
          ])

(* Expression *)

and check_expr (expr : Parser.expr) : semantic_errors =
  match expr with
  | VAR var, pos -> (
      let var_name = match var with IDENT vname -> vname in
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt var_env var with
      | Some ent -> (
          match ent.assigned_val with
          | EXPR e -> []
          | NONE ->
              [
                Utils.format_msg pos
                  (Printf.sprintf
                     "Expression: contains an unassigned variable '%s'!"
                     var_name);
              ])
      | _ ->
          [
            Utils.format_msg pos
              (Printf.sprintf "Expression: contains an undefined variable '%s'!"
                 var_name);
          ])
  (* check increment *)
  | INCREMENT var, pos -> (
      let var_name = match var with IDENT vname -> vname in
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
                        Utils.format_msg pos
                          (Printf.sprintf
                             "Expression: Increment operation requires integer \
                              variable. Variable '%s' has %s!"
                             var_name
                             (Lexer.format_dtype ent.dtype));
                      ])
              | NONE ->
                  [
                    Utils.format_msg pos
                      (Printf.sprintf
                         "Expression: contains an unassigned variable '%s'!"
                         var_name);
                  ])
          | false ->
              [
                Utils.format_msg pos
                  (Printf.sprintf
                     "Expression: cannot increment immutable variable '%s'!"
                     var_name);
              ])
      | _ ->
          [
            Utils.format_msg pos
              (Printf.sprintf "Expression: contains an undefined variable '%s'!"
                 var_name);
          ])
  (* check conditional expression *)
  | COND ce, _ -> check_cond_expr ce
  (* check function call *)
  | FUNC_CALL fc, _ -> check_func_call fc
  (* check arithmetic expression *)
  | BINARY (arithmetic_op, exp1, exp2), pos -> (
      match (exp1, exp2) with
      | (VAR var1, _), (VAR var2, _) ->
          check_binary_operation var1 var2 "Expression: arithmetic expression"
            pos
      | (VAR var, pos), e | e, (VAR var, pos) -> (
          let var_name = match var with IDENT vname -> vname in
          (* check whether var is defined *)
          match Hashtbl.find_opt var_env var with
          | Some ent -> (
              (* check whether var is assigned a value *)
              match ent.assigned_val with
              | EXPR e -> (
                  match ent.dtype with
                  | INT -> check_expr e
                  | _ ->
                      Utils.format_msg pos
                        (Printf.sprintf
                           "Expression: arithmetic operation requires integer \
                            operands. Variable '%s' has %s!"
                           var_name
                           (Lexer.format_dtype ent.dtype))
                      :: check_expr e)
              | NONE ->
                  Utils.format_msg pos
                    (Printf.sprintf
                       "Expression: contains an unassigned variable '%s'!"
                       var_name)
                  :: check_expr e)
          | _ ->
              Utils.format_msg pos
                (Printf.sprintf
                   "Expression: contains an undefined variable '%s'!" var_name)
              :: check_expr e)
      (* check func_call *)
      | (FUNC_CALL (f, _), pos), e | e, (FUNC_CALL (f, _), pos) -> (
          (* condition: only integer functions must be used in arithmetic expressions *)
          let fname = match f.func_name with IDENT f -> f in
          (* check return type of function *)
          match Hashtbl.find_opt func_env f.func_name with
          | Some ent -> (
              match ent.ret_type with
              | DATATYPE INT, _ -> []
              | _ ->
                  Utils.format_msg pos
                    (Printf.sprintf
                       "Expression: non-integer function '%s' cannot be used \
                        in an arithmetic expression!"
                       fname)
                  :: check_expr e)
          | _ ->
              Utils.format_msg pos
                (Printf.sprintf
                   "Expression: contains an undefined function '%s'!" fname)
              :: check_expr e)
      | (COND ce, pos), _ | _, (COND ce, pos) ->
          [
            Utils.format_msg pos
              "Expression: arithmetic operation cannot be performed on \
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
          let var_name = match var with IDENT vname -> vname in
          (* check whether var is declared *)
          match Hashtbl.find_opt var_env var with
          | Some ent -> (
              (* check whether var is assigned a value *)
              match ent.assigned_val with
              | NONE ->
                  [
                    Utils.format_msg pos
                      (Printf.sprintf
                         "Conditional Expression: contains unassigned variable \
                          %s!"
                         var_name);
                  ]
              | _ -> (
                  match ent.dtype with
                  | INT -> []
                  | _ ->
                      [
                        Utils.format_msg pos
                          (Printf.sprintf
                             "Conditional Expression: comparison requires \
                              integer operands!. Variable '%s' has %s!"
                             var_name
                             (Lexer.format_dtype ent.dtype));
                      ]))
          | _ ->
              [
                Utils.format_msg pos
                  (Printf.sprintf
                     "Conditional Expression: contains undefined variable '%s'!"
                     var_name);
              ])
      | (IDENTIFIER var1, pos), (IDENTIFIER var2, _) ->
          check_binary_operation var1 var2 "Conditional Expression" pos)
  | COMBINATION ((cm, ce1, ce2), _) -> check_cond_expr ce1 @ check_cond_expr ce2
  | _ -> []

and check_binary_operation (var1 : Parser.ident) (var2 : Parser.ident)
    (msg_prefix : string) (pos : Lexer.position) : semantic_errors =
  let var_name1 = match var1 with IDENT vname -> vname in
  let var_name2 = match var2 with IDENT vname -> vname in
  (* condition: both the variables must be defined and assigned a value *)
  match (Hashtbl.find_opt var_env var1, Hashtbl.find_opt var_env var2) with
  | Some ent1, Some ent2 -> (
      (* check whether the variables have been assigned values *)
      match (ent1.assigned_val, ent2.assigned_val) with
      (* --valid path-- *)
      | EXPR e1, EXPR e2 -> (
          (* condition: both the variables must have matching types *)
          (* condition: the variable operands must not be boolean *)
          match (ent1.dtype, ent2.dtype) with
          | INT, INT -> []
          | _ ->
              [
                Utils.format_msg pos
                  (Printf.sprintf
                     "%s requires integer operands. Variable '%s' has '%s' and \
                      variable '%s' has '%s'!"
                     msg_prefix var_name1
                     (Lexer.format_dtype ent1.dtype)
                     var_name2
                     (Lexer.format_dtype ent2.dtype));
              ])
      (* --invalid paths-- *)
      | EXPR e, NONE ->
          [
            Utils.format_msg pos
              (Printf.sprintf "%s contains an unassigned variable '%s'!"
                 msg_prefix var_name2);
          ]
      | NONE, EXPR e ->
          [
            Utils.format_msg pos
              (Printf.sprintf "%s contains an unassigned variable '%s'!"
                 msg_prefix var_name1);
          ]
      | NONE, NONE ->
          [
            Utils.format_msg pos
              (Printf.sprintf "%s contains unassigned variables '%s' and '%s'!"
                 msg_prefix var_name1 var_name2);
          ])
  (* --invalid paths-- *)
  | Some ent, None ->
      [
        Utils.format_msg pos
          (Printf.sprintf "%s: contains an undefined variable '%s'!" msg_prefix
             var_name2);
      ]
  | None, Some ent ->
      [
        Utils.format_msg pos
          (Printf.sprintf "%s: contains an undefined variable '%s'!" msg_prefix
             var_name1);
      ]
  | None, None ->
      [
        Utils.format_msg pos
          (Printf.sprintf "%s: contains undefined variables '%s' and '%s'!"
             msg_prefix var_name1 var_name2);
      ]

and get_expr_type (expr : Parser.expr) : expr_type option =
  (* returns the type for an expression if it can be determined, None otherwise *)
  (* NOTE: assumes expr has already been checked *)
  match expr with
  | INT _, _ -> Some (DTYPE INT)
  | VAR v, _ -> (
      (* determine var type *)
      match Hashtbl.find_opt var_env v with
      | Some ent -> Some (DTYPE ent.dtype)
      | None -> None)
  | INCREMENT x, _ -> Some (DTYPE INT)
  | FUNC_CALL (f, _), _ -> (
      (* determine func return type *)
      match Hashtbl.find_opt func_env f.func_name with
      | Some ent -> (
          match (ent.ret_type, ent.is_optional_type) with
          | (DATATYPE d, _), false -> Some (DTYPE d)
          | (DATATYPE d, _), true -> Some (OPTION_DTYPE d)
          | (VOID, _), _ -> Some VOID)
      | None -> None)
  | BINARY (_, _, _), _ -> Some (DTYPE INT)
  | COND _, _ -> Some (DTYPE BOOL)

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
            | Parser.IF (cond, blk) -> acc @ get_ret_expr_in_blk blk true
            | Parser.IF_ELSE ((cond, if_blk), else_blk) ->
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

      let fname = match func.func_name with IDENT f -> f in
      (* condition: each block must contain a maximum of 1 return expression (not including sub-blocks) *)
      match check_func_block func.body fname with
      | [] -> (
          (* check block *)
          match check_block func.body with
          | [] -> (
              (* condition: void function must not contain return expression *)
              let ret_stmts = get_ret_expr_in_blk func.body true in
              match func.return_type with
              | DATATYPE func_rettype, _ -> (
                  match ret_stmts with
                  | [] ->
                      [
                        Utils.format_msg f_pos
                          (Printf.sprintf
                             "Non-void Function: '%s' contains no return \
                              expression!"
                             fname);
                      ]
                  | _ ->
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
                                     match (func_rettype, get_expr_type e) with
                                     | INT, Some (DTYPE INT) -> []
                                     | BOOL, Some (DTYPE BOOL) -> []
                                     | _, Some dt ->
                                         [
                                           Utils.format_msg e_pos
                                             (Printf.sprintf
                                                "Function: '%s's return \
                                                 expression of %s doesn't \
                                                 match declared %s!"
                                                fname
                                                (Lexer.format_dtype func_rettype)
                                                (format_expr_type dt));
                                         ]
                                     | _, None ->
                                         [
                                           Utils.format_msg e_pos
                                             (Printf.sprintf
                                                "Function: '%s's return \
                                                 expression of dtype('none') \
                                                 doesn't match declared  %s!"
                                                fname
                                                (Lexer.format_dtype func_rettype));
                                         ])
                                 | (NONE, e_pos), true -> []
                                 (* --invalid paths-- *)
                                 | (EXPR e, e_pos), true ->
                                     [
                                       Utils.format_msg e_pos
                                         (Printf.sprintf
                                            "Function: function '%s' expects \
                                             optional return expressions - \
                                             'Some (return expression)' or \
                                             'None'!"
                                            fname);
                                     ]
                                 | (SOME_EXPR e, e_pos), false ->
                                     [
                                       Utils.format_msg e_pos
                                         (Printf.sprintf
                                            "Function: function '%s' expects \
                                             non-optional return expressions. \
                                             Found 'Some (return expression)'!"
                                            fname);
                                     ]
                                 | (NONE, e_pos), false ->
                                     [
                                       Utils.format_msg e_pos
                                         (Printf.sprintf
                                            "Function: function '%s' expects \
                                             non-optional return expressions. \
                                             Found 'None'!"
                                            fname);
                                     ])
                             | _ ->
                                 [
                                   Utils.format_msg f_pos
                                     (Printf.sprintf
                                        "Function: non-void function '%s' \
                                         contains multiple return expressions \
                                         in the same block!"
                                        fname);
                                 ])
                           ret_stmts))
              | VOID, _ -> (
                  (* void function shouldn't contain return expressions *)
                  match ret_stmts with
                  | [] -> []
                  | _ ->
                      [
                        Utils.format_msg f_pos
                          (Printf.sprintf
                             "Function: void function '%s' cannot return a \
                              value!"
                             fname);
                      ]))
          | blk_errs ->
              Utils.format_msg f_pos
                (Printf.sprintf "Function:'%s' contains invalid block!" fname)
              :: blk_errs)
      | fd_errs -> fd_errs)

(* function call *)
and check_func_call (func_call : Parser.func_call) : semantic_errors =
  match func_call with
  | func, pos -> (
      let fname = match func.func_name with IDENT f -> f in
      (* check if function has been defined *)
      match Hashtbl.find_opt func_env func.func_name with
      | Some ent -> (
          (* check each expression in the list of arguments *)
          let errs =
            List.fold_left (fun acc exp -> acc @ check_expr exp) [] func.args
          in
          match errs with
          (* compare parameters with arguments *)
          | [] -> compare_params_with_args ent.params func.args pos fname
          | errors -> errors)
      | None ->
          [
            Utils.format_msg pos
              (Printf.sprintf "Function Call: call to undefined function '%s'!"
                 fname);
          ])

and check_func_block (block : Parser.block) (func_name : string) :
    semantic_errors =
  (* condition: each block should contain a maximum of 1 return expression *)
  let rec check_sub_blocks (stmts : Parser.stmt list) : semantic_errors =
    match stmts with
    | [] -> []
    | s :: st -> (
        match s with
        | IF (cond, blk) ->
            (* check sub_block *)
            check_func_block blk func_name @ check_sub_blocks st
        | IF_ELSE ((cond, if_blk), else_blk) ->
            (* check both sub-blocks *)
            check_func_block if_blk func_name
            @ check_func_block else_blk func_name
        | LOOP lp -> (
            (* check loop *)
            match lp with
            | WHILE lp, _ -> check_func_block lp.body func_name
            | FOR lp, _ -> check_func_block lp.body func_name)
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
            Utils.format_msg pos
              (Printf.sprintf
                 "Function: each block inside function '%s' cannot contain \
                  more than 1 return expression "
                 func_name);
          ])

and compare_params_with_args (params : Parser.param_list)
    (args : Parser.arg_list) (pos : Lexer.position) (func_name : string) :
    semantic_errors =
  (* helper function which compares parameters & arguments *)
  (* Note: each argument should already be checked *)
  match (params, args) with
  | (param_type, param_name) :: rem_param, arg :: rem_args -> (
      let pname = match param_name with IDENT p -> p in
      (* condition: argument type must match parameter type *)
      match (param_type, get_expr_type arg) with
      | INT, Some (DTYPE INT) -> []
      | BOOL, Some (DTYPE BOOL) -> []
      | _, Some dt ->
          [
            Utils.format_msg pos
              (Printf.sprintf
                 "Function call: argument type doesn't match parameter type. \
                  Parameter '%s' has '%s' and argument has '%s' in call to \
                  '%s'!"
                 pname
                 (Lexer.format_dtype param_type)
                 (format_expr_type dt) func_name);
          ]
      | _, None ->
          [
            Utils.format_msg pos
              (Printf.sprintf
                 "Function call: argument type doesn't match parameter type. \
                  Parameter '%s' has '%s' and argument has dtype('none') in \
                  call to '%s' !"
                 pname
                 (Lexer.format_dtype param_type)
                 func_name);
          ])
  | [], [] -> []
  (* condition: argument count must match parameter count *)
  | _ ->
      [
        Utils.format_msg pos
          (Printf.sprintf
             "Function Call: argument count must match parameter count in call \
              to '%s'!"
             func_name);
      ]

(* Statement *)
and check_stmt (stmt : Parser.stmt) : semantic_errors =
  (* check stmt *)
  match stmt with
  | VARDECL vd -> check_var_decl vd
  | ASST at -> check_asst at false
  | LOOP lp -> check_loop lp
  | IF (cond, blk) -> check_cond_expr cond @ check_block blk
  | IF_ELSE ((cond, if_blk), else_blk) ->
      check_cond_expr cond @ check_block if_blk @ check_block else_blk
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
