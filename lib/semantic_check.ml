(* --EXCEPTIONS-- *)
exception Internal_error

(* --TYPE DECLARATIONS-- *)
type semantic_errors = string list

(* Variables *)
type eval_value = INT of int | BOOL of bool
type asst_val = NONE | EXPR of Ast.expr | VAL of eval_value | IS_PARAM
type var_details = { mut : bool; dtype : Ast.dtype; assigned_val : asst_val }

(* Expressions *)
type expr_type = DTYPE of Ast.dtype | OPTION_DTYPE of Ast.dtype | VOID

(* Functions *)
type scope_vars =
  (Ast.ident, var_details) Hashtbl.t (* keeps track of variables *)

type func_details = {
  fname : string;
  is_optional_type : bool;
  ret_type : Ast.ret_type;
  params : Ast.param_list;
  body : Ast.block;
  env : scope_vars;
}

(* --GLOBAL VARS-- *)
let hashtable_size = 50

let func_decls : (Ast.ident, func_details) Hashtbl.t =
  Hashtbl.create hashtable_size (* keeps track of function details *)

(* --HELPER FUNCTIONS-- *)
let format_expr_type (etype : expr_type) =
  match etype with
  | DTYPE d -> Ast.format_dtype d
  | OPTION_DTYPE d -> (
      match d with
      | INT -> "dtype('Option<Int>')"
      | BOOL -> "dtype('Option<Bool>')")
  | VOID -> "dtype('void')"

(* --SEMANTIC CHECKER IMPLEMENTATION-- *)
(* Variables *)
(* variable declaration *)
let rec check_var_decl (var_decl : Ast.var_decl) (env : scope_vars) :
    semantic_errors =
  match var_decl with
  | DECL (d, _) ->
      let decl_mut, decl_type = (d.mut, d.dtype) in
      (* store all the variables declared *)
      List.iter
        (fun var_name ->
          Hashtbl.replace env var_name
            { mut = decl_mut; dtype = decl_type; assigned_val = NONE })
        d.vars;
      []
  | DECL_ASST da -> check_decl_asst da env

and check_decl_asst ((decl_asst, pos) : Ast.decl_asst) (env : scope_vars) :
    semantic_errors =
  match decl_asst.asst with
  | at, _ ->
      (* store variable *)
      Hashtbl.replace env at.name
        {
          mut = decl_asst.mut;
          dtype = decl_asst.dtype;
          assigned_val = EXPR at.expr;
        };
      (* check asst *)
      check_asst decl_asst.asst env true

(* variable assignment *)
and check_asst ((at, pos) : Ast.asst) (env : scope_vars) (is_decl_asst : bool) :
    semantic_errors =
  (* condition: var must be defined *)

  (* check whether var entry exists in our env *)
  let var_name = match at.name with IDENT vname -> vname in
  match Hashtbl.find_opt env at.name with
  | Some ent ->
      if
        (* condition: var must be mutable or it is must be a declaration assignment *)
        is_decl_asst || ent.mut
      then
        (* check the expr variable is being assigned to *)
        match check_expr at.expr env with
        | [] -> (
            (* condition: expression type must match type of var being assigned to *)
            match (ent.dtype, get_expr_type at.expr env) with
            | INT, Some (DTYPE INT) -> []
            | BOOL, Some (DTYPE BOOL) -> []
            | dt1, Some dt2 ->
                (* type mismatch *)
                [
                  Utils.format_msg pos
                    (Printf.sprintf
                       "Assignment: variable %s and expression %s do not match!"
                       (Ast.format_dtype dt1) (format_expr_type dt2));
                ]
            | dt, None ->
                (* type mismatch *)
                [
                  Utils.format_msg pos
                    (Printf.sprintf
                       "Assignment: variable %s and expression dtype('none') \
                        do not match!"
                       (Ast.format_dtype dt));
                ])
        | expr_errs ->
            (* var is being assigned an invalid expression *)
            Utils.format_msg pos "Assignment: contains invalid expression!"
            :: expr_errs
      else
        (* assigning to immutable var *)
        [
          Utils.format_msg pos
            (Printf.sprintf
               "Assignment: cannot assign value to immutable variable '%s'!"
               var_name);
        ]
  | _ ->
      (* var entry doesn't exist in our env *)
      [
        Utils.format_msg pos
          (Printf.sprintf "Assignment: contains an undefined variable '%s'!"
             var_name);
      ]

(* Expression *)
and check_expr (expr : Ast.expr) (env : scope_vars) : semantic_errors =
  match expr with
  | VAR var, pos -> (
      let var_name = match var with IDENT vname -> vname in
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt env var with
      (* check whether var entry exists in our env *)
      | Some ent -> (
          (* check whether unassigned var is being used in the expression *)
          match ent.assigned_val with
          | EXPR e -> []
          | VAL v -> []
          | IS_PARAM -> []
          | NONE ->
              (* var is not assigned a value *)
              [
                Utils.format_msg pos
                  (Printf.sprintf
                     "Expression: contains an unassigned variable '%s'!"
                     var_name);
              ])
      | _ ->
          (* var entry doesn't exist in our env *)
          [
            Utils.format_msg pos
              (Printf.sprintf "Expression: contains an undefined variable '%s'!"
                 var_name);
          ])
  (* check increment *)
  | INCREMENT var, pos -> (
      let var_name = match var with IDENT vname -> vname in
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt env var with
      (* check whether var entry exists in our env *)
      | Some ent -> (
          (* condition: var must be mutable *)
          match ent.mut with
          | true -> (
              match ent.assigned_val with
              (* condition: var must be an integer variable *)
              | EXPR e -> (
                  (* check var dtype *)
                  match ent.dtype with
                  | INT -> []
                  | _ ->
                      (* var has non-int dtype *)
                      [
                        Utils.format_msg pos
                          (Printf.sprintf
                             "Expression: Increment operation requires integer \
                              variable. Variable '%s' has %s!"
                             var_name
                             (Ast.format_dtype ent.dtype));
                      ])
              | VAL v -> []
              | IS_PARAM -> []
              | NONE ->
                  (* var has not been assigned a value *)
                  [
                    Utils.format_msg pos
                      (Printf.sprintf
                         "Expression: contains an unassigned variable '%s'!"
                         var_name);
                  ])
          | false ->
              (* var is immutable *)
              [
                Utils.format_msg pos
                  (Printf.sprintf
                     "Expression: cannot increment immutable variable '%s'!"
                     var_name);
              ])
      | _ ->
          (* var entry doesn't exist in our env *)
          [
            Utils.format_msg pos
              (Printf.sprintf "Expression: contains an undefined variable '%s'!"
                 var_name);
          ])
  (* check conditional expression *)
  | COND ce, _ -> check_cond_expr ce env
  (* check function call *)
  | FUNC_CALL fc, _ -> check_func_call fc env
  (* check arithmetic expression *)
  | BINARY (arithmetic_op, exp1, exp2), pos -> (
      match (exp1, exp2) with
      | (VAR var1, _), (VAR var2, _) ->
          check_binary_operation var1 var2 env
            "Expression: arithmetic expression" pos
      | (VAR var, pos), e | e, (VAR var, pos) ->
          check_var_in_expr var env pos (check_expr e env)
            "Expression: arithmetic operation "
      (* check func_call *)
      | (FUNC_CALL (f, _), pos), e | e, (FUNC_CALL (f, _), pos) -> (
          (* condition: only integer functions must be used in arithmetic expressions *)
          let fname = match f.func_name with IDENT f -> f in
          (* check return type of function *)
          match Hashtbl.find_opt func_decls f.func_name with
          (* check whether func entry exists in our env *)
          | Some ent -> (
              match ent.ret_type with
              (* check func dtype *)
              | DATATYPE INT, _ -> []
              | _ ->
                  (* func has non-int dtype *)
                  Utils.format_msg pos
                    (Printf.sprintf
                       "Expression: non-integer function '%s' cannot be used \
                        in an arithmetic expression!"
                       fname)
                  :: check_expr e env)
          | _ ->
              (* func entry doesn't exist in our env *)
              Utils.format_msg pos
                (Printf.sprintf
                   "Expression: contains an undefined function '%s'!" fname)
              :: check_expr e env)
      | (COND ce, pos), _ | _, (COND ce, pos) ->
          [
            Utils.format_msg pos
              "Expression: arithmetic operation cannot be performed on \
               conditional expressions!";
          ]
      (* check both operands *)
      | _ -> check_expr exp1 env @ check_expr exp2 env)
  | _ -> []

(* conditional expression *)
and check_cond_expr (cond_expr : Ast.cond_expr) (env : scope_vars) :
    semantic_errors =
  match cond_expr with
  | BOOL_VAL (VAL_CMP (op, val1, val2), pos) -> (
      (* condition: logical_compare operands must be integer literals/variables *)
      match (val1, val2) with
      | (NUMBER n1, _), (NUMBER n2, _) -> []
      | (NUMBER n, _), (IDENTIFIER var, _) | (IDENTIFIER var, _), (NUMBER n, _)
        ->
          check_var_in_expr var env pos [] "Conditional Expression: "
      | (IDENTIFIER var1, pos), (IDENTIFIER var2, _) ->
          check_binary_operation var1 var2 env "Conditional Expression" pos)
  | COMBINATION ((cm, ce1, ce2), _) ->
      check_cond_expr ce1 env @ check_cond_expr ce2 env
  | _ -> []

and check_var_in_expr (var : Ast.ident) (env : scope_vars) (pos : Ast.position)
    (errs : semantic_errors) (prefix : string) : semantic_errors =
  let var_name = match var with IDENT vname -> vname in
  match Hashtbl.find_opt env var with
  | Some ent -> (
      (* check whether var is assigned a value *)
      match ent.assigned_val with
      | EXPR e -> (
          (* check var type *)
          match ent.dtype with
          | INT -> errs
          | _ ->
              (* var has non-int dtype *)
              Utils.format_msg pos
                (Printf.sprintf
                   "%s: requires integer operands. Variable '%s' has %s!" prefix
                   var_name
                   (Ast.format_dtype ent.dtype))
              :: errs)
      | VAL v -> []
      | IS_PARAM -> []
      | NONE ->
          (* var has not been assigned a value *)
          Utils.format_msg pos
            (Printf.sprintf "%s: contains an unassigned variable '%s'!" prefix
               var_name)
          :: errs)
  | _ ->
      (* var entry doesn't exist in our env *)
      Utils.format_msg pos
        (Printf.sprintf "%s: contains an undefined variable '%s'!" prefix
           var_name)
      :: errs

and check_binary_operation (var1 : Ast.ident) (var2 : Ast.ident)
    (env : scope_vars) (msg_prefix : string) (pos : Ast.position) :
    semantic_errors =
  let var_name1 = match var1 with IDENT vname -> vname in
  let var_name2 = match var2 with IDENT vname -> vname in
  (* condition: both the variables must be defined and assigned a value *)
  match (Hashtbl.find_opt env var1, Hashtbl.find_opt env var2) with
  | Some ent1, Some ent2 -> (
      (* check whether the variables have been assigned values *)
      match (ent1.assigned_val, ent2.assigned_val) with
      (* --valid path-- *)
      | EXPR e1, EXPR e2 -> (
          (* condition: both the variables must have matching types *)
          (* condition: the variable operands must not be boolean *)
          match (ent1.dtype, ent2.dtype) with
          (* check whether both operands are ints *)
          | INT, INT -> []
          | _ ->
              [
                Utils.format_msg pos
                  (Printf.sprintf
                     "%s requires integer operands. Variable '%s' has '%s' and \
                      variable '%s' has '%s'!"
                     msg_prefix var_name1
                     (Ast.format_dtype ent1.dtype)
                     var_name2
                     (Ast.format_dtype ent2.dtype));
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
          ]
      | _ -> [])
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

and get_expr_type (expr : Ast.expr) (env : scope_vars) : expr_type option =
  (* returns the type for an expression if it can be determined, None otherwise *)
  (* NOTE: assumes expr has already been checked *)
  match expr with
  | INT _, _ -> Some (DTYPE INT)
  | VAR v, _ -> (
      (* determine var type *)
      match Hashtbl.find_opt env v with
      | Some ent -> Some (DTYPE ent.dtype)
      | None -> None)
  | INCREMENT x, _ -> Some (DTYPE INT)
  | FUNC_CALL (f, _), _ -> (
      (* determine func return type *)
      match Hashtbl.find_opt func_decls f.func_name with
      | Some ent -> (
          match (ent.ret_type, ent.is_optional_type) with
          | (DATATYPE d, _), false -> Some (DTYPE d)
          | (DATATYPE d, _), true -> Some (OPTION_DTYPE d)
          | (VOID, _), _ -> Some VOID)
      | None -> None)
  | BINARY (_, _, _), _ -> Some (DTYPE INT)
  | COND _, _ -> Some (DTYPE BOOL)

and get_ret_expr_in_blk (func_blk : Ast.block) (include_sub_blocks : bool) :
    Ast.ret_expr list =
  (* returns all the return expressions present in a block *)
  if include_sub_blocks then
    (* check all sub-blocks *)
    match func_blk with
    | STMTS stmts, _ ->
        (* store all return expressions encountered *)
        List.fold_left
          (fun acc st ->
            match st with
            | Ast.RETURN re -> acc @ [ re ]
            (* if stmt contains subblocks, explore them *)
            | Ast.IF (cond, blk) -> acc @ get_ret_expr_in_blk blk true
            | Ast.IF_ELSE ((cond, if_blk), else_blk) ->
                (acc @ get_ret_expr_in_blk if_blk true)
                @ get_ret_expr_in_blk else_blk true
            | Ast.LET ((let_stmt, if_blk), else_blk) ->
                (acc @ get_ret_expr_in_blk if_blk true)
                @ get_ret_expr_in_blk else_blk true
            | Ast.LOOP lp -> (
                match lp with
                | WHILE lp, _ -> acc @ get_ret_expr_in_blk lp.body true
                | FOR lp, _ -> acc @ get_ret_expr_in_blk lp.body true)
            | _ -> acc)
          [] stmts
  else
    (* dont check sub-blocks *)
    match func_blk with
    | STMTS stmts, _ ->
        (* store all return expressions encountered *)
        List.rev
          (List.fold_left
             (fun acc st ->
               match st with Ast.RETURN re -> re :: acc | _ -> acc)
             [] stmts)

and check_ret_expr (re : Ast.ret_expr) (fdetails : func_details) :
    semantic_errors =
  (* checks each return expression *)
  match fdetails.ret_type with
  | DATATYPE func_rettype, _ -> (
      (* condition: return type of function must match return expression's type *)
      (* condition: return expressions in optional functions must use "Some" & "None" *)
      match (re, fdetails.is_optional_type) with
      | (EXPR e, e_pos), false | (SOME_EXPR e, e_pos), true -> (
          match (func_rettype, get_expr_type e fdetails.env) with
          | INT, Some (DTYPE INT) -> []
          | BOOL, Some (DTYPE BOOL) -> []
          | _, Some dt ->
              [
                Utils.format_msg e_pos
                  (Printf.sprintf
                     "Function: '%s's return expression of %s doesn't match \
                      declared %s!"
                     fdetails.fname (format_expr_type dt)
                     (Ast.format_dtype func_rettype));
              ]
          | _, None ->
              [
                Utils.format_msg e_pos
                  (Printf.sprintf
                     "Function: '%s's return expression of dtype('none') \
                      doesn't match declared  %s!"
                     fdetails.fname
                     (Ast.format_dtype func_rettype));
              ])
      | (NONE, e_pos), true -> []
      | (EXPR e, e_pos), true -> (
          (* if function uses optional types and return expression contains optional types, it is valid *)
          match (func_rettype, get_expr_type e fdetails.env) with
          | INT, Some (OPTION_DTYPE INT) -> []
          | BOOL, Some (OPTION_DTYPE BOOL) -> []
          | _, _ ->
              [
                Utils.format_msg e_pos
                  (Printf.sprintf
                     "Function: function '%s' expects optional return \
                      expressions - 'Some (return expression)' or 'None'!"
                     fdetails.fname);
              ])
      (* --invalid path-- *)
      | (SOME_EXPR e, e_pos), false ->
          [
            Utils.format_msg e_pos
              (Printf.sprintf
                 "Function: function '%s' expects non-optional return \
                  expressions. Found 'Some (return expression)'!"
                 fdetails.fname);
          ]
      | (NONE, e_pos), false ->
          [
            Utils.format_msg e_pos
              (Printf.sprintf
                 "Function: function '%s' expects non-optional return \
                  expressions. Found 'None'!"
                 fdetails.fname);
          ])
  | VOID, _ -> []

(* Functions *)
(* function declaration *)
and check_func_decl ((func_decl, pos) : Ast.func_decl) : semantic_errors =
  (* store func_decl *)
  let fenv : scope_vars = Hashtbl.create hashtable_size in
  let fname = match func_decl.func_name with IDENT n -> n in
  let func_entry : func_details =
    {
      fname;
      is_optional_type = func_decl.is_optional_type;
      ret_type = func_decl.return_type;
      params = func_decl.params;
      body = func_decl.body;
      env = fenv;
    }
  in
  (* store function declaration *)
  Hashtbl.replace func_decls func_decl.func_name func_entry;

  (* initialize parameters in function env *)
  List.iter
    (fun (param_type, param_name) ->
      Hashtbl.replace fenv param_name
        {
          mut = true;
          (* parameter can be modified *)
          dtype = param_type;
          assigned_val = IS_PARAM;
        })
    func_decl.params;

  (* condition: each block must contain a maximum of 1 return expression (not including sub-blocks) *)
  match check_func_block func_decl.body fname with
  | [] -> (
      (* check block *)
      match check_block func_decl.body func_entry with
      | [] -> (
          (* condition: void function must not contain return expression *)
          let ret_stmts = get_ret_expr_in_blk func_decl.body true in
          match func_decl.return_type with
          | DATATYPE func_rettype, _ -> (
              (* non-void function should return an expression *)
              match ret_stmts with
              | [] ->
                  [
                    Utils.format_msg pos
                      (Printf.sprintf
                         "Non-void Function: '%s' contains no return \
                          expression!"
                         fname);
                  ]
              | _ -> [])
          | VOID, _ -> (
              (* void function shouldn't contain return expressions *)
              match ret_stmts with
              | [] -> []
              | _ ->
                  [
                    Utils.format_msg pos
                      (Printf.sprintf
                         "Function: void function '%s' cannot return a value!"
                         fname);
                  ]))
      | blk_errs ->
          Utils.format_msg pos
            (Printf.sprintf "Function:'%s' contains invalid block!" fname)
          :: blk_errs)
  | fd_errs -> fd_errs

(* function call *)
and check_func_call ((func_call, pos) : Ast.func_call) (env : scope_vars) :
    semantic_errors =
  let fname = match func_call.func_name with IDENT f -> f in
  (* check if function has been defined *)
  match Hashtbl.find_opt func_decls func_call.func_name with
  | Some ent -> (
      (* check each expression in the list of arguments *)
      let errs =
        List.fold_left
          (fun acc exp -> acc @ check_expr exp env)
          [] func_call.args
      in
      match errs with
      (* compare parameters with arguments *)
      | [] -> compare_params_with_args ent.params func_call.args env pos fname
      | errors -> errors)
  | None ->
      (* func doesnt exist in our env *)
      [
        Utils.format_msg pos
          (Printf.sprintf "Function Call: call to undefined function '%s'!"
             fname);
      ]

and check_func_block (block : Ast.block) (func_name : string) : semantic_errors
    =
  (* condition: each block should contain a maximum of 1 return expression *)
  let rec check_sub_blocks (stmts : Ast.stmt list) : semantic_errors =
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

and compare_params_with_args (params : Ast.param_list) (args : Ast.arg_list)
    (env : scope_vars) (pos : Ast.position) (func_name : string) :
    semantic_errors =
  (* helper function which compares parameters & arguments *)
  (* Note: each argument should already be checked *)
  let rec loop (p : Ast.param_list) (a : Ast.arg_list) (accum : semantic_errors)
      : semantic_errors =
    match (p, a) with
    | [], [] -> List.rev accum
    | (param_type, IDENT param_name) :: rem_param, arg :: rem_args -> (
        match (param_type, get_expr_type arg env) with
        | INT, Some (DTYPE INT) -> loop rem_param rem_args accum
        | BOOL, Some (DTYPE BOOL) -> loop rem_param rem_args accum
        | _, Some dt ->
            loop rem_param rem_args
              (Utils.format_msg pos
                 (Printf.sprintf
                    "Function call: argument type doesn't match parameter \
                     type. Parameter '%s' has '%s' and argument has '%s' in \
                     call to '%s'!"
                    param_name
                    (Ast.format_dtype param_type)
                    (format_expr_type dt) func_name)
              :: accum)
        | _, None ->
            loop rem_param rem_args
              (Utils.format_msg pos
                 (Printf.sprintf
                    "Function call: argument type doesn't match parameter \
                     type. Parameter '%s' has '%s' and argument has \
                     dtype('none') in call to '%s' !"
                    param_name
                    (Ast.format_dtype param_type)
                    func_name)
              :: accum))
    | _ ->
        (* condition: argument count must match parameter count *)
        [
          Utils.format_msg pos
            (Printf.sprintf
               "Function Call: argument count must match parameter count in \
                call to '%s'!"
               func_name);
        ]
  in
  loop params args []

(* Statement *)
and check_stmt (stmt : Ast.stmt) (fdetails : func_details) : semantic_errors =
  (* check stmt *)
  match stmt with
  | VARDECL vd -> check_var_decl vd fdetails.env
  | ASST at -> check_asst at fdetails.env false
  | LOOP lp -> check_loop lp fdetails
  | IF (cond, blk) ->
      check_cond_expr cond fdetails.env
      @ check_block blk { fdetails with env = Hashtbl.copy fdetails.env }
  | IF_ELSE ((cond, if_blk), else_blk) ->
      check_cond_expr cond fdetails.env
      @ check_block if_blk { fdetails with env = Hashtbl.copy fdetails.env }
      @ check_block else_blk { fdetails with env = Hashtbl.copy fdetails.env }
  | LET _ -> check_let_stmt stmt fdetails
  | RETURN ret_expr -> check_ret_expr ret_expr fdetails
  | EXP e -> check_expr e fdetails.env
  | EMPTY -> []

and check_let_stmt (stmt : Ast.stmt) (fdetails : func_details) : semantic_errors
    =
  match stmt with
  | LET (({ var; func_call }, if_blk), else_blk) -> (
      let fc, fc_name, pos =
        match func_call with
        | fc, pos -> ( match fc.func_name with IDENT f -> (fc, f, pos))
      in

      match check_func_call func_call fdetails.env with
      | [] -> (
          let func_called_entry = Hashtbl.find func_decls fc.func_name in
          if
            (* condition: function called must be of optional type *)
            not func_called_entry.is_optional_type
          then
            [
              Utils.format_msg pos
                (Printf.sprintf
                   "Let: cannot use non-optional function %s in a let \
                    statement!"
                   fc_name);
            ]
          else
            let ret_type =
              match func_called_entry.ret_type with rt, pos -> rt
            in

            match ret_type with
            | DATATYPE dt -> (
                (* create var *)
                (* var only exists within the if block scope/env *)
                let env_with_var = Hashtbl.copy fdetails.env in
                Hashtbl.replace env_with_var var
                  {
                    mut = false;
                    dtype = dt;
                    assigned_val = EXPR (Ast.FUNC_CALL func_call, pos);
                  };
                match
                  check_block if_blk { fdetails with env = env_with_var }
                with
                | [] -> (
                    match
                      check_block else_blk
                        (* else block scope/env doesnt contain var *)
                        { fdetails with env = Hashtbl.copy fdetails.env }
                    with
                    | [] -> []
                    | else_blk_errs -> else_blk_errs)
                | if_blk_errs -> if_blk_errs)
            | VOID ->
                [
                  Utils.format_msg pos
                    (Printf.sprintf
                       "Let: cannot use optional types with a void function %s \
                        in a let statement!"
                       fc_name);
                ])
      | fc_errs -> fc_errs)
  | _ -> []

(* Block *)
and check_block (block : Ast.block) (fdetails : func_details) : semantic_errors
    =
  (* check each statement in block *)
  match block with
  | STMTS stmts, _ ->
      List.fold_left (fun acc st -> acc @ check_stmt st fdetails) [] stmts

(* Loops *)
and check_loop (loop : Ast.loop) (fdetails : func_details) : semantic_errors =
  match loop with
  (* check while loop *)
  | WHILE while_lp, _ ->
      check_cond_expr while_lp.cond fdetails.env
      @ check_block while_lp.body fdetails
  (* check for loop *)
  | FOR for_lp, _ ->
      (*
       enforce evaluation order 
      @ doesnt guarantee left-to-right evaluation and I spent way too much time debugging this :)
      *)
      let init_errs = check_decl_asst for_lp.init fdetails.env in
      let cond_errs = check_cond_expr for_lp.cond fdetails.env in
      let update_errs =
        match for_lp.update with
        | INC i, p -> check_expr (Ast.INCREMENT i, p) fdetails.env
        | UPDATE_ASST at, _ -> check_asst at fdetails.env false
      in
      let body_errs = check_block for_lp.body fdetails in
      init_errs @ cond_errs @ update_errs @ body_errs

(* --PUBLIC INTERFACE-- *)
(* Program *)
let rec check_prog (program : Ast.prog) : semantic_errors =
  (* check all the functions in the program *)
  match program with
  | [] -> []
  | func :: rem_funcs ->
      List.fold_left
        (* gather all func_decl's *)
        (fun acc fn_decl -> acc @ check_func_decl fn_decl)
        [] (func :: rem_funcs)
