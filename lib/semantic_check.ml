type semantic_errors = string list
type var_details = { mut : bool; dtype : Lexer.dtype; assigned_val : bool }
type func_details = { dtype : Lexer.dtype; params : Parser.param_list }

let var_env = Hashtbl.create 100 (* maps var -> var_details *)

let func_env =
  Hashtbl.create
    100 (* maps func -> type declarations, param names & type declarations *)

let check_func_call (asst : Parser.func_call) : semantic_errors = []

(* check if function defined *)
(* check if arglist matches paramlist & no of args matches paramlist *)

let check_var_init (var_init : Parser.var_init) : semantic_errors = []
(* check asst *)

let check_var_decl (var_decl : Parser.var_decl) : semantic_errors = []

(* check decl_asst. Declared type should match assigned type, and var should be mutable *)
let check_asst (asst : Parser.asst) : semantic_errors = []

(* check if var being assigned to mutable *)
let check_cond_expr (cond_expr : Parser.cond_expr) : semantic_errors = []

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
              | true -> (
                  match ent.dtype with
                  | INT -> []
                  | _ -> [ "Conditional Expression requires integer operands" ])
              | false ->
                  [ "Conditional Expression compares unassigned variables!" ])
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
              | true, true -> (
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
          | true -> []
          | false -> [ "Expression has an unassigned variable!" ])
      | _ -> [ "Expression has an undefined variable!" ])
  (* check increment *)
  | INCREMENT var -> (
      (* condition: var must be defined and assigned a value *)
      match Hashtbl.find_opt var_env var with
      | Some ent -> (
          match ent.assigned_val with
          (* condition: var must be an integer variable *)
          | true -> (
              match ent.dtype with
              | INT -> []
              | _ -> [ "Increment operation requires an integer variable!" ])
          | false -> [ "Expression has an unassigned variable!" ])
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
              | true, true -> (
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
              | true -> (
                  match ent.dtype with
                  | INT -> check_expr e
                  | _ -> "Expression requires integer operands" :: check_expr e)
              | false ->
                  "Expression compares unassigned variables!" :: check_expr e)
          | _ -> "Expression compares undefined variable!" :: check_expr e)
      (* check func_call *)
      | FUNC_CALL f, e | e, FUNC_CALL f -> (
          (* conditional: function used in arithmetic expression should return an integer *)

          (* check return type of function *)
          match Hashtbl.find_opt func_env f with
          | Some ent -> (
              match ent.dtype with
              | INT -> []
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

let check_stmt (stmt : Parser.stmt) : semantic_errors = []

(* check vardecl *)
(* check asst *)
(* check expr *)
(* check func_call *)
(* check whether undeclared vars being used *)
let check_block (asst : Parser.block) : semantic_errors = []

(* check each statement in block *)
let check_func (func : Parser.func_decl) : semantic_errors = []
(* if function is void, shouldnt have return statement *)
(* else;
 check whether func has return statement 
 check if ret_type matches ret_expr, if it exists
*)

let rec check_prog (program : Parser.prog) : semantic_errors =
  (* check all the functions in the program *)
  match program with
  | [] -> []
  | func :: rem_funcs -> check_func func @ check_prog rem_funcs
