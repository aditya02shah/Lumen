type semantic_errors = string list
type var_details = { mut : bool; dtype : Lexer.dtype; assigned_val : bool }

let var_env = Hashtbl.create 100 (* maps var -> var_details *)

let func_enc =
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
let check_expr (expr : Parser.expr) : semantic_errors = []
(* check cond_expr *)
(* check whether arithmetic_op compatible with its operands *)
(* check increment *)
(* check func_call *)
(* check whether undeclared vars being used *)

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
