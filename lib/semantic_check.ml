type semantic_errors = string list

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

(* check boolval *)
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
