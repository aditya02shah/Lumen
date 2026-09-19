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
  is_optional_type : bool;
  ret_type : Ast.ret_type;
  params : Ast.param_list;
  body : Ast.block;
  env : scope_vars;
}

(* --GLOBAL VARS-- *)
val func_decls :
  (Ast.ident, func_details) Hashtbl.t (* keeps track of function details *)

(* --PUBLIC INTERFACE-- *)
val check_prog : Ast.prog -> semantic_errors
