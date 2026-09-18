(* --TYPE DECLARATIONS-- *)
type semantic_errors = string list

(* Variables *)
type asst_val = NONE | EXPR of Ast.expr
type var_details = { mut : bool; dtype : Ast.dtype; assigned_val : asst_val }

(* Expressions *)
type expr_type = DTYPE of Ast.dtype | OPTION_DTYPE of Ast.dtype | VOID

(* Functions *)
type func_details = {
  is_optional_type : bool;
  ret_type : Ast.ret_type;
  params : Ast.param_list;
}

(* --GLOBAL VARS-- *)
val var_env : (Ast.ident, var_details) Hashtbl.t
val func_env : (Ast.ident, func_details) Hashtbl.t

(* --PUBLIC INTERFACE-- *)
val check_prog : Ast.prog -> semantic_errors
