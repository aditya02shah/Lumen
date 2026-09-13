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
val var_env : (Parser.ident, var_details) Hashtbl.t
val func_env : (Parser.ident, func_details) Hashtbl.t

(* --PUBLIC INTERFACE-- *)
val check_prog : Parser.prog -> semantic_errors
