(* --AST TYPE DECLARATIONS-- *)

(* Basic Units *)
type dtype = INT | BOOL
type position = int
type ident = IDENT of string
type value_node = IDENTIFIER of ident | NUMBER of int

type value = value_node * position

(* Variables *)

(* variable declaration *)
and decl = decl_node * position
and decl_node = { mut : bool; dtype : dtype; vars : ident list }

(* variable assignment *)
and asst = asst_node * position
and decl_asst = decl_asst_node * position
and asst_node = { name : ident; expr : expr }
and decl_asst_node = { mut : bool; dtype : dtype; asst : asst }

and var_decl =
  | DECL of decl (* declaration only *)
  | DECL_ASST of decl_asst (* declaration and assignment in same statement *)

(* variable initialisation *)
and var_update = var_update_node * position
and var_update_node = INC of ident | UPDATE_ASST of asst

(* Expression *)
and expr = expr_node * position

and expr_node =
  | INT of int
  | VAR of ident
  | INCREMENT of ident
  | FUNC_CALL of func_call
  | BINARY of arithmetic_op * expr * expr
  | COND of cond_expr

and arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

(* conditional expression *)
and cond_expr = BOOL_VAL of bool_value | COMBINATION of combination
and bool_value = bool_value_node * position

and bool_value_node =
  | BOOL of bool
  | VAL_CMP of logical_compare * value * value

and logical_combination = LOGICAL_AND | LOGICAL_OR
and combination = combination_node * position
and combination_node = logical_combination * cond_expr * cond_expr

and logical_compare =
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN
  | GREATER_THAN_EQUALS
  | EQUALS
  | NOT_EQUALS

(* Functions *)

(* function declaration *)
and func_decl = func_decl_node * position

and func_decl_node = {
  func_name : ident;
  return_type : ret_type;
  is_optional_type : bool;
  params : param_list;
  body : block;
}

(* return expression *)
and ret_expr = ret_expr_node * position
and ret_expr_node = EXPR of expr | SOME_EXPR of expr | NONE
and ret_type = ret_type_node * position
and ret_type_node = DATATYPE of dtype | VOID

(* params *)
and param_list = (dtype * ident) list

(* function call *)
and func_call = func_call_node * position
and func_call_node = { func_name : ident; args : arg_list }
and arg_list = expr list

(* Statement *)
and stmt =
  | VARDECL of var_decl
  | ASST of asst
  | LOOP of loop
  | IF of if_stmt
  | IF_ELSE of if_stmt * else_stmt
  | LET of if_let_stmt * else_stmt
  | RETURN of ret_expr
  | EXP of expr
  | EMPTY

and if_stmt = cond_expr * block
and if_let_stmt = let_stmt * block
and let_stmt = { var : ident; func_call : func_call }
and else_stmt = block

(* Block *)
and block = block_node * position
and block_node = STMTS of stmt list

(* Loops *)
and loop = loop_node * position
and loop_node = WHILE of while_loop | FOR of for_loop

(* while loop *)
and while_loop = { cond : cond_expr; body : block }

(* for loop *)
and for_loop = {
  init : decl_asst;
  cond : cond_expr;
  update : var_update;
  body : block;
}

(* Program *)
type prog = func_decl list

(* --HELPER FUNCTIONS-- *)
let format_dtype (dt : dtype) : string =
  match dt with INT -> "dtype('Int')" | BOOL -> "dtype('Bool')"
