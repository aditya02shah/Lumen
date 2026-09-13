exception Invalid_expr

(* --TYPE DECLARATIONS-- *)

(* Basic Units *)
type ident = IDENT of string
type value_node = IDENTIFIER of ident | NUMBER of int | BOOLEAN of bool

type value = value_node * Lexer.position

(* Variables *)

(* variable declaration *)
and decl = decl_node * Lexer.position
and decl_node = { mut : bool; dtype : Lexer.dtype; vars : ident list }

(* variable assignment *)
and asst = asst_node * Lexer.position
and decl_asst = decl_asst_node * Lexer.position
and asst_node = { name : ident; expr : expr }
and decl_asst_node = { mut : bool; dtype : Lexer.dtype; asst : asst }

and var_decl =
  | DECL of decl (* declaration only *)
  | DECL_ASST of decl_asst (* declaration and assignment in same statement *)

(* variable initialisation *)
and var_init = var_init_node * Lexer.position
and var_init_node = ASST of asst | DECL_ASST of decl_asst

(* Expression *)
and expr = expr_node * Lexer.position

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
and bool_value = bool_value_node * Lexer.position

and bool_value_node =
  | BOOL of bool
  | VAL_CMP of logical_compare * value * value

and logical_combination = LOGICAL_AND | LOGICAL_OR
and combination = combination_node * Lexer.position
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
and func_decl = func_decl_node * Lexer.position

and func_decl_node = {
  func_name : ident;
  return_type : ret_type;
  is_optional_type : bool;
  params : param_list;
  body : block;
}

(* return expression *)
and ret_expr = ret_expr_node * Lexer.position
and ret_expr_node = EXPR of expr | SOME_EXPR of expr | NONE
and ret_type = ret_type_node * Lexer.position
and ret_type_node = DATATYPE of Lexer.dtype | VOID

(* params *)
and param_list = (Lexer.dtype * ident) list

(* function call *)
and func_call = func_call_node * Lexer.position
and func_call_node = { func_name : ident; args : arg_list }
and arg_list = expr list

(* Statement *)
and stmt =
  | VARDECL of var_decl
  | ASST of asst
  | LOOP of loop
  | IF of if_stmt
  | IF_ELSE of if_stmt * else_stmt
  | RETURN of ret_expr
  | EXP of expr
  | EMPTY

and if_stmt = cond_expr * block
and else_stmt = block

(* Block *)
and block = block_node * Lexer.position
and block_node = STMTS of stmt list

(* Loops *)
and loop = loop_node * Lexer.position
and loop_node = WHILE of while_loop | FOR of for_loop

(* while loop *)
and while_loop = { cond : cond_expr; body : block }

(* for loop *)
and for_loop = {
  init : var_init;
  cond : cond_expr;
  update : expr;
  body : block;
}

(* Program *)
type prog = func_decl list

(* --PARSER IMPLEMENTATION-- *)
val match_expr :
  Lexer.located_token list -> expr option * Lexer.located_token list

val format_expr : expr -> string

val match_cond_expr :
  Lexer.located_token list -> cond_expr option * Lexer.located_token list

val match_var_decl :
  Lexer.located_token list -> var_decl option * Lexer.located_token list

val match_asst :
  Lexer.located_token list -> asst option * Lexer.located_token list

val format_asst : asst option -> string

val match_arg_list :
  Lexer.located_token list -> arg_list option * Lexer.located_token list

val match_param_list :
  Lexer.located_token list -> param_list * Lexer.located_token list

val match_func_call :
  Lexer.located_token list -> func_call option * Lexer.located_token list

val match_loop :
  Lexer.located_token list -> loop option * Lexer.located_token list

val match_block :
  Lexer.located_token list -> block option * Lexer.located_token list

val match_stmt :
  Lexer.located_token list -> stmt list * Lexer.located_token list

val match_func_decl :
  Lexer.located_token list -> func_decl option * Lexer.located_token list

(* --PUBLIC INTERFACE-- *)
val parse_prog :
  Lexer.located_token list -> func_decl list * Lexer.located_token list
