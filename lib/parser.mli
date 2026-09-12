exception Invalid_expr

(* TYPES *)

(* Basic Units *)
type ident = IDENT of string

type value = IDENTIFIER of ident | NUMBER of int | BOOLEAN of bool

(* Variables *)

(* variable declaration *)
and decl = { mut : bool; dtype : Lexer.dtype; vars : ident list }

(* variable assignment *)
type asst = { name : ident; expr : expr }
and decl_asst = { mut : bool; dtype : Lexer.dtype; asst : asst }

and var_decl =
  | DECL of decl (* declaration only *)
  | DECL_ASST of decl_asst (* declaration and assignment in same statement *)

(* variable initialisation *)
and var_init = ASST of asst | DECL_ASST of decl_asst

(* Expression *)
and expr =
  | INT of int
  | VAR of ident
  | INCREMENT of ident
  | FUNC_CALL of func_call
  | BINARY of arithmetic_op * expr * expr
  | COND of cond_expr

(* conditional expression *)
and logical_compare =
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN
  | GREATER_THAN_EQUALS
  | EQUALS
  | NOT_EQUALS

and bool_value = BOOL of bool | VAL_CMP of logical_compare * value * value
and arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW
and logical_combination = LOGICAL_AND | LOGICAL_OR

and cond_expr =
  | BOOL_VAL of bool_value
  | COMBINATION of logical_combination * cond_expr * cond_expr

(* Functions *)

(* function declaration *)
and ret_type = DATATYPE of Lexer.dtype | VOID
and param_list = (Lexer.dtype * ident) list
and ret_expr = EXPR of expr | SOME_EXPR of expr | NONE

(* function call *)
and arg_list = expr list
and func_call = { func_name : ident; args : arg_list }

type func_decl = {
  func_name : ident;
  return_type : ret_type;
  is_optional_type : bool;
  params : param_list;
  body : block;
}

(* Statement *)
and stmt =
  | VARDECL of var_decl
  | ASST of asst
  | LOOP of loop
  | IF of block
  | IF_ELSE of block * block
  | RETURN of ret_expr
  | EXP of expr
  | EMPTY

(* Block *)
and block = STMTS of stmt list

(* Loops *)
and loop = WHILE of while_loop | FOR of for_loop

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

val match_expr : Lexer.token list -> expr option * Lexer.token list
val format_expr : expr -> string
val match_cond_expr : Lexer.token list -> cond_expr option * Lexer.token list
val match_var_decl : Lexer.token list -> var_decl option * Lexer.token list
val match_asst : Lexer.token list -> asst option * Lexer.token list
val format_asst : asst option -> string
val match_arg_list : Lexer.token list -> arg_list option * Lexer.token list
val match_param_list : Lexer.token list -> param_list * Lexer.token list
val match_func_call : Lexer.token list -> func_call option * Lexer.token list
val match_loop : Lexer.token list -> loop option * Lexer.token list
val match_block : Lexer.token list -> block option * Lexer.token list
val match_stmt : Lexer.token list -> stmt list * Lexer.token list
val match_func_decl : Lexer.token list -> func_decl option * Lexer.token list
val parse_prog : Lexer.token list -> func_decl list * Lexer.token list
