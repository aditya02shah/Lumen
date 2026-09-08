exception Invalid_expr

(* TYPES *)

(* basic values *)
type ident = IDENT of string
type value = IDENTIFIER of ident | NUMBER of int | BOOLEAN of bool

(* conditional expression *)
type logical_compare =
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN
  | GREATER_THAN_EQUALS
  | EQUALS
  | NOT_EQUALS

type logical_combination = LOGICAL_AND | LOGICAL_OR

type bool_value = BOOL of bool | VAL_CMP of logical_compare * value * value

and cond_expr =
  | BOOL_VAL of bool_value
  | COMBINATION of logical_combination * cond_expr * cond_expr

(* func call *)
type arg_list = expr list
and func_call = { func_name : ident; args : arg_list }

(* expression *)
and arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

and expr =
  | INT of int
  | STRING of string
  | VAR of ident
  | INCREMENT of expr
  | FUNC_CALL of func_call
  | BINARY of arithmetic_op * expr * expr
  | COND of cond_expr

(* variables *)
type asst = { name : ident; expr : expr }

type var_decl =
  (* just declaring *)
  | DECL of { mut : bool; dtype : Lexer.dtype; vars : ident list }
    (* declaring and assigning in same statement *)
  | DECL_ASST of { mut : bool; dtype : Lexer.dtype; asst : asst }

(* statement *)
type ret_expr = EXPR of expr | SOME_EXPR of expr | NONE

type stmt =
  | VARDECL of var_decl
  | ASST of asst
  | LOOP of loop
  | IF of block
  | IF_ELSE of block * block
  | RETURN of ret_expr
  | EXP of expr
  | EMPTY

and block = STMTS of stmt list
and while_loop = { cond : cond_expr; body : block }
and loop = WHILE of while_loop

(* function decl *)
type ret_type = DATATYPE of Lexer.dtype | VOID
type param_list = (Lexer.dtype * ident) list

type func_decl = {
  func_name : ident;
  return_type : ret_type;
  is_optional_type : bool;
  params : param_list;
  body : block;
}

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
