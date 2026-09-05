exception Invalid_expr

(* TYPES *)

(* basic values *)
type ident = IDENT of string
type value = IDENTIFIER of ident | NUMBER of int | BOOLEAN of bool

(* expression *)
type arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

type expr =
  | INT of int
  | STRING of string
  | VAR of ident
  | INCREMENT of expr
  | BINARY of arithmetic_op * expr * expr

(* conditional expression *)
type logical_compare =
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN
  | GREATER_THAN_EQUALS
  | EQUALS
  | NOT_EQUALS

type logical_combination = LOGICAL_AND | LOGICAL_OR

type bool_value =
  | BOOL of bool
  | NOT of bool
  | VAL_CMP of logical_compare * value * value
  | BOOL_CMP of logical_combination * bool * bool

type cond_expr =
  | BOOL_VAL of bool_value
  | COMBINATION of logical_combination * bool_value * cond_expr

(* variables *)
type varDecl = { dtype : Lexer.dtype; vars : ident list }
type asst = { name : ident; expr : expr }

(* statement *)
type stmt =
  | VARDECL of varDecl
  | ASST of asst
  | LOOP of loop
  | EXP of expr
  | EMPTY

and block = STMTS of stmt list
and whileLoop = { cond : cond_expr; body : block }
and loop = WHILE of whileLoop

(* functions *)
type retType = DATATYPE of Lexer.dtype | VOID
type retVal = VAL of value | NONE
type argList = value list
type paramList = (Lexer.dtype * ident) list

type funcDecl = {
  funcName : ident;
  return_type : retType;
  params : paramList;
  body : block;
  retVal : retVal;
}

type funcCall = { funcName : ident; args : argList }

val match_expr : Lexer.token list -> expr option * Lexer.token list
val format_expr : expr -> string
val match_condexpr : Lexer.token list -> cond_expr option * Lexer.token list
val match_vardecl : Lexer.token list -> varDecl option * Lexer.token list
val match_asst : Lexer.token list -> asst option * Lexer.token list
val format_asst : asst option -> string
val match_arglist : Lexer.token list -> argList option * Lexer.token list
val match_paramList : Lexer.token list -> paramList * Lexer.token list
val match_funcall : Lexer.token list -> funcCall option * Lexer.token list
val match_loop : Lexer.token list -> loop option * Lexer.token list
val match_block : Lexer.token list -> block option * Lexer.token list
val match_stmt : Lexer.token list -> stmt list * Lexer.token list
val match_funcDecl : Lexer.token list -> funcDecl option * Lexer.token list
