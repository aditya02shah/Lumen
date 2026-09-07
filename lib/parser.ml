exception Invalid_expr

(* TYPES *)

(* basic values *)
type ident = IDENT of string
type value = IDENTIFIER of ident | NUMBER of int | BOOLEAN of bool

(* func call *)
type arg_list = value list
type func_call = { func_name : ident; args : arg_list }

(* expression *)
type arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

type expr =
  | INT of int
  | STRING of string
  | VAR of ident
  | INCREMENT of expr
  | FUNC_CALL of func_call
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
type asst = { name : ident; expr : expr }

type var_decl =
  (* just declaring *)
  | DECL of { mut : bool; dtype : Lexer.dtype; vars : ident list }
    (* declaring and assigning in same statement *)
  | DECL_ASST of { mut : bool; dtype : Lexer.dtype; asst : asst }

(* statement *)
type stmt =
  | VARDECL of var_decl
  | ASST of asst
  | LOOP of loop
  | EXP of expr
  | EMPTY

and block = STMTS of stmt list
and while_loop = { cond : cond_expr; body : block }
and loop = WHILE of while_loop

(* function decl *)
type ret_type = DATATYPE of Lexer.dtype | VOID
type ret_expr = EXPR of expr | NONE
type param_list = (Lexer.dtype * ident) list

type func_decl = {
  func_name : ident;
  return_type : ret_type;
  params : param_list;
  body : block;
  ret_expr : ret_expr;
}

(* helper functions *)
let rec format_expr expr =
  match expr with
  | INT n -> Printf.sprintf "INT(%d)" n
  | STRING s -> Printf.sprintf "STRING(%S)" s
  | INCREMENT e -> Printf.sprintf "INCREMENT(%s)" (format_expr e)
  | VAR (IDENT i) -> Printf.sprintf "IDENT(%s)" i
  | FUNC_CALL fc -> "FUNCCALL"
  | BINARY (op, e1, e2) ->
      let op_str =
        match op with
        | ADD -> "ADD"
        | MULTIPLY -> "MULTIPLY"
        | SUBTRACT -> "SUBTRACT"
        | DIVIDE -> "DIVIDE"
        | MOD -> "MOD"
        | POW -> "POW"
      in
      Printf.sprintf "BINARY(%s, %s, %s)" op_str (format_expr e1)
        (format_expr e2)

let construct_expr (op : Lexer.token) (op1 : expr) (op2 : expr) : expr =
  match op with
  | ADD -> BINARY (ADD, op1, op2)
  | SUBTRACT -> BINARY (SUBTRACT, op1, op2)
  | MULTIPLY -> BINARY (MULTIPLY, op1, op2)
  | DIVIDE -> BINARY (DIVIDE, op1, op2)
  | MOD -> BINARY (MOD, op1, op2)
  | POW -> BINARY (POW, op1, op2)
  | _ -> raise Invalid_expr

let rec continue (t : Lexer.token list)
    (curr : Lexer.token list -> 'a option * Lexer.token list)
    (next : Lexer.token list -> 'a option * Lexer.token list) :
    'b option * Lexer.token list =
  match curr t with Some v, r -> (Some v, r) | None, _ -> next t

(* ARGUMENT LIST *)
let match_value (tokens : Lexer.token list) : value option * Lexer.token list =
  (* value = IDENTIFIER | INT | BOOLEAN *)
  match tokens with
  | Lexer.NUMBER n :: rem -> (Some (NUMBER n), rem)
  | Lexer.BOOLEAN b :: rem -> (Some (BOOLEAN b), rem)
  | Lexer.IDENTIFIER i :: rem -> (Some (IDENTIFIER (IDENT i)), rem)
  | _ -> (None, tokens)

let match_arg_list (tokens : Lexer.token list) :
    arg_list option * Lexer.token list =
  (* arg_list -> [] | value ("," value)* *)
  (* try to match value *)
  match match_value tokens with
  | Some v, [] -> (Some [ v ], [])
  | Some v, rem ->
      (* match as as many (",", value) pairs *)
      let rec loop (t : Lexer.token list) (accum : arg_list) :
          arg_list option * Lexer.token list =
        match t with
        (* try to match comma *)
        | COMMA :: ts -> (
            (* try to match another value *)
            match match_value ts with
            | Some newval, r -> loop r (newval :: accum)
            | _, _ -> (None, tokens) (* return arguments matched so far *))
        | _ -> (Some (List.rev accum), t)
      in
      loop rem [ v ]
  (* empty arglist *)
  | _, _ -> (Some [], tokens)

(* PARAMETER LIST *)
let match_param_list (tokens : Lexer.token list) : param_list * Lexer.token list
    =
  (* paramList -> [] | dtype Identifier ("," dtype Identifier)* *)
  match tokens with
  | DATATYPE d :: IDENTIFIER pname :: rem ->
      (* try to match dtype *)
      let rec loop (t : Lexer.token list) (accum : param_list) :
          param_list * Lexer.token list =
        (* match all possible (dtype identifier) pairs *)
        match t with
        | COMMA :: DATATYPE t :: IDENTIFIER p :: r ->
            loop r ((t, IDENT p) :: accum)
        | _ -> (List.rev accum, t)
      in
      loop rem [ (d, IDENT pname) ]
  | _ -> ([], tokens)

(* FUNCTION CALL *)
let match_func_call (tokens : Lexer.token list) :
    func_call option * Lexer.token list =
  (* func_call -> func_name "(" arg_list ")" ";" *)

  (* try to match func_name *)
  match tokens with
  | IDENTIFIER fname :: L_PAREN :: rem -> (
      (* try to match ")" & ";" *)
      match match_arg_list rem with
      | Some args, Lexer.R_PAREN :: r ->
          (Some { func_name = IDENT fname; args }, r)
      | _ -> (None, tokens))
  | _ -> (None, tokens)

(* EXPRESSION *)
let rec match_expr (tokens : Lexer.token list) : expr option * Lexer.token list
    =
  (* returns an expression along with the remaining tokens *)
  (* expr -> term (("+" | "-") term)* *)

  (* try to match term *)
  match match_term tokens with
  | None, _ -> (None, tokens)
  | Some left, rem ->
      (* find all possible terms *)
      let rec loop (t : Lexer.token list) (left : expr) :
          expr option * Lexer.token list =
        match t with
        | ((ADD | SUBTRACT) as op) :: r -> (
            match match_term r with
            | Some right, rem_tok ->
                let left = construct_expr op left right in
                loop rem_tok left
            | _ -> (Some left, t))
        | _ -> (Some left, t)
      in
      loop rem left

and match_term (tokens : Lexer.token list) : expr option * Lexer.token list =
  (* returns an expression along with the remaining tokens *)
  (* term -> factor (("/" | "*") factor)* *)

  (* try to match factor *)
  match match_factor tokens with
  | None, _ -> (None, tokens)
  | Some left, rem ->
      (* find all possible factors *)
      let rec loop (t : Lexer.token list) (left : expr) :
          expr option * Lexer.token list =
        match t with
        | ((MULTIPLY | DIVIDE) as op) :: r -> (
            match match_factor r with
            | Some right, rem_tok ->
                let left = construct_expr op left right in
                loop rem_tok left
            | _ -> (Some left, t))
        | _ -> (Some left, t)
      in
      loop rem left

and match_factor (tokens : Lexer.token list) : expr option * Lexer.token list =
  (* returns a factor along with the remaining tokens *)
  (* factor -> Identifier | Number | Identifier++ | func_call | "(" expr ")" *)
  match tokens with
  (* try to match identifier++ *)
  | IDENTIFIER i :: INCREMENT :: rem -> (Some (INCREMENT (VAR (IDENT i))), rem)
  (* try to match func_call *)
  | IDENTIFIER i :: L_PAREN :: r -> (
      match match_func_call tokens with
      | Some fc, rem -> (Some (FUNC_CALL fc), rem)
      | _ -> (None, tokens))
  (* try to match an identifier *)
  | IDENTIFIER i :: rem -> (Some (VAR (IDENT i)), rem)
  (* try to match a number *)
  | NUMBER i :: rem -> (Some (INT i), rem)
  (* try to match ( *)
  | L_PAREN :: rem -> (
      (* try to match an expression *)
      match match_expr rem with
      | Some e, Lexer.R_PAREN :: r -> (Some e, r) (* try to match ) *)
      | _, _ -> (None, tokens))
  | _ -> (None, tokens)

(* ASSIGNMENT *)
let format_asst (a : asst option) : string =
  match a with
  | Some x ->
      let name_str = match x.name with IDENT n -> n in
      name_str ^ " = " ^ format_expr x.expr
  | None -> "Not found"

let match_asst (tokens : Lexer.token list) : asst option * Lexer.token list =
  (* asst -> var_name "=" expr ";" *)
  match tokens with
  | IDENTIFIER name :: ASSIGNMENT :: rem -> (
      match match_expr rem with
      | Some e, SEMICOLON :: r -> (Some { name = IDENT name; expr = e }, r)
      | _, _ -> (None, tokens))
  | _ -> (None, tokens)

(* VARIABLE DECLARATION *)
let match_var_decl_helper (tokens : Lexer.token list) (dtype : Lexer.dtype)
    (vname : string) (is_mutable : bool) : var_decl option * Lexer.token list =
  (* find all var_name declarations matching ("," var_name)* *)
  let rec loop (t : Lexer.token list) (accum : ident list) :
      var_decl option * Lexer.token list =
    match (t, accum) with
    | SEMICOLON :: ts, a :: acs ->
        (Some (DECL { mut = false; dtype; vars = List.rev (a :: acs) }), ts)
    | COMMA :: IDENTIFIER v :: r, a :: acs -> loop r (IDENT v :: accum)
    | _, _ -> (None, tokens)
  in
  loop tokens [ IDENT vname ]

let match_var_decl (tokens : Lexer.token list) :
    var_decl option * Lexer.token list =
  (* var_decl -> dtype var_name ("," var_name)* ";"
              -> mut dtype var_name ("," var_name)* ";"
              -> dtype asst 
              -> mut dtype asst 
  *)
  match tokens with
  (* try matching var_decl -> dtype var_name ("," var_name)* ";"
                           -> mut dtype var_name ("," var_name)* ";" *)
  (* single var decl *)
  | DATATYPE dtype :: IDENTIFIER vname :: SEMICOLON :: rem ->
      (Some (DECL { mut = false; dtype; vars = [ IDENT vname ] }), rem)
  | MUT :: DATATYPE dtype :: IDENTIFIER vname :: SEMICOLON :: rem ->
      (Some (DECL { mut = true; dtype; vars = [ IDENT vname ] }), rem)
  (* multiple var decl *)
  | DATATYPE dtype :: IDENTIFIER vname :: COMMA :: rem ->
      match_var_decl_helper (COMMA :: rem) dtype vname false
  | MUT :: DATATYPE dtype :: IDENTIFIER vname :: COMMA :: rem ->
      match_var_decl_helper (COMMA :: rem) dtype vname true
  (* try matching var_decl -> dtype asst
                           -> mut dtype asst  *)
  (* immutable decl *)
  | DATATYPE dtype :: rem -> (
      match match_asst rem with
      | Some at, r -> (Some (DECL_ASST { mut = false; dtype; asst = at }), r)
      | _ -> (None, tokens))
  (* mutable decl *)
  | MUT :: DATATYPE dtype :: rem -> (
      match match_asst rem with
      | Some at, r -> (Some (DECL_ASST { mut = true; dtype; asst = at }), r)
      | _ -> (None, tokens))
  | _ -> (None, tokens)

(* CONDITIONAL EXPRESSION *)
let match_valcmp (tokens : Lexer.token list) :
    bool_value option * Lexer.token list =
  (* valcmp = "(" val logical_compare val ")"*)
  match tokens with
  (* try to match "(" *)
  | L_PAREN :: rem_tokens -> (
      (* try to match val (operand1) *)
      match match_value rem_tokens with
      | Some op1, tok :: rem -> (
          (* try to match logical_compare operator *)
          let res =
            match tok with
            | LESS_THAN -> Some LESS_THAN
            | LESS_THAN_EQUALS -> Some LESS_THAN_EQUALS
            | GREATER_THAN -> Some GREATER_THAN
            | GREATER_THAN_EQUALS -> Some GREATER_THAN_EQUALS
            | EQUALS -> Some EQUALS
            | NOT_EQUALS -> Some NOT_EQUALS
            | _ -> None
          in
          match res with
          | Some op -> (
              (* try to match val (operand2) *)
              match match_value rem with
              | Some op2, R_PAREN :: r -> (Some (VAL_CMP (op, op1, op2)), r)
              | _ -> (None, tokens))
          | None -> (None, tokens))
      | _ -> (None, tokens))
  | _ -> (None, tokens)

let match_boolcmp (tokens : Lexer.token list) :
    bool_value option * Lexer.token list =
  (* boolcmp = "(" bool logical_combination bool ")"*)
  match tokens with
  | L_PAREN :: BOOLEAN op1 :: LOGICAL_AND :: BOOLEAN op2 :: R_PAREN :: r ->
      (Some (BOOL_CMP (LOGICAL_AND, op1, op2)), r)
  | L_PAREN :: BOOLEAN op1 :: LOGICAL_OR :: BOOLEAN op2 :: R_PAREN :: r ->
      (Some (BOOL_CMP (LOGICAL_OR, op1, op2)), r)
  | _ -> (None, tokens)

let match_boolval (tokens : Lexer.token list) :
    bool_value option * Lexer.token list =
  (* 
  boolval = bool
          = not bool
          = boolcmp
          = valcmp
  *)
  match tokens with
  | [] -> (None, tokens)
  | BOOLEAN b :: rem_tokens ->
      (Some (BOOL b), rem_tokens) (* try to match bool *)
  | NOT :: BOOLEAN b :: rem_tokens ->
      (Some (NOT b), rem_tokens) (* try to match "not" bool *)
  | token :: rem_tokens ->
      (* try to match valcmp and valcmp *)
      continue tokens match_valcmp match_boolcmp

let rec match_cond_expr (tokens : Lexer.token list) :
    cond_expr option * Lexer.token list =
  (* 
  condexpr = boolval
           = boolval (logical_combination condexpr)
  *)

  (* try to match boolval *)
  match match_boolval tokens with
  (* | Some v, [] -> (Some (BOOL_VAL v), []) *)
  (* try to match boolval (logical_combination condexpr) *)
  | Some v, LOGICAL_AND :: r -> (
      (* try to match condexpr *)
      match match_cond_expr r with
      | Some c, rem -> (Some (COMBINATION (LOGICAL_AND, v, c)), rem)
      | _ -> (Some (BOOL_VAL v), LOGICAL_AND :: r))
  | Some v, LOGICAL_OR :: r -> (
      (* try to match condexpr *)
      match match_cond_expr r with
      | Some c, rem -> (Some (COMBINATION (LOGICAL_OR, v, c)), rem)
      | _ -> (Some (BOOL_VAL v), LOGICAL_OR :: r))
  | Some v, r -> (Some (BOOL_VAL v), r)
  | _ -> (None, tokens)

(* STATEMENT *)
let rec match_loop (tokens : Lexer.token list) : loop option * Lexer.token list
    =
  (* loop = while cond block *)
  match tokens with
  (* try to match while *)
  | WHILE :: rem_tokens -> (
      (* try to match cond *)
      match match_cond_expr rem_tokens with
      | Some exp, rem -> (
          (* try to match block *)
          match match_block rem with
          | Some b, r ->
              let loop = { cond = exp; body = b } in
              (Some (WHILE loop), r)
          | _ -> (None, tokens))
      | _ -> (None, tokens))
  | _ -> (None, tokens)

and match_block (tokens : Lexer.token list) : block option * Lexer.token list =
  (* block = "{" stmt "}" *)
  match tokens with
  (* try to match "{" *)
  | L_BRACE :: rem_tokens -> (
      (* try to stmt *)
      match match_stmt rem_tokens with
      (* try to match "}" *)
      | s :: st, R_BRACE :: rem -> (Some (STMTS (s :: st)), rem)
      | _ -> (None, tokens))
  | _ -> (None, tokens)

and match_stmt (tokens : Lexer.token list) : stmt list * Lexer.token list =
  (* stmt =  (var_decl | asst | loop | (expr ";"))* *)
  let rec loop (t : Lexer.token list) (accum : stmt list) :
      stmt list * Lexer.token list =
    (* match all valid stmts *)
    match match_stmt_helper t with
    | Some st, r -> loop r (st :: accum)
    | None, r -> (
        match accum with
        | [] -> ([ EMPTY ], tokens)
        | a :: acs -> (List.rev (a :: acs), r))
  in
  loop tokens []

and match_stmt_helper (tokens : Lexer.token list) :
    stmt option * Lexer.token list =
  (* match a single statement (stmt =  (var_decl | asst | loop | (expr ";") | func_call )* *)

  (* try to match var_decl *)
  match match_var_decl tokens with
  | Some vd, rem_tokens -> (Some (VARDECL vd), rem_tokens)
  | _ -> (
      (* try to match asst *)
      match match_asst tokens with
      | Some at, rem -> (Some (ASST at), rem)
      | _ -> (
          (* try to match loop *)
          match match_loop tokens with
          | Some lp, r -> (Some (LOOP lp), r)
          | _ -> (
              (* try to match expr *)
              match match_expr tokens with
              | Some e, SEMICOLON :: rem_tok -> (Some (EXP e), rem_tok)
              | _ -> (None, tokens))))

(* FUNCTION DECLARATION *)
let match_func_decl_helper (ret : ret_type) (fname : string)
    (tokens : Lexer.token list) : func_decl option * Lexer.token list =
  (* matches  "(" param_list ")" "{" stmt "}"    
            | "(" param_list ")" "{" stmt return expr ";" "}"  
    *)
  match match_param_list tokens with
  (* get paramList *)
  | params, R_PAREN :: L_BRACE :: rem_tokens -> (
      (* try to match block *)
      match match_stmt rem_tokens with
      | st, RETURN :: r_tok -> (
          (* try to match retval *)
          match match_expr r_tok with
          | Some exp, SEMICOLON :: R_BRACE :: toks ->
              ( Some
                  {
                    func_name = IDENT fname;
                    return_type = ret;
                    params;
                    body = STMTS st;
                    ret_expr = EXPR exp;
                  },
                toks )
          | _ -> (None, tokens))
      | st, R_BRACE :: r ->
          (* void function *)
          ( Some
              {
                func_name = IDENT fname;
                return_type = ret;
                params;
                body = STMTS [];
                ret_expr = NONE;
              },
            r )
      | _ -> (None, tokens))
  | _ -> (None, tokens)

let match_func_decl (tokens : Lexer.token list) :
    func_decl option * Lexer.token list =
  (* func_decl = ret_type func_name "(" param_list ")" "{" stmt "}"  
              = ret_type func_name "(" param_list ")" "{" stmt return value ";" "}" 
    *)
  match tokens with
  | Lexer.DATATYPE d :: IDENTIFIER fname :: L_PAREN :: rem ->
      match_func_decl_helper (DATATYPE d) fname rem
  | VOID :: IDENTIFIER fname :: L_PAREN :: rem ->
      match_func_decl_helper VOID fname rem
  | _ -> (None, tokens)
