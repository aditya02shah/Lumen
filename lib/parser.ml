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
  | STRING of string
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

(* helper functions *)
let rec format_expr expr =
  match expr with
  | INT n -> Printf.sprintf "INT(%d)" n
  | STRING s -> Printf.sprintf "STRING(%S)" s
  | INCREMENT (IDENT i) -> Printf.sprintf "INCREMENT IDENT(%s)" i
  | VAR (IDENT i) -> Printf.sprintf "IDENT(%s)" i
  | FUNC_CALL fc -> "FUNCCALL"
  | COND cd -> "CE"
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

(* Basic Units *)
let match_value (tokens : Lexer.token list) : value option * Lexer.token list =
  (* value -> IDENTIFIER | INT | BOOLEAN *)
  match tokens with
  | Lexer.NUMBER n :: rem -> (Some (NUMBER n), rem)
  | Lexer.BOOLEAN b :: rem -> (Some (BOOLEAN b), rem)
  | Lexer.IDENTIFIER i :: rem -> (Some (IDENTIFIER (IDENT i)), rem)
  | _ -> (None, tokens)

(* Variables *)
(* variable declaration *)
let rec match_var_decl (tokens : Lexer.token list) :
    var_decl option * Lexer.token list =
  (* var_decl -> dtype var_name ("," var_name)* ";"
              -> mut dtype var_name ("," var_name)* ";"
              -> decl_asst
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
  | _ -> (
      (* try matching decl_asst *)
      match match_var_decl_asst tokens with
      | Some vd, r -> (Some (DECL_ASST vd), r)
      | _ -> (None, tokens))

and match_var_decl_asst (tokens : Lexer.token list) :
    decl_asst option * Lexer.token list =
  (* decl_asst -> dtype asst 
              -> mut dtype asst 
  *)
  (* immutable decl *)
  match tokens with
  | DATATYPE dtype :: rem -> (
      match match_asst rem with
      | Some at, r -> (Some { mut = false; dtype; asst = at }, r)
      | _ -> (None, tokens))
  (* mutable decl *)
  | MUT :: DATATYPE dtype :: rem -> (
      match match_asst rem with
      | Some at, r -> (Some { mut = true; dtype; asst = at }, r)
      | _ -> (None, tokens))
  | _ -> (None, tokens)

and match_var_decl_helper (tokens : Lexer.token list) (dtype : Lexer.dtype)
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

(* variable assignment *)
and match_asst (tokens : Lexer.token list) : asst option * Lexer.token list =
  (* asst -> var_name "=" expr ";" *)
  match tokens with
  | IDENTIFIER name :: ASSIGNMENT :: rem -> (
      match match_expr rem with
      | Some e, SEMICOLON :: r -> (Some { name = IDENT name; expr = e }, r)
      | _, _ -> (None, tokens))
  | _ -> (None, tokens)

and format_asst (a : asst option) : string =
  match a with
  | Some x ->
      let name_str = match x.name with IDENT n -> n in
      name_str ^ " = " ^ format_expr x.expr
  | None -> "Not found"

(* Expression *)
and match_expr (tokens : Lexer.token list) : expr option * Lexer.token list =
  (* expr -> ce | term (("+" | "-") term)* *)

  (* try to match term *)
  match match_term tokens with
  | None, _ -> (
      (* try to match cond_expr *)
      match match_cond_expr tokens with
      | Some ce, rem -> (Some (COND ce), rem)
      | _ -> (None, tokens))
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
  (* term -> factor (("/" | "*" | "%" | "^") factor)* *)

  (* try to match factor *)
  match match_factor tokens with
  | None, _ -> (None, tokens)
  | Some left, rem ->
      (* find all possible factors *)
      let rec loop (t : Lexer.token list) (left : expr) :
          expr option * Lexer.token list =
        match t with
        | ((MULTIPLY | DIVIDE | MOD | POW) as op) :: r -> (
            match match_factor r with
            | Some right, rem_tok ->
                let left = construct_expr op left right in
                loop rem_tok left
            | _ -> (Some left, t))
        | _ -> (Some left, t)
      in
      loop rem left

and match_factor (tokens : Lexer.token list) : expr option * Lexer.token list =
  (* factor -> Identifier | Number | Identifier++ | func_call | "(" expr ")" *)
  match tokens with
  (* try to match identifier++ *)
  | IDENTIFIER i :: INCREMENT :: rem -> (Some (INCREMENT (IDENT i)), rem)
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

(* conditional expression *)
and match_cond_expr (tokens : Lexer.token list) :
    cond_expr option * Lexer.token list =
  (* cond_expr -> boolval (logical_combination boolval)* *)

  (* try to match boolval *)
  match match_boolval tokens with
  | Some bv, rem_tokens ->
      (* try to match all (logical_combination boolval) pairs *)
      let rec loop (t : Lexer.token list) (prev : cond_expr) :
          cond_expr option * Lexer.token list =
        match t with
        (* found operator *)
        | LOGICAL_AND :: r -> (
            match match_boolval r with
            | Some bv, rem -> loop rem (COMBINATION (LOGICAL_AND, prev, bv))
            | _ -> (None, t))
        | LOGICAL_OR :: r -> (
            match match_boolval r with
            | Some bv, rem -> loop rem (COMBINATION (LOGICAL_OR, prev, bv))
            | _ -> (None, t))
        (* no operator found; return cond_expr found so far *)
        | _ -> (Some prev, t)
      in
      loop rem_tokens bv
  | _ -> (None, tokens)

and match_valcmp (tokens : Lexer.token list) :
    cond_expr option * Lexer.token list =
  (* valcmp -> val logical_compare val *)

  (* try to match val (operand1)*)
  match match_value tokens with
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
          | Some op2, r -> (Some (BOOL_VAL (VAL_CMP (op, op1, op2))), r)
          | _ -> (None, tokens))
      | None -> (None, tokens))
  | _ -> (None, tokens)

and match_boolval (tokens : Lexer.token list) :
    cond_expr option * Lexer.token list =
  (* 
  boolval = bool
          = val logical_compare val  
          = "(" cond_expr ")"
  *)
  match tokens with
  | [] -> (None, tokens)
  (* try to match bool *)
  | BOOLEAN b :: rem -> (Some (BOOL_VAL (BOOL b)), rem)
  (* try to "(" cond_expr ")" *)
  | L_PAREN :: rem -> (
      match match_cond_expr rem with
      | Some ce, R_PAREN :: r -> (Some ce, r)
      | _ -> (None, tokens))
  (* try to match val logical_compare val *)
  | token :: rem -> match_valcmp tokens

(* Functions *)
(* function declaration *)
and match_func_decl (tokens : Lexer.token list) :
    func_decl option * Lexer.token list =
  (* func_decl -> ret_type func_name "(" param_list ")" block 
               -> option ret_type func_name "(" param_list ")" block 
  *)
  match tokens with
  | OPTION :: Lexer.DATATYPE d :: IDENTIFIER fname :: L_PAREN :: rem ->
      match_func_decl_helper rem (DATATYPE d) fname true
  | Lexer.DATATYPE d :: IDENTIFIER fname :: L_PAREN :: rem ->
      match_func_decl_helper rem (DATATYPE d) fname false
  | VOID :: IDENTIFIER fname :: L_PAREN :: rem ->
      match_func_decl_helper rem VOID fname false
  | _ -> (None, tokens)

and match_func_decl_helper (tokens : Lexer.token list) (ret : ret_type)
    (fname : string) (is_optional : bool) : func_decl option * Lexer.token list
    =
  (* helps match func_decl -> param_list ")" block *)

  (* try to match param_list *)
  match match_param_list tokens with
  | params, R_PAREN :: rem -> (
      (* try to match block *)
      match match_block rem with
      | Some blk, r ->
          ( Some
              {
                func_name = IDENT fname;
                return_type = ret;
                params;
                is_optional_type = is_optional;
                body = blk;
              },
            r )
      | _ -> (None, tokens))
  | _ -> (None, tokens)

and match_ret_expr (tokens : Lexer.token list) :
    ret_expr option * Lexer.token list =
  (* ret_expr -> return expr ";"  
              -> return Some expr ";"      
              -> return None ";"      
  *)
  match tokens with
  (* try to match return None ";" *)
  | RETURN :: NONE :: SEMICOLON :: rem -> (Some NONE, rem)
  (* try to match return Some expr ";"  *)
  | RETURN :: SOME :: rem -> (
      match match_expr rem with
      | Some e, SEMICOLON :: r -> (Some (SOME_EXPR e), r)
      | _ -> (None, tokens))
  (* try to match return expr ";" *)
  | RETURN :: rem -> (
      match match_expr rem with
      | Some e, SEMICOLON :: r -> (Some (EXPR e), r)
      | _ -> (None, tokens))
  | _ -> (None, tokens)

and match_param_list (tokens : Lexer.token list) : param_list * Lexer.token list
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

(* function call *)
and match_func_call (tokens : Lexer.token list) :
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

and match_arg_list (tokens : Lexer.token list) :
    arg_list option * Lexer.token list =
  (* arg_list -> [] | expr ("," expr)* *)
  (* try to match expr *)
  match match_expr tokens with
  | Some e, [] -> (Some [ e ], [])
  | Some e, rem ->
      (* match as as many (",", value) pairs *)
      let rec loop (t : Lexer.token list) (accum : arg_list) :
          arg_list option * Lexer.token list =
        match t with
        (* try to match comma *)
        | COMMA :: ts -> (
            (* try to match another value *)
            match match_expr ts with
            | Some exp, r -> loop r (exp :: accum)
            | _, _ -> (Some (List.rev accum), t))
        | _ -> (Some (List.rev accum), t)
      in
      loop rem [ e ]
  (* empty arglist *)
  | _, _ -> (Some [], tokens)

(* Statement *)
and match_stmt (tokens : Lexer.token list) : stmt list * Lexer.token list =
  (* stmt -> (var_decl | asst | loop | expr ";") | func_call | ctrl_flow)* *)
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
  (* match a single statement (stmt = (var_decl | asst | loop | expr ";" | return expr ";" | func_call | ctrl_flow )* *)

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
              | _ -> (
                  (* try to match return expr *)
                  match match_ret_expr tokens with
                  | Some rt, rtok -> (Some (RETURN rt), rtok)
                  (* try to match ctrl flow *)
                  | _ -> match_ctrl_flow tokens))))

and match_ctrl_flow (tokens : Lexer.token list) : stmt option * Lexer.token list
    =
  (* ctrl_flow -> if cond block 
               -> if cond block else block
  *)
  match tokens with
  (* try to match "if"*)
  | IF :: rem -> (
      (* try to match cond *)
      match match_cond_expr rem with
      | Some exp, r -> (
          (* try to match block *)
          match match_block r with
          (* try to match "else" block *)
          | Some if_blk, ELSE :: rem_tok -> (
              match match_block rem_tok with
              | Some else_blk, rtok -> (Some (IF_ELSE (if_blk, else_blk)), rtok)
              | _ -> (None, tokens))
          | Some if_blk, rem_tok -> (Some (IF if_blk), rem_tok)
          | _ -> (None, tokens))
      | _ -> (None, tokens))
  | _ -> (None, tokens)

(* Block *)
and match_block (tokens : Lexer.token list) : block option * Lexer.token list =
  (* block -> "{" stmt "}" *)
  match tokens with
  (* try to match "{" *)
  | L_BRACE :: rem_tokens -> (
      (* try to stmt *)
      match match_stmt rem_tokens with
      (* try to match "}" *)
      | s :: st, R_BRACE :: rem -> (Some (STMTS (s :: st)), rem)
      | _ -> (None, tokens))
  | _ -> (None, tokens)

(* Loops *)
and match_loop (tokens : Lexer.token list) : loop option * Lexer.token list =
  (* loop -> while cond_expr block 
          -> for "(" var_init  cond_expr ";" update_expr ")" block

    var_init = asst | decl_asst 
  *)
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
  (* try to match for *)
  | FOR :: L_PAREN :: rem_tokens -> (
      (* try to match var_init *)
      match match_asst rem_tokens with
      | Some at, rem -> for_loop_helper rem (ASST at)
      | _ -> (
          match match_var_decl_asst rem_tokens with
          | Some dcl, r -> for_loop_helper r (DECL_ASST dcl)
          | _ -> (None, tokens)))
  | _ -> (None, tokens)

and for_loop_helper (tokens : Lexer.token list) (init : var_init) :
    loop option * Lexer.token list =
  (* helps match  cond_expr ";" update_expr ")" block *)

  (* try to match cond_expr ";" *)
  match match_cond_expr tokens with
  | Some cond, SEMICOLON :: rem -> (
      (* try to match update_expr ")" *)
      match match_expr rem with
      | Some update, R_PAREN :: r -> (
          (* try to match block *)
          match match_block r with
          | Some body, rtok -> (Some (FOR { init; cond; update; body }), rtok)
          | _ -> (None, tokens))
      | _ -> (None, tokens))
  | _ -> (None, tokens)

(* Program *)
let parse_prog (tokens : Lexer.token list) : prog * Lexer.token list =
  (* prog -> func_decl* *)

  (* remove whitespace *)
  let tok = List.filter (fun tok -> tok != Lexer.WHITESPACE) tokens in

  (* gather all the functions *)
  let rec loop (t : Lexer.token list) (accum : func_decl list) :
      prog * Lexer.token list =
    match match_func_decl t with
    | Some fd, r -> loop r (fd :: accum)
    | _ -> ( match accum with [] -> ([], t) | _ -> (List.rev accum, t))
  in

  loop tok []
