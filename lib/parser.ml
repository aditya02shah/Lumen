exception Invalid_expr

(* --TYPE DECLARATIONS-- *)
type parser_errors = string list

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

(* --HELPER FUNCTIONS-- *)
let rec format_expr expr =
  match expr with
  | INT n, _ -> Printf.sprintf "INT(%d)" n
  | INCREMENT (IDENT i), _ -> Printf.sprintf "INCREMENT IDENT(%s)" i
  | VAR (IDENT i), _ -> Printf.sprintf "IDENT(%s)" i
  | FUNC_CALL fc, _ -> "FUNCCALL"
  | COND cd, _ -> "CE"
  | BINARY (op, e1, e2), _ ->
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

let construct_expr (op : Lexer.located_token) (op1 : expr) (op2 : expr) : expr =
  match op with
  | ADD, pos -> (BINARY (ADD, op1, op2), pos)
  | SUBTRACT, pos -> (BINARY (SUBTRACT, op1, op2), pos)
  | MULTIPLY, pos -> (BINARY (MULTIPLY, op1, op2), pos)
  | DIVIDE, pos -> (BINARY (DIVIDE, op1, op2), pos)
  | MOD, pos -> (BINARY (MOD, op1, op2), pos)
  | POW, pos -> (BINARY (POW, op1, op2), pos)
  | _ -> raise Invalid_expr

(* --PARSER IMPLEMENTATION-- *)
(* Basic Units *)
let match_value (tokens : Lexer.located_token list) :
    value option * Lexer.located_token list * parser_errors =
  (* value -> IDENTIFIER | INT | BOOLEAN *)

  (* --valid paths-- *)
  match tokens with
  | (Lexer.NUMBER n, pos) :: rem -> (Some (NUMBER n, pos), rem, [])
  | (Lexer.BOOLEAN b, pos) :: rem -> (Some (BOOLEAN b, pos), rem, [])
  | (Lexer.IDENTIFIER i, pos) :: rem ->
      (Some (IDENTIFIER (IDENT i), pos), rem, [])
  (* --invalid paths-- *)
  | (tok, pos) :: r ->
      ( None,
        tokens,
        [
          Utils.format_msg pos "Value: expected int, bool or variable. Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | [] -> (None, tokens, [])

(* Variables *)
(* variable declaration *)
let rec match_var_decl (tokens : Lexer.located_token list) :
    var_decl option * Lexer.located_token list * parser_errors =
  (* var_decl -> dtype var_name ("," var_name)* ";"
              -> mut dtype var_name ("," var_name)* ";"
              -> decl_asst
  *)
  match tokens with
  (* try matching var_decl -> dtype var_name ("," var_name)* ";"
                           -> mut dtype var_name ("," var_name)* ";" *)
  (* single var decl *)

  (* --valid paths-- *)
  | (DATATYPE dtype, pos) :: (IDENTIFIER vname, _) :: (SEMICOLON, _) :: rem ->
      ( Some (DECL ({ mut = false; dtype; vars = [ IDENT vname ] }, pos)),
        rem,
        [] )
  | (MUT, pos)
    :: (DATATYPE dtype, _)
    :: (IDENTIFIER vname, _)
    :: (SEMICOLON, _)
    :: rem ->
      (Some (DECL ({ mut = true; dtype; vars = [ IDENT vname ] }, pos)), rem, [])
  (* multiple var decl *)
  | (DATATYPE dtype, _) :: (IDENTIFIER vname, _) :: (COMMA, p) :: rem ->
      match_var_decl_helper ((COMMA, p) :: rem) dtype vname false
  | (MUT, _)
    :: (DATATYPE dtype, _)
    :: (IDENTIFIER vname, _)
    :: (COMMA, p)
    :: rem ->
      match_var_decl_helper ((COMMA, p) :: rem) dtype vname true
  | _ -> (
      (* try matching decl_asst *)
      match match_var_decl_asst tokens with
      | Some vd, r, _ -> (Some (DECL_ASST vd), r, [])
      (* --invalid paths-- *)
      | None, r, decl_errs -> (
          match tokens with
          | (DATATYPE dtype, pos) :: (IDENTIFIER vname, _) :: (tok, _) :: rem
          | (MUT, _)
            :: (DATATYPE dtype, pos)
            :: (IDENTIFIER vname, _)
            :: (tok, _)
            :: rem ->
              ( None,
                tokens,
                [
                  Utils.format_msg pos
                    "Variable Declaration: expected ';' or ',' identifier. \
                     Found "
                  ^ Lexer.format_token tok ^ "!";
                ]
                @ decl_errs )
          | (DATATYPE dtype, pos) :: (tok, _) :: rem
          | (MUT, _) :: (DATATYPE dtype, pos) :: (tok, _) :: rem ->
              ( None,
                tokens,
                [
                  Utils.format_msg pos
                    "Variable Declaration: expected identifier. Found "
                  ^ Lexer.format_token tok ^ "!";
                ]
                @ decl_errs )
          | _ -> (None, tokens, decl_errs)))

and match_var_decl_asst (tokens : Lexer.located_token list) :
    decl_asst option * Lexer.located_token list * parser_errors =
  (* decl_asst -> dtype asst 
              -> mut dtype asst 
  *)
  (* immutable decl *)
  match tokens with
  | (DATATYPE dtype, pos) :: rem -> (
      (* --valid path-- *)
      match match_asst rem with
      | Some at, r, _ -> (Some ({ mut = false; dtype; asst = at }, pos), r, [])
      | None, _, asst_errs ->
          (* --invalid path-- *)
          ( None,
            tokens,
            [ Utils.format_msg pos "Variable Declaration: invalid assignment!" ]
            @ asst_errs ))
  (* mutable decl *)
  | (MUT, pos) :: (DATATYPE dtype, _) :: rem -> (
      (* --valid path-- *)
      match match_asst rem with
      | Some at, r, _ -> (Some ({ mut = true; dtype; asst = at }, pos), r, [])
      | None, _, asst_errs ->
          (* --invalid path-- *)
          ( None,
            tokens,
            [ Utils.format_msg pos "Variable Declaration: invalid assignment!" ]
            @ asst_errs ))
  (* --invalid paths-- *)
  | (MUT, pos) :: rem ->
      ( None,
        tokens,
        [ Utils.format_msg pos "Variable Declaration: invalid declaration!" ] )
  | _ -> (None, tokens, [])

and match_var_decl_helper (tokens : Lexer.located_token list)
    (dtype : Lexer.dtype) (vname : string) (is_mutable : bool) :
    var_decl option * Lexer.located_token list * parser_errors =
  (* find all var_name declarations matching ("," var_name)* *)
  let rec loop (t : Lexer.located_token list) (accum : ident list) :
      var_decl option * Lexer.located_token list * parser_errors =
    match (t, accum) with
    (* --valid paths-- *)
    | (SEMICOLON, pos) :: ts, a :: acs ->
        ( Some (DECL ({ mut = false; dtype; vars = List.rev (a :: acs) }, pos)),
          ts,
          [] )
    | (COMMA, _) :: (IDENTIFIER v, _) :: r, a :: acs -> loop r (IDENT v :: accum)
    (* --invalid paths-- *)
    | (COMMA, pos) :: (tok, _) :: r, a :: acs ->
        ( None,
          tokens,
          [
            Utils.format_msg pos
              "Variable Declaration: expected identifier. Found "
            ^ Lexer.format_token tok ^ "!";
          ] )
    | _, _ -> (None, tokens, [])
  in
  loop tokens [ IDENT vname ]

(* variable assignment *)
and match_asst (tokens : Lexer.located_token list) :
    asst option * Lexer.located_token list * parser_errors =
  (* asst -> var_name "=" expr ";" *)
  match tokens with
  | (IDENTIFIER name, pos) :: (ASSIGNMENT, _) :: rem -> (
      match match_expr rem with
      | Some e, (SEMICOLON, _) :: r, _ ->
          (* --valid path-- *)
          (Some ({ name = IDENT name; expr = e }, pos), r, [])
          (* --invalid path-- *)
      | _, _, exp_errs ->
          ( None,
            tokens,
            [ Utils.format_msg pos "Assignment: invalid expression!" ]
            @ exp_errs ))
  (* --invalid paths-- *)
  | (IDENTIFIER name, pos) :: (tok, _) :: _ ->
      ( None,
        tokens,
        [
          Utils.format_msg pos "Assignment: expected '='. Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | _ -> (None, tokens, [])

and format_asst (a : asst option) : string =
  match a with
  | Some (x, _) ->
      let name_str = match x.name with IDENT n -> n in
      name_str ^ " = " ^ format_expr x.expr
  | None -> "Not found"

(* Expression *)
and match_expr (tokens : Lexer.located_token list) :
    expr option * Lexer.located_token list * parser_errors =
  (* expr -> ce | term (("+" | "-") term)* *)

  (* try to match term *)
  match match_term tokens with
  | None, _, term_errs -> (
      (* try to match cond_expr *)
      match match_cond_expr tokens with
      | Some ce, rem, ce_errs -> (
          (* determine source positino *)
          match ce with
          | BOOL_VAL (_, pos) | COMBINATION (_, pos) ->
              (Some (COND ce, pos), rem, ce_errs))
      | _ -> (None, tokens, []))
  | Some left, rem, term_errs ->
      (* find all possible terms *)
      let rec loop (t : Lexer.located_token list) (left : expr) :
          expr option * Lexer.located_token list * parser_errors =
        match t with
        (* maintain left-associativity *)
        | ((ADD, _ | SUBTRACT, _) as op) :: r -> (
            match match_term r with
            | Some right, rem_tok, e ->
                let left = construct_expr op left right in
                loop rem_tok left
            | _ -> (Some left, t, term_errs))
        | _ -> (Some left, t, term_errs)
      in
      loop rem left

and match_term (tokens : Lexer.located_token list) :
    expr option * Lexer.located_token list * parser_errors =
  (* term -> factor (("/" | "*" | "%" | "^") factor)* *)

  (* try to match factor *)
  match match_factor tokens with
  (* --valid path-- *)
  | Some left, rem, factor_errs ->
      (* find all possible factors *)
      let rec loop (t : Lexer.located_token list) (left : expr) :
          expr option * Lexer.located_token list * parser_errors =
        match t with
        (* maintain left-associativity *)
        | ((MULTIPLY, pos | DIVIDE, pos | MOD, pos | POW, pos) as op) :: r -> (
            match match_factor r with
            | Some right, rem_tok, es ->
                let left = construct_expr op left right in
                loop rem_tok left
            | _, _, f_errs ->
                ( Some left,
                  t,
                  [ Utils.format_msg pos "Expression: invalid factor!" ]
                  @ f_errs ))
        | (op, pos) :: r ->
            ( Some left,
              t,
              [
                Utils.format_msg pos "Expression: invalid operation("
                ^ Lexer.format_token op ^ ")!";
              ] )
        | [] -> (Some left, t, factor_errs)
      in
      loop rem left
      (* --invalid path-- *)
  | None, _, factor_errs -> (None, tokens, factor_errs)

and match_factor (tokens : Lexer.located_token list) :
    expr option * Lexer.located_token list * parser_errors =
  (* factor -> Identifier | Number | Identifier++ | func_call | "(" expr ")" *)

  (* --valid paths-- *)
  match tokens with
  (* try to match identifier++ *)
  | (IDENTIFIER i, pos) :: (INCREMENT, _) :: rem ->
      (Some (INCREMENT (IDENT i), pos), rem, [])
  (* try to match func_call *)
  | (IDENTIFIER i, pos) :: (L_PAREN, _) :: r -> (
      match match_func_call tokens pos with
      | Some fc, rem, _ -> (Some (FUNC_CALL fc, pos), rem, [])
      (* --invalid path-- *)
      | _, _, fcall_errs ->
          ( None,
            tokens,
            [ Utils.format_msg pos "Expression: invalid function call!" ]
            @ fcall_errs ))
  (* try to match an identifier *)
  | (IDENTIFIER i, pos) :: rem -> (Some (VAR (IDENT i), pos), rem, [])
  (* try to match a number *)
  | (NUMBER i, pos) :: rem -> (Some (INT i, pos), rem, [])
  (* try to match "(" *)
  | (L_PAREN, pos) :: rem -> (
      (* try to match an expression *)
      match match_expr rem with
      (* try to match ")" *)
      | Some e, (Lexer.R_PAREN, _) :: r, _ -> (Some e, r, [])
      (* --invalid path-- *)
      | _, _, exp_errs ->
          ( None,
            tokens,
            [ Utils.format_msg pos "Expression: invalid sub-expression!" ]
            @ exp_errs ))
  (* --invalid path-- *)
  | _ -> (None, tokens, [])

(* conditional expression *)
and match_cond_expr (tokens : Lexer.located_token list) :
    cond_expr option * Lexer.located_token list * parser_errors =
  (* cond_expr -> boolval (logical_combination boolval)* *)

  (* try to match boolval *)
  match match_boolval tokens with
  | Some bv, rem_tokens, bv_errs ->
      (* try to match all (logical_combination boolval) pairs *)
      let rec loop (t : Lexer.located_token list) (prev : cond_expr) :
          cond_expr option * Lexer.located_token list * parser_errors =
        match t with
        (* maintain left associativity *)
        | (LOGICAL_AND, pos) :: r -> (
            (* --valid path-- *)
            match match_boolval r with
            | Some bv, rem, _ ->
                loop rem (COMBINATION ((LOGICAL_AND, prev, bv), pos))
            (* --invalid path-- *)
            | _ ->
                ( None,
                  t,
                  [
                    Utils.format_msg pos
                      "Conditional Expression: invalid boolean value after \
                       'and' !";
                  ]
                  @ bv_errs ))
        | (LOGICAL_OR, pos) :: r -> (
            (* --valid path-- *)
            match match_boolval r with
            | Some bv, rem, _ ->
                loop rem (COMBINATION ((LOGICAL_OR, prev, bv), pos))
            (* --invalid path-- *)
            | _ ->
                ( None,
                  t,
                  [
                    Utils.format_msg pos
                      "Conditional Expression: invalid boolean value after \
                       'or' !";
                  ]
                  @ bv_errs ))
        (* no operator found; return cond_expr found so far *)
        | _ -> (Some prev, t, bv_errs)
      in
      loop rem_tokens bv
  (* --invalid path-- *)
  | _ -> (None, tokens, [])

and match_boolval (tokens : Lexer.located_token list) :
    cond_expr option * Lexer.located_token list * parser_errors =
  (* 
  boolval -> bool
          -> valcmp
          -> "(" cond_expr ")"

  valcmp -> val logical_compare val
  *)
  (* --valid paths-- *)
  match tokens with
  | [] -> (None, tokens, [])
  (* try to match bool *)
  | (BOOLEAN b, pos) :: rem -> (Some (BOOL_VAL (BOOL b, pos)), rem, [])
  (* try to "(" cond_expr ")" *)
  | (L_PAREN, pos) :: rem -> (
      (* --valid path-- *)
      match match_cond_expr rem with
      | Some ce, (R_PAREN, _) :: r, ce_errs -> (Some ce, r, ce_errs)
      (* --invalid path-- *)
      | Some ce, (tok, _) :: r, ce_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos "Boolean value: expected ')'. Found "
              ^ Lexer.format_token tok ^ "!";
            ]
            @ ce_errs )
      | _, _, ce_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos
                "Boolean value: invalid conditional expression!";
            ]
            @ ce_errs ))
  (* try to match val logical_compare val *)
  | (token, pos) :: rem -> match_valcmp tokens pos

and match_valcmp (tokens : Lexer.located_token list) (pos : Lexer.position) :
    cond_expr option * Lexer.located_token list * parser_errors =
  (* valcmp -> val logical_compare val *)

  (* try to match val (operand1)*)
  match match_value tokens with
  | Some op1, (tok, p) :: rem, val1_errs -> (
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
          (* --valid path-- *)
          | Some op2, r, _ ->
              (Some (BOOL_VAL (VAL_CMP (op, op1, op2), p)), r, [])
          (* --invalid path-- *)
          | None, _, val2_errs ->
              ( None,
                tokens,
                [
                  Utils.format_msg p
                    "Value comparison: second operand is invalid!!";
                ]
                @ val2_errs @ val1_errs ))
      (* --invalid path-- *)
      | None ->
          ( None,
            tokens,
            [ Utils.format_msg p "Value comparison: invalid operation!" ]
            @ val1_errs ))
  (* --invalid path-- *)
  | _ -> (None, tokens, [])

(* Functions *)
(* function declaration *)
and match_func_decl (tokens : Lexer.located_token list) :
    func_decl option * Lexer.located_token list * parser_errors =
  (* func_decl -> ret_type func_name "(" param_list ")" block 
               -> option ret_type func_name "(" param_list ")" block 
  *)
  match tokens with
  (* --valid paths-- *)
  | (OPTION, pos)
    :: (Lexer.DATATYPE d, _)
    :: (IDENTIFIER fname, _)
    :: (L_PAREN, _)
    :: rem ->
      match_func_decl_helper rem (DATATYPE d, pos) fname true pos
  | (Lexer.DATATYPE d, pos) :: (IDENTIFIER fname, _) :: (L_PAREN, _) :: rem ->
      match_func_decl_helper rem (DATATYPE d, pos) fname false pos
  | (VOID, pos) :: (IDENTIFIER fname, _) :: (L_PAREN, _) :: rem ->
      match_func_decl_helper rem (VOID, pos) fname false pos
  (* --invalid paths-- *)
  | (OPTION, pos)
    :: (Lexer.DATATYPE d, _)
    :: (IDENTIFIER fname, _)
    :: (tok, _)
    :: rem
  | (Lexer.DATATYPE d, pos) :: (IDENTIFIER fname, _) :: (tok, _) :: rem ->
      ( None,
        tokens,
        [
          Utils.format_msg pos "Function declaration: expected '('. Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | (OPTION, pos) :: (Lexer.DATATYPE d, _) :: (tok, _) :: rem
  | (Lexer.DATATYPE d, pos) :: (tok, _) :: rem ->
      ( None,
        tokens,
        [
          Utils.format_msg pos
            "Function declaration: expected function name (identifier). Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | (OPTION, pos) :: (tok, _) :: rem ->
      ( None,
        tokens,
        [
          Utils.format_msg pos
            "Function declaration: expected return type (datatype). Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | (VOID, pos) :: (IDENTIFIER fname, _) :: (tok, _) :: rem ->
      ( None,
        tokens,
        [
          Utils.format_msg pos "Function declaration: expected '('. Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | (VOID, pos) :: (tok, _) :: rem ->
      ( None,
        tokens,
        [
          Utils.format_msg pos
            "Function declaration: expected function name (identifier). Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | _ -> (None, tokens, [])

and match_func_decl_helper (tokens : Lexer.located_token list) (ret : ret_type)
    (fname : string) (is_optional : bool) (pos : Lexer.position) :
    func_decl option * Lexer.located_token list * parser_errors =
  (* helps match func_decl -> param_list ")" block *)

  (* try to match param_list *)
  match match_param_list tokens with
  | params, (R_PAREN, p) :: rem, _ -> (
      (* try to match block *)
      match match_block rem with
      (* --valid path-- *)
      | Some blk, r, blk_errs ->
          ( Some
              ( {
                  func_name = IDENT fname;
                  return_type = ret;
                  params;
                  is_optional_type = is_optional;
                  body = blk;
                },
                p ),
            r,
            [] )
      (* --invalid path-- *)
      | _, _, blk_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg p "Function declaration: contains invalid block!";
            ]
            @ blk_errs ))
  (* --invalid path-- *)
  | _, _, param_errs ->
      ( None,
        tokens,
        [
          Utils.format_msg pos
            "Function declaration: contains invalid parameter list!";
        ]
        @ param_errs )

and match_ret_expr (tokens : Lexer.located_token list) :
    ret_expr option * Lexer.located_token list * parser_errors =
  (* ret_expr -> return expr ";"  
              -> return Some expr ";"      
              -> return None ";"      
  *)
  match tokens with
  (* try to match return None ";" *)
  | (RETURN, pos) :: (NONE, _) :: (SEMICOLON, _) :: rem ->
      (* --valid path-- *)
      (Some (NONE, pos), rem, [])
  | (RETURN, pos) :: (NONE, _) :: (tok, _) :: rem ->
      (* --invalid path-- *)
      ( None,
        tokens,
        [
          Utils.format_msg pos "Return expression: expected ';'. Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  (* try to match return Some expr ";"  *)
  | (RETURN, pos) :: (SOME, _) :: rem -> (
      (* --valid path-- *)
      match match_expr rem with
      | Some e, (SEMICOLON, _) :: r, _ -> (Some (SOME_EXPR e, pos), r, [])
      (* --invalid paths-- *)
      | Some e, (tok, _) :: r, exp_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos "Return expression: expected ';'. Found "
              ^ Lexer.format_token tok ^ "!";
            ]
            @ exp_errs )
      | _, _, exp_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos
                "Return expression: contains invalid expression!";
            ]
            @ exp_errs ))
  (* try to match return expr ";" *)
  | (RETURN, pos) :: rem -> (
      (* --valid path-- *)
      match match_expr rem with
      | Some e, (SEMICOLON, _) :: r, _ -> (Some (EXPR e, pos), r, [])
      (* --invalid paths-- *)
      | Some e, (tok, _) :: r, exp_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos "Return expression: expected ';'. Found "
              ^ Lexer.format_token tok ^ "!";
            ]
            @ exp_errs )
      | _, _, exp_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos
                "Return expression: contains invalid expression!";
            ]
            @ exp_errs ))
  | _ -> (None, tokens, [])

and match_param_list (tokens : Lexer.located_token list) :
    param_list * Lexer.located_token list * parser_errors =
  (* paramList -> [] | dtype Identifier ("," dtype Identifier)* *)
  match tokens with
  (* --valid paths-- *)
  | (DATATYPE d, _) :: (IDENTIFIER pname, _) :: rem ->
      (* try to match dtype *)
      let rec loop (t : Lexer.located_token list) (accum : param_list) :
          param_list * Lexer.located_token list * parser_errors =
        (* match all possible (dtype identifier) pairs *)
        match t with
        | (COMMA, _) :: (DATATYPE dt, _) :: (IDENTIFIER p, _) :: r ->
            loop r ((dt, IDENT p) :: accum)
        | (COMMA, pos) :: (DATATYPE dt, _) :: (tok, _) :: r ->
            ( List.rev accum,
              t,
              [
                Utils.format_msg pos
                  "Parameter list: expected parameter name (identifier). Found "
                ^ Lexer.format_token tok ^ "!";
              ] )
        | (COMMA, pos) :: (tok, _) :: r ->
            ( List.rev accum,
              t,
              [
                Utils.format_msg pos
                  "Parameter list: expected parameter type (datatype). Found "
                ^ Lexer.format_token tok ^ "!";
              ] )
        | _ -> (List.rev accum, t, [])
      in
      loop rem [ (d, IDENT pname) ]
  (* --invalid paths-- *)
  | (DATATYPE d, pos) :: (tok, _) :: rem ->
      ( [],
        tokens,
        [
          Utils.format_msg pos
            "Parameter list: expected parameter name (identifier). Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | (tok, pos) :: rem ->
      ( [],
        tokens,
        [
          Utils.format_msg pos
            "Parameter list: expected parameter type (datatype). Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | _ -> ([], tokens, [])

(* function call *)
and match_func_call (tokens : Lexer.located_token list) (pos : Lexer.position) :
    func_call option * Lexer.located_token list * parser_errors =
  (* func_call -> func_name "(" arg_list ")" ";" *)

  (* try to match func_name *)
  match tokens with
  | (IDENTIFIER fname, p) :: (L_PAREN, _) :: rem -> (
      (* try to match ")" & ";" *)
      (* --valid path-- *)
      match match_arg_list rem p with
      (* --invalid paths-- *)
      | Some args, (Lexer.R_PAREN, _) :: r, _ ->
          (Some ({ func_name = IDENT fname; args }, p), r, [])
      | Some args, (tok, _) :: r, _ ->
          ( None,
            tokens,
            [
              Utils.format_msg p "Function call: expected ')'. Found! "
              ^ Lexer.format_token tok ^ "!";
            ] )
      | _, _, arg_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg p
                "Function call: contains invalid argument list!";
            ]
            @ arg_errs ))
  (* --invalid paths-- *)
  | (IDENTIFIER fname, p) :: (tok, _) :: rem ->
      ( None,
        tokens,
        [
          Utils.format_msg p "Function call: expected '('. Found! "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | (tok, p) :: rem ->
      ( None,
        tokens,
        [
          Utils.format_msg p
            "Function call: expected function name (identifier). Found! "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | [] ->
      (None, tokens, [ Utils.format_msg pos "Function call: invalid syntax!" ])

and match_arg_list (tokens : Lexer.located_token list) (pos : Lexer.position) :
    arg_list option * Lexer.located_token list * parser_errors =
  (* arg_list -> [] | expr ("," expr)* *)
  (* try to match expr *)
  match match_expr tokens with
  | Some e, [], _ -> (Some [ e ], [], [])
  | Some e, rem, exp1_errs ->
      (* match as as many (",", expr) pairs *)
      let rec loop (t : Lexer.located_token list) (accum : arg_list) :
          arg_list option * Lexer.located_token list * parser_errors =
        match t with
        (* try to match comma *)
        | (COMMA, _) :: ts -> (
            (* try to match another expr *)
            match match_expr ts with
            | Some exp, r, _ -> loop r (exp :: accum)
            | _, _, exp2_errs ->
                (Some (List.rev accum), t, exp2_errs @ exp1_errs))
        | (tok, p) :: ts ->
            ( Some (List.rev accum),
              t,
              [
                Utils.format_msg p
                  "Argument list: expected ',' expression. Found "
                ^ Lexer.format_token tok ^ "!";
              ]
              @ exp1_errs )
        | _ -> (Some (List.rev accum), t, exp1_errs)
      in
      loop rem [ e ]
  | None, (tok, p) :: r, exp_errs ->
      ( Some [],
        tokens,
        [
          Utils.format_msg p "Argument list: expected expression. Found "
          ^ Lexer.format_token tok ^ "!";
        ]
        @ exp_errs )
      (* empty arglist *)
  | None, [], exp_errs ->
      ( None,
        tokens,
        [ Utils.format_msg pos "Argument list: no arguments provided!" ]
        @ exp_errs )

(* Statement *)
and match_stmt (tokens : Lexer.located_token list) :
    stmt list * Lexer.located_token list * parser_errors =
  (* stmt -> (var_decl | asst | loop | expr ";") | func_call | ctrl_flow)* *)
  let rec loop (t : Lexer.located_token list) (accum : stmt list) :
      stmt list * Lexer.located_token list * parser_errors =
    (* match all valid stmts *)
    match match_stmt_helper t with
    | Some st, r, _ -> loop r (st :: accum)
    | None, r, stmt_errs -> (
        match accum with
        | [] -> ([ EMPTY ], tokens, stmt_errs)
        | a :: acs -> (List.rev (a :: acs), r, stmt_errs))
  in
  loop tokens []

and match_stmt_helper (tokens : Lexer.located_token list) :
    stmt option * Lexer.located_token list * parser_errors =
  (* match a single statement (stmt = (var_decl | asst | loop | expr ";" | return expr ";" | func_call | ctrl_flow )* *)

  (* try to match var_decl *)
  match match_var_decl tokens with
  | Some vd, rem_tokens, vd_errs -> (Some (VARDECL vd), rem_tokens, vd_errs)
  | _, _, vd_errs -> (
      (* try to match asst *)
      match match_asst tokens with
      | Some at, rem, at_errs -> (Some (ASST at), rem, at_errs)
      | _, _, at_errs -> (
          (* try to match loop *)
          match match_loop tokens with
          | Some lp, r, lp_errs -> (Some (LOOP lp), r, lp_errs)
          | _, _, lp_errs -> (
              (* try to match expr *)
              match match_expr tokens with
              | Some e, (SEMICOLON, _) :: rem_tok, exp_errs ->
                  (Some (EXP e), rem_tok, exp_errs)
              | Some e, (tok, pos) :: rem_tok, exp_errs ->
                  ( None,
                    tokens,
                    [
                      Utils.format_msg pos "Expression: expected ';'. Found "
                      ^ Lexer.format_token tok ^ "!";
                    ]
                    @ exp_errs )
              | _, _, exp_errs -> (
                  (* try to match return expr *)
                  match match_ret_expr tokens with
                  | Some rt, rtok, ret_errs -> (Some (RETURN rt), rtok, ret_errs)
                  (* try to match ctrl flow *)
                  | _, _, ret_errs ->
                      match_ctrl_flow tokens
                        (* provide errors encountered so far *)
                        (vd_errs @ at_errs @ lp_errs @ exp_errs @ ret_errs)))))

and match_ctrl_flow (tokens : Lexer.located_token list)
    (prev_errs : parser_errors) :
    stmt option * Lexer.located_token list * parser_errors =
  (* ctrl_flow -> if cond block 
               -> if cond block else block
  *)
  match tokens with
  (* try to match "if"*)
  | (IF, pos) :: rem -> (
      (* try to match cond *)
      match match_cond_expr rem with
      | Some if_cond, r, ce_errs -> (
          (* try to match block *)
          match match_block r with
          (* try to match "else" block *)
          | Some if_blk, (ELSE, _) :: rem_tok, _ -> (
              match match_block rem_tok with
              | Some else_blk, rtok, _ ->
                  ( Some (IF_ELSE ((if_cond, if_blk), else_blk)),
                    rtok,
                    ce_errs @ prev_errs )
              | _, _, blk_errs ->
                  ( None,
                    tokens,
                    [
                      Utils.format_msg pos
                        "If-Else: contains invalid else block!";
                    ]
                    @ blk_errs @ ce_errs @ prev_errs ))
          | Some if_blk, rem_tok, _ ->
              (Some (IF (if_cond, if_blk)), rem_tok, prev_errs)
          | _, _, blk_errs ->
              ( None,
                tokens,
                [ Utils.format_msg pos "If: contains invalid block!" ]
                @ blk_errs @ ce_errs @ prev_errs ))
      | _, _, ce_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos
                "If: contains invalid conditional expression!";
            ]
            @ ce_errs @ prev_errs ))
  | _ -> (None, tokens, prev_errs)

(* Block *)
and match_block (tokens : Lexer.located_token list) :
    block option * Lexer.located_token list * parser_errors =
  (* block -> "{" stmt "}" *)
  match tokens with
  (* try to match "{" *)
  | (L_BRACE, pos) :: rem_tokens -> (
      (* try to stmt *)
      match match_stmt rem_tokens with
      (* try to match "}" *)
      (* --valid path-- *)
      | st, (R_BRACE, _) :: rem, _ -> (Some (STMTS st, pos), rem, [])
      (* --invalid paths-- *)
      | _, (tok, p) :: r, stmt_errs ->
          ( None,
            tokens,
            [ Utils.format_msg p "Block: contains invalid statement!" ]
            @ stmt_errs )
      | _, _, stmt_errs -> (None, tokens, stmt_errs))
  | _ -> (None, tokens, [])

(* Loops *)
and match_loop (tokens : Lexer.located_token list) :
    loop option * Lexer.located_token list * parser_errors =
  (* loop -> while cond_expr block 
          -> for "(" var_init  cond_expr ";" update_expr ")" block

    var_init = asst | decl_asst 
  *)
  match tokens with
  (* try to match while *)
  (* --valid paths-- *)
  | (WHILE, pos) :: rem_tokens -> (
      (* try to match cond *)
      match match_cond_expr rem_tokens with
      | Some exp, rem, ce_errs -> (
          (* try to match block *)
          match match_block rem with
          | Some b, r, blk_errs ->
              let loop = { cond = exp; body = b } in
              (Some (WHILE loop, pos), r, ce_errs)
          | _, _, blk_errs ->
              ( None,
                tokens,
                [ Utils.format_msg pos "While loop: contains invalid block!" ]
                @ blk_errs @ ce_errs ))
      | _, _, ce_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos
                "While loop: contains invalid conditional expression!";
            ]
            @ ce_errs ))
  (* try to match for *)
  | (FOR, pos) :: (L_PAREN, _) :: rem_tokens -> (
      (* try to match var_init *)
      match match_asst rem_tokens with
      | Some at, rem, asst_errs -> for_loop_helper rem (ASST at, pos)
      | _, _, asst_errs -> (
          match match_var_decl_asst rem_tokens with
          | Some dcl, r, decl_errs -> for_loop_helper r (DECL_ASST dcl, pos)
          | _, _, decl_errs ->
              ( None,
                tokens,
                [
                  Utils.format_msg pos
                    "For loop: contains invalid initialization!";
                ]
                @ decl_errs @ asst_errs )))
  (* --invalid paths-- *)
  | (FOR, pos) :: (tok, _) :: rem_tokens ->
      ( None,
        tokens,
        [
          Utils.format_msg pos "For loop: expected '('. Found "
          ^ Lexer.format_token tok ^ "!";
        ] )
  | _ -> (None, tokens, [])

and for_loop_helper (tokens : Lexer.located_token list) (init : var_init) :
    loop option * Lexer.located_token list * parser_errors =
  (* helps match cond_expr ";" update_expr ")" block *)

  (* try to match cond_expr ";" *)

  (* --valid paths-- *)
  match match_cond_expr tokens with
  | Some cond, (SEMICOLON, pos) :: rem, ce_errs -> (
      (* try to match update_expr ")" *)
      match match_expr rem with
      | Some update, (R_PAREN, _) :: r, upd_errs -> (
          (* try to match block *)
          match match_block r with
          | Some body, rtok, blk_errs ->
              ( Some (FOR { init; cond; update; body }, pos),
                rtok,
                blk_errs @ blk_errs @ upd_errs @ ce_errs )
          (* --invalid paths-- *)
          | _, _, blk_errs ->
              ( None,
                tokens,
                [ Utils.format_msg pos "For loop: contains invalid block!" ]
                @ blk_errs ))
      | Some update, (tok, _) :: r, upd_errs ->
          ( None,
            tokens,
            [
              Utils.format_msg pos "For loop: expected ')'. Found "
              ^ Lexer.format_token tok ^ "!";
            ]
            @ upd_errs )
      | None, _, upd_errs ->
          ( None,
            tokens,
            [ Utils.format_msg pos "For loop: contains  update expression!" ]
            @ upd_errs )
      | _ -> (None, tokens, []))
  (* --invalid paths-- *)
  | Some cond, (tok, pos) :: rem, errs ->
      ( None,
        tokens,
        [
          Utils.format_msg pos "For loop: expected ';'. Found "
          ^ Lexer.format_token tok ^ "!";
        ]
        @ errs )
  | _, _, errs -> (None, tokens, errs)

(* --PUBLIC INTERFACE-- *)
(* Program *)
let parse_prog (tokens : Lexer.located_token list) :
    prog * Lexer.located_token list * parser_errors =
  (* prog -> func_decl* *)

  (* remove whitespace *)
  let tok = List.filter (fun (tok, pos) -> tok != Lexer.WHITESPACE) tokens in

  (* collect all the function declarations *)
  let rec loop (t : Lexer.located_token list) (accum : func_decl list) :
      prog * Lexer.located_token list * parser_errors =
    match match_func_decl t with
    | Some (fd, pos), r, _ -> loop r ((fd, pos) :: accum)
    | _, _, fd_errs -> (
        match accum with
        | [] -> ([], t, fd_errs)
        | _ -> (List.rev accum, t, fd_errs))
  in

  loop tok []
