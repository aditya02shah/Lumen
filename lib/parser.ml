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

(* functions *)
type argList = value list
type paramList = (Lexer.dtype * ident) list
type funcCall = { funcName : ident; args : argList }

let rec format_expr expr =
  match expr with
  | INT n -> Printf.sprintf "INT(%d)" n
  | STRING s -> Printf.sprintf "STRING(%S)" s
  | INCREMENT e -> Printf.sprintf "INCREMENT(%s)" (format_expr e)
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

let match_next_token (t : Lexer.token list) (expected : Lexer.token) : bool =
  (* returns true if expected matches first token in list, false otherwise *)
  match t with
  | [] -> false
  | token :: rem -> if token = expected then true else false

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
  (* factor -> Number | "(" expr ")" *)
  match tokens with
  | [] -> (None, tokens)
  | token :: rem_tokens -> (
      match token with
      (* try to match a number *)
      | NUMBER i -> (Some (INT i), rem_tokens)
      (* try to match ( *)
      | L_PAREN -> (
          (* try to match an expression *)
          match match_expr rem_tokens with
          | Some e, Lexer.R_PAREN :: r -> (Some e, r) (* try to match ) *)
          | _, _ -> (None, token :: rem_tokens))
      | _ -> (None, token :: rem_tokens))

(* ASSIGNMENT *)
let format_asst (a : asst option) : string =
  match a with
  | Some x ->
      let name_str = match x.name with IDENT n -> n in
      name_str ^ " = " ^ format_expr x.expr
  | None -> "Not found"

let match_ident (tokens : Lexer.token list) : ident option * Lexer.token list =
  (* IDENT -> string *)
  match tokens with
  | [] -> (None, tokens)
  | token :: rem_tokens -> (
      (* try to match IDENT *)
      match token with
      | IDENTIFIER name -> (Some (IDENT name), rem_tokens)
      | _ -> (None, tokens))

let match_asst (tokens : Lexer.token list) : asst option * Lexer.token list =
  (* asst -> varName "=" expr ";" *)
  match tokens with
  | [] -> (None, tokens)
  | token :: rem_tokens -> (
      (* try to match varname *)
      match match_ident (token :: rem_tokens) with
      | Some name, ASSIGNMENT :: rem -> (
          (* try to match expr *)
          match match_expr rem with
          | Some e, SEMICOLON :: r -> (Some { name; expr = e }, r)
          | _, _ -> (None, tokens))
      | _ -> (None, tokens))

(* VARIABLE DECLARATION *)
let match_vardecl (tokens : Lexer.token list) :
    varDecl option * Lexer.token list =
  (* vardecl -> dtype varname ("," varname)* ";" *)
  match tokens with
  (* try to match dtype *)
  | DATATYPE dtype :: rem_tokens -> (
      (* try to match varname *)
      match match_ident rem_tokens with
      | Some vname, rtok :: rem ->
          (* check for semicolon *)
          if match_next_token (rtok :: rem) Lexer.SEMICOLON then
            (Some { dtype; vars = [ vname ] }, rem)
          else
            (* look for more varname declarations *)
            let rec loop (t : Lexer.token list) (accum : ident list) :
                varDecl option * Lexer.token list =
              match (t, accum) with
              | SEMICOLON :: ts, a :: acs ->
                  (Some { dtype; vars = List.rev (a :: acs) }, ts)
              | COMMA :: ts, a :: acs -> (
                  (* try to match varname *)
                  match match_ident ts with
                  | Some v, r -> loop r (v :: accum)
                  | _, _ -> (None, tokens))
              | _, _ -> (None, tokens)
            in
            loop (rtok :: rem) [ vname ]
      | _ -> (None, tokens))
  | _ -> (None, tokens)

(* ARGUMENT LIST *)
let match_value (tokens : Lexer.token list) : value option * Lexer.token list =
  (* value = IDENTIFIER | INT | BOOLEAN *)
  match tokens with
  | [] -> (None, [])
  | token :: rem_tokens -> (
      match token with
      | Lexer.NUMBER n -> (Some (NUMBER n), rem_tokens)
      | Lexer.BOOLEAN b -> (Some (BOOLEAN b), rem_tokens)
      | Lexer.IDENTIFIER i -> (Some (IDENTIFIER (IDENT i)), rem_tokens)
      | _ -> (None, token :: rem_tokens))

let match_arglist (tokens : Lexer.token list) :
    argList option * Lexer.token list =
  (* arglist -> [] | value ("," value)* *)
  match tokens with
  | [] -> (Some [], tokens)
  | token :: rem_tokens -> (
      (* try to match value *)
      match match_value tokens with
      | Some v, [] -> (Some [ v ], [])
      | Some v, rem ->
          (* match as as many (",", value) pairs *)
          let rec loop (t : Lexer.token list) (accum : argList) :
              argList option * Lexer.token list =
            match t with
            (* try to match comma *)
            | COMMA :: ts -> (
                (* try to match another value *)
                match match_value ts with
                | Some newval, r -> loop r (newval :: accum)
                | _, _ ->
                    (None, token :: rem_tokens)
                    (* return arguments matched so far *))
            | _ -> (Some (List.rev accum), t)
          in
          loop rem [ v ]
      (* empty arglist *)
      | _, _ -> (Some [], token :: rem_tokens))

(* FUNCTION CALL *)
let match_funcall (tokens : Lexer.token list) :
    funcCall option * Lexer.token list =
  (* funcCall -> funcName "(" argList ") ";" "*)
  match tokens with
  | [] -> (None, tokens)
  | token :: rem_tokens -> (
      (* try to match funcName *)
      match match_ident (token :: rem_tokens) with
      | Some fname, rem_tok :: rem -> (
          if
            (* try to match "(" *)
            match_next_token (rem_tok :: rem) Lexer.L_PAREN = false
          then (None, tokens)
          else
            (* try to match ")" & ";" *)
            match match_arglist rem with
            | Some args, Lexer.R_PAREN :: Lexer.SEMICOLON :: r ->
                (Some { funcName = fname; args }, r)
            | _ -> (None, tokens))
      | _, _ -> (None, tokens))

(* PARAMETER LIST *)
let match_paramList (tokens : Lexer.token list) :
    paramList option * Lexer.token list =
  (* paramList -> [] | dtype Identifier ("," dtype Identifier)* *)
  match tokens with
  | token :: t :: rem_tokens -> (
      (* try to match dtype *)
      match token with
      | DATATYPE d -> (
          (* try to match identifier *)
          match t with
          | IDENTIFIER i ->
              let rec loop (t : Lexer.token list) (accum : paramList) :
                  paramList option * Lexer.token list =
                (* match all possible (dtype identifier) pairs *)
                match t with
                | COMMA :: DATATYPE dt :: IDENTIFIER it :: r ->
                    loop r ((dt, IDENT it) :: accum)
                | _ -> (Some (List.rev accum), tokens)
              in
              loop rem_tokens [ (d, IDENT i) ]
          | _ -> (Some [], tokens))
      | _ -> (Some [], tokens))
  | _ -> (Some [], tokens)

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
  | token :: rem_tokens -> (
      (* try to match valcmp *)
      match match_valcmp tokens with
      | Some v, rem -> (Some v, rem)
      | _ -> (
          (* try to match boolcmp *)
          match match_boolcmp tokens with
          | Some bv, r -> (Some bv, r)
          | _ -> (None, tokens)))

let rec match_condexpr (tokens : Lexer.token list) :
    cond_expr option * Lexer.token list =
  match tokens with
  (* 
  condexpr = boolval
           = boolval (logical_combination condexpr)
  *)
  | token :: rem_tokens -> (
      (* try to match boolval *)
      match match_boolval tokens with
      | Some v, [] -> (Some (BOOL_VAL v), [])
      (* try to match boolval (logical_combination condexpr) *)
      | Some v, LOGICAL_AND :: r -> (
          (* try to match condexpr *)
          match match_condexpr r with
          | Some c, rem -> (Some (COMBINATION (LOGICAL_AND, v, c)), rem)
          | _ -> (Some (BOOL_VAL v), LOGICAL_AND :: r))
      | Some v, LOGICAL_OR :: r -> (
          (* try to match condexpr *)
          match match_condexpr r with
          | Some c, rem -> (Some (COMBINATION (LOGICAL_OR, v, c)), rem)
          | _ -> (Some (BOOL_VAL v), LOGICAL_OR :: r))
      | _ -> (None, tokens))
  | _ -> (None, tokens)
