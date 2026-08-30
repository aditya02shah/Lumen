(* expression *)

type arithmetic_op = ADD | MULTIPLY | SUBTRACT | DIVIDE | MOD | POW

type expr =
  | INT of int
  | STRING of string
  | INCREMENT of expr
  | BINARY of arithmetic_op * expr * expr
(* TODO: logical ops *)

exception Invalid_expr

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
        (* | LESS_THAN -> "LESS_THAN"
        | LESS_THAN_EQUALS -> "LESS_THAN_EQUALS"
        | GREATER_THAN -> "GREATER_THAN"
        | GREATER_THAN_EQUALS -> "GREATER_THAN_EQUALS"
        | LOGICAL_AND -> "LOGICAL_AND"
        | LOGICAL_OR -> "LOGICAL_OR" *)
      in
      Printf.sprintf "BINARY(%s, %s, %s)" op_str (format_expr e1)
        (format_expr e2)

let rec match_next_token (t : Lexer.token list) (expected : Lexer.token) : bool
    =
  (* returns true if expected matches first token in list, false otherwise *)
  match t with
  | [] -> false
  | token :: rem -> if token = expected then true else false

(* EXPRESSION *)
let rec match_expr (tokens : Lexer.token list) : expr list * Lexer.token list =
  (* returns an expression along with the remaining tokens *)
  (* expr -> term (("+" | "-") term)* *)
  match tokens with
  | [] -> ([], tokens)
  | token :: rem_tokens -> (
      (* try to match a term *)
      match match_term (token :: rem_tokens) with
      | e :: es, r ->
          (* found a term; handle all occurrences of (("+" | "-") term) *)
          let rec loop (t : Lexer.token list) (accum : expr list) :
              expr list * Lexer.token list =
            match t with
            | [] -> (accum, [])
            | x :: xs -> (
                (* try to find an operator *)
                let op =
                  if match_next_token (x :: xs) Lexer.ADD = true then Some ADD
                  else if match_next_token (x :: xs) Lexer.SUBTRACT = true then
                    Some SUBTRACT
                  else None
                in
                match op with
                | Some op -> (
                    match (match_term xs, accum) with
                    | (subexpr :: exprs, rem), a :: acs ->
                        (* try to match more occurrences *)
                        loop rem ((BINARY (op, a, subexpr) :: exprs) @ acs)
                    | (tok :: ts, rem), [] -> raise Invalid_expr
                    | _, _ -> (accum, x :: xs))
                | None -> (accum, x :: xs))
          in
          loop r (e :: es)
      | [], [] -> ([], [])
      | [], r -> ([], r))

and match_term (tokens : Lexer.token list) : expr list * Lexer.token list =
  (* returns an expression along with the remaining tokens *)
  (* expr -> term (("+" | "-") term)* *)
  match tokens with
  | [] -> ([], tokens) (* no expression found *)
  | token :: rem_tokens -> (
      (* try to match a factor *)
      match match_factor (token :: rem_tokens) with
      | e :: es, r ->
          (* found a factor; handle all occurrences of (("*" | "/") factor) *)
          let rec loop (t : Lexer.token list) (accum : expr list) :
              expr list * Lexer.token list =
            match t with
            | [] -> (accum, [])
            | x :: xs -> (
                (* try to find an operator *)
                let op =
                  if match_next_token (x :: xs) Lexer.MULTIPLY = true then
                    Some MULTIPLY
                  else if match_next_token (x :: xs) Lexer.DIVIDE = true then
                    Some DIVIDE
                  else None
                in
                match op with
                | Some op -> (
                    match (match_factor xs, accum) with
                    | (f :: fs, rem), a :: acs ->
                        (* try to match more occurrences *)
                        loop rem ((BINARY (op, a, f) :: fs) @ acs)
                    | (f :: fs, rem), [] -> raise Invalid_expr
                    | _, _ -> (accum, x :: xs))
                | None -> (accum, x :: xs))
          in
          loop r (e :: es)
      | [], [] -> ([], [])
      | [], r -> ([], r))

and match_factor (tokens : Lexer.token list) : expr list * Lexer.token list =
  (* returns a factor along with the remaining tokens *)
  (* factor -> Number | "(" expr ")" *)
  match tokens with
  | [] -> ([], tokens)
  | token :: rem_tokens -> (
      match token with
      (* try to match a number *)
      | NUMBER i -> ([ INT i ], rem_tokens)
      (* try to match ( *)
      | L_PAREN -> (
          (* try to match an expression *)
          match match_expr rem_tokens with
          | e :: es, t :: r ->
              (* try to match ) *)
              if peek (t :: r) R_PAREN = true then (e :: es, r)
              else ([], token :: rem_tokens)
          | _, _ -> ([], token :: rem_tokens))
      | _ -> ([], token :: rem_tokens))
