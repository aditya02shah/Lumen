(* expression *)

type operator =
  | ADD
  | MULTIPLY
  | SUBTRACT
  | DIVIDE
  | MOD
  | POW
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN_EQUALS
  | GREATER_THAN
  | LOGICAL_AND
  | LOGICAL_OR

type expr =
  (* terminals *)
  | INT of int
  | STRING of string
  (* non-terminals *)
  (* unary *)
  | INCREMENT of expr
  (* binary *)
  | BINARY of operator * expr * expr

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
        | LESS_THAN -> "LESS_THAN"
        | LESS_THAN_EQUALS -> "LESS_THAN_EQUALS"
        | GREATER_THAN -> "GREATER_THAN"
        | GREATER_THAN_EQUALS -> "GREATER_THAN_EQUALS"
        | LOGICAL_AND -> "LOGICAL_AND"
        | LOGICAL_OR -> "LOGICAL_OR"
      in
      Printf.sprintf "BINARY(%s, %s, %s)" op_str (format_expr e1)
        (format_expr e2)

let rec peek (t : Lexer.token list) (expected : Lexer.token) : bool =
  match t with
  | [] -> false
  | token :: rem -> if token = expected then true else false

let rec match_expr (t : Lexer.token list) : expr option * Lexer.token list =
  match t with
  | [] -> (None, t)
  | token :: rem -> (
      match match_term (token :: rem) with
      | Some t1, r1 :: r2 ->
          if peek (r1 :: r2) Lexer.ADD = true then
            match match_term r2 with
            | Some t2, r3 -> (Some (BINARY (ADD, t1, t2)), r3)
            | _ -> (None, rem)
          else if peek (r1 :: r2) Lexer.SUBTRACT = true then
            match match_term r2 with
            | Some t2, r3 -> (Some (BINARY (SUBTRACT, t1, t2)), r3)
            | _ -> (None, rem)
          else (None, rem)
      | _, _ -> (None, rem))

and match_term (t : Lexer.token list) : expr option * Lexer.token list =
  match t with
  | [] -> (None, t)
  | token :: rem -> (
      match match_factor (token :: rem) with
      | Some f1, r1 :: r2 ->
          if peek (r1 :: r2) Lexer.MULTIPLY = true then
            match match_factor r2 with
            | Some f2, r3 -> (Some (BINARY (MULTIPLY, f1, f2)), r3)
            | _ -> (None, rem)
          else if peek (r1 :: r2) Lexer.DIVIDE = true then
            match match_factor r2 with
            | Some f2, r3 -> (Some (BINARY (DIVIDE, f1, f2)), r3)
            | _ -> (None, rem)
          else (None, rem)
      | _, _ -> (None, rem))

and match_factor (t : Lexer.token list) : expr option * Lexer.token list =
  match t with
  | [] -> (None, t)
  | token :: rem -> (
      match token with
      | NUMBER i -> (Some (INT i), rem)
      | L_PAREN -> (
          match match_expr rem with
          | Some e, r1 :: r2 ->
              if peek (r1 :: r2) R_PAREN = true then (Some e, r2)
              else (None, rem)
          | _, _ -> (None, rem))
      | _ -> (None, rem))
