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
  (* returns true if expected matches first token in list, false otherwise *)
  match t with
  | [] -> false
  | token :: rem -> if token = expected then true else false

let rec match_expr (tokens : Lexer.token list) : expr list * Lexer.token list =
  (* returns an expression along with the remaining tokens *)
  (* expr -> term (("+" | "-") term)* *)
  match tokens with
  | [] -> ([], tokens)
  | token :: rem_tokens -> (
      (* match a term *)
      match match_term (token :: rem_tokens) with
      | e :: es, r ->
          let rec loop (t : Lexer.token list) (accum : expr list) :
              expr list * Lexer.token list =
            match t with
            | [] -> (accum, [])
            | x :: xs -> (
                let op =
                  if peek (x :: xs) Lexer.ADD = true then Some ADD
                  else if peek (x :: xs) Lexer.SUBTRACT = true then
                    Some SUBTRACT
                  else None
                in
                match op with
                | Some op -> (
                    match (match_term xs, accum) with
                    | (subexpr :: exprs, rem), a :: acs ->
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
      (* match a term *)
      match match_factor (token :: rem_tokens) with
      | Some e, r ->
          let rec loop (t : Lexer.token list) (accum : expr list) :
              expr list * Lexer.token list =
            match t with
            | [] -> (accum, [])
            | x :: xs -> (
                let op =
                  if peek (x :: xs) Lexer.MULTIPLY = true then Some MULTIPLY
                  else if peek (x :: xs) Lexer.DIVIDE = true then Some DIVIDE
                  else None
                in
                match op with
                | Some op -> (
                    match (match_factor xs, accum) with
                    | (Some subexpr, rem), a :: acs ->
                        loop rem (BINARY (op, a, subexpr) :: acs)
                    | (Some subexpr, rem), [] -> raise Invalid_expr
                    | _, _ -> (accum, x :: xs))
                | None -> (accum, x :: xs))
          in
          loop r [ e ]
      | _, _ -> ([], token :: rem_tokens))

and match_factor (t : Lexer.token list) : expr option * Lexer.token list =
  (* returns a factor along with the remaining tokens *)
  (* factor -> Number | "(" expr ")" *)
  match t with
  | [] -> (None, t)
  | token :: rem -> (
      match token with
      (* match a number *)
      | NUMBER i -> (Some (INT i), rem)
      (* match parenthesis *)
      (* | L_PAREN -> (
          match match_expr rem with
          | Some e, r1 :: r2 ->
              if peek (r1 :: r2) R_PAREN = true then (Some e, r2)
              else (None, rem)
          | _, _ -> (None, rem)) *)
      | _ -> (None, rem))
