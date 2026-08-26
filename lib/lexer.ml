(* --type declarations-- *)
type dtype = INT | BOOL (* supported datatypes *)

type token =
  (* data *)
  | NUMBER of int
  | IDENTIFIER of string
  (* keywords *)
  | FOR
  | WHILE
  | RETURN
  | MUT
  (* syntax *)
  | DATATYPE of dtype
  | OPTION
  | SOME
  | NONE
  | WHITESPACE
  | SEMICOLON
  | L_PAREN
  | R_PAREN
  | L_BRACE
  | R_BRACE
  | L_BRACKET
  | R_BRACKET
  | COMMA
  (* operations *)
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | MOD
  | POW
  | EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN_EQUALS
  | GREATER_THAN
  | LOGICAL_AND
  | LOGICAL_OR
  | INCREMENT
  | UNKNOWN of string (* TODO *)

type partial_token =
  (* helps handle tokens spanning multiple characters *)
  | DIGIT
  | CHAR
  | WS

type accum =
  (* type representing accumlated tokens *)
  | Accum of char list * partial_token
  | Empty

exception Unknown_token

let int_of_char_list chars =
  chars |> List.to_seq |> String.of_seq |> int_of_string

let string_of_char_list chars = chars |> List.to_seq |> String.of_seq
let string_to_char_list str = List.init (String.length str) (String.get str)

let rec format_token (t : token) : string =
  match t with
  (* data *)
  | NUMBER n -> "NUMBER " ^ string_of_int n
  | IDENTIFIER x -> "IDENTIFIER: " ^ x
  (* keywords *)
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | RETURN -> "RETURN"
  | MUT -> "MUT"
  (* syntax *)
  | DATATYPE d -> (
      match d with INT -> "DATATYPE: INT" | BOOL -> "DATATYPE: BOOL")
  | OPTION -> "'OPTION'"
  | SOME -> "'SOME'"
  | NONE -> "'NONE'"
  | WHITESPACE -> "WHITESPACE"
  | SEMICOLON -> "';'"
  | L_PAREN -> "'('"
  | R_PAREN -> "')'"
  | L_BRACE -> "'{'"
  | R_BRACE -> "'}'"
  | L_BRACKET -> "'['"
  | R_BRACKET -> "']'"
  | COMMA -> "','"
  (* operations *)
  | ADD -> "+"
  | SUBTRACT -> "'-'"
  | MULTIPLY -> "'*'"
  | DIVIDE -> "'/'"
  | MOD -> "'%'"
  | POW -> "'^'"
  | EQUALS -> "'='"
  | NOT_EQUALS -> "!="
  | LESS_THAN -> "'<'"
  | LESS_THAN_EQUALS -> "'<="
  | GREATER_THAN_EQUALS -> "'>="
  | GREATER_THAN -> "'>'"
  | LOGICAL_AND -> "AND"
  | LOGICAL_OR -> "OR"
  | INCREMENT -> "'INCREMENT'"
  | UNKNOWN x -> "<UNK ='" ^ x ^ "'>"

let rec print_list (format_element : 'a -> string) (l : 'a list) : unit =
  (* prints every element of the list *)
  match l with
  | [] -> ()
  | x :: xs ->
      print_endline (format_element x);
      print_list format_element xs

let rec merge_partial_token (acc : accum) : token option =
  (* tries to combines partial tokens into a complete token *)
  match acc with
  | Empty -> None
  | Accum (chars, tok_type) -> (
      match tok_type with
      | DIGIT -> Some (NUMBER (int_of_char_list (List.rev chars)))
      | CHAR -> Some (IDENTIFIER (string_of_char_list (List.rev chars)))
      | WS -> Some WHITESPACE)

let rec match_characters (chars : char list) (target : char list) :
    char list option =
  (* match target to chars and return rest of chars *)
  match (chars, target) with
  | [], [] -> Some []
  | rest, [] -> Some rest
  | [], t -> None
  | c :: cs, t :: ts -> if c = t then match_characters cs ts else None

let rec get_token (chars : char list) (acc : accum) : token option * char list =
  (* match token and returns the rest of the characters *)
  match (chars, acc) with
  | [], Empty -> (None, [])
  | [], acc -> (merge_partial_token acc, [])
  | c :: rest, Empty -> (
      match c with
      | 'O' -> (
          (* Option *)
          match match_characters rest (string_to_char_list "ption") with
          | Some remaining -> (Some OPTION, remaining)
          | None -> get_token rest (Accum ([ c ], CHAR)))
      | 'S' -> (
          (* Some *)
          match match_characters rest (string_to_char_list "ome") with
          | Some remaining -> (Some SOME, remaining)
          | None -> get_token rest (Accum ([ c ], CHAR)))
      | 'N' -> (
          (* None *)
          match match_characters rest (string_to_char_list "one") with
          | Some remaining -> (Some NONE, remaining)
          | None -> get_token rest (Accum ([ c ], CHAR)))
      (* words *)
      | 'a' .. 'z' | 'A' .. 'Z' -> get_token rest (Accum ([ c ], CHAR))
      (* numbers *)
      | '0' .. '9' -> get_token rest (Accum ([ c ], DIGIT))
      (* whitespace *)
      | ' ' | '\t' | '\n' | '\r' -> get_token rest (Accum ([ c ], WS))
      (* syntax *)
      | ';' -> (Some SEMICOLON, rest)
      | '(' -> (Some L_PAREN, rest)
      | ')' -> (Some R_PAREN, rest)
      | '{' -> (Some L_BRACE, rest)
      | '}' -> (Some R_BRACE, rest)
      | '[' -> (Some L_BRACKET, rest)
      | ']' -> (Some R_BRACKET, rest)
      | ',' -> (Some COMMA, rest)
      (* operations *)
      | '+' -> (
          match rest with
          | '+' :: rem -> (Some INCREMENT, rem)
          | _ -> (Some ADD, rest))
      | '-' -> (Some SUBTRACT, rest)
      | '*' -> (Some MULTIPLY, rest)
      | '/' -> (Some DIVIDE, rest)
      | '^' -> (Some POW, rest)
      | '%' -> (Some MOD, rest)
      | '=' -> (Some EQUALS, rest)
      | '>' -> (Some GREATER_THAN, rest)
      | '<' -> (Some LESS_THAN, rest)
      | _ -> (None, c :: rest))
  | c :: rest, Accum (partials, tok_type) -> (
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' -> (
          match tok_type with
          | CHAR -> get_token rest (Accum (c :: partials, tok_type))
          | _ -> (merge_partial_token acc, c :: rest))
      | '0' .. '9' -> (
          match tok_type with
          | DIGIT -> get_token rest (Accum (c :: partials, tok_type))
          | _ -> (merge_partial_token acc, c :: rest))
      | ' ' | '\t' | '\n' | '\r' -> (
          match tok_type with
          | WS -> get_token rest (Accum (c :: partials, tok_type))
          | _ -> (merge_partial_token acc, c :: rest))
      | _ -> (merge_partial_token acc, c :: rest))

let rec tokenize_helper (accum : token list) (prog : string list) : token list =
  (* helps tokenizes input list *)
  match prog with
  | [] -> accum
  | word :: rest -> (
      match word with
      (* keywords *)
      | "for" -> tokenize_helper (FOR :: accum) rest
      | "while" -> tokenize_helper (WHILE :: accum) rest
      | "return" -> tokenize_helper (RETURN :: accum) rest
      | "mut" -> tokenize_helper (MUT :: accum) rest
      (* syntax *)
      | "int" -> tokenize_helper (DATATYPE INT :: accum) rest
      | "bool" -> tokenize_helper (DATATYPE BOOL :: accum) rest
      | "Option" -> tokenize_helper (OPTION :: accum) rest
      | "Some" -> tokenize_helper (SOME :: accum) rest
      | "None" -> tokenize_helper (NONE :: accum) rest
      (* | _ -> raise Unknown_token) *)
      (* operations *)
      | "!=" -> tokenize_helper (NOT_EQUALS :: accum) rest
      | "<=" -> tokenize_helper (LESS_THAN_EQUALS :: accum) rest
      | ">=" -> tokenize_helper (GREATER_THAN_EQUALS :: accum) rest
      | "and" -> tokenize_helper (LOGICAL_AND :: accum) rest
      | "or" -> tokenize_helper (LOGICAL_OR :: accum) rest
      | _ -> (
          (* data *)
          match get_token (string_to_char_list word) Empty with
          | None, [] -> tokenize_helper (UNKNOWN word :: accum) rest
          | None, r ->
              tokenize_helper (UNKNOWN word :: accum)
                (string_of_char_list r :: rest)
          | Some t, [] -> tokenize_helper (t :: accum) rest
          | Some t, r ->
              tokenize_helper (t :: accum) (string_of_char_list r :: rest)))

let rec tokenize (prog : string) : token list =
  (* processes input string & tokenizes it *)
  String.split_on_char ' ' prog (* split program based on whitespace *)
  |> List.filter (fun s -> s <> "") (* remove empty strings *)
  |> tokenize_helper [] |> List.rev (* tokenize*)
