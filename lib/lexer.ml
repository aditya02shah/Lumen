(* --TYPE DECLARATIONS-- *)
type dtype = INT | BOOL (* supported datatypes *)

(* Recognised tokens *)
type token =
  (* data *)
  | NUMBER of int
  | BOOLEAN of bool
  | IDENTIFIER of string
  (* keywords *)
  | FOR
  | WHILE
  | RETURN
  | MUT
  | IF
  | ELSE
  (* syntax *)
  | DATATYPE of dtype
  | VOID
  | OPTION
  | SOME
  | NONE
  | ASSIGNMENT
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
  | INCREMENT
  | EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | LESS_THAN_EQUALS
  | GREATER_THAN_EQUALS
  | GREATER_THAN
  | LOGICAL_AND
  | LOGICAL_OR
  | NOT
  | UNKNOWN of string (* unrecognized tokens *)

type partial_token =
  (* helps handle tokens spanning multiple characters *)
  | DIGIT
  | CHAR
  | NEGATIVE_DIGIT
  | WS

type accum =
  (* type representing accumlated tokens *)
  | Accum of char list * partial_token
  | Empty

(* Position-aware types *)
type position = int (* line number *)
type located_str = string * position (* source strings *)
type located_token = token * position (* source tokens *)
type lexer_errors = located_token list (* source errors *)

(* --HELPER FUNCTIONS-- *)
let int_of_char_list chars =
  chars |> List.to_seq |> String.of_seq |> int_of_string

let string_of_char_list chars = chars |> List.to_seq |> String.of_seq
let string_to_char_list str = List.init (String.length str) (String.get str)

let format_dtype (dt : dtype) : string =
  match dt with INT -> "dtype('Int')" | BOOL -> "dtype('Bool')"

let format_token (t : token) : string =
  match t with
  (* data *)
  | NUMBER n -> "Int literal " ^ string_of_int n
  | BOOLEAN b -> if b = true then "'true'" else "'false''"
  | IDENTIFIER x -> "identifier: " ^ x
  (* keywords *)
  | FOR -> "'for'"
  | WHILE -> "'while"
  | RETURN -> "'return'"
  | MUT -> "mut'"
  | IF -> "if'"
  | ELSE -> "else'"
  (* syntax *)
  | DATATYPE d -> (
      match d with INT -> "dtype('Int')" | BOOL -> "dtype('Bool')")
  | VOID -> "'void'"
  | OPTION -> "'option'"
  | SOME -> "'some'"
  | NONE -> "'none'"
  | ASSIGNMENT -> "'='"
  | WHITESPACE -> "whitespace"
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
  | INCREMENT -> "'++'"
  | EQUALS -> "'=='"
  | NOT_EQUALS -> "!="
  | LESS_THAN -> "'<'"
  | LESS_THAN_EQUALS -> "'<="
  | GREATER_THAN_EQUALS -> "'>="
  | GREATER_THAN -> "'>'"
  | LOGICAL_AND -> "and"
  | LOGICAL_OR -> "or"
  | NOT -> "not"
  | UNKNOWN x -> "<UNK ='" ^ x ^ "'>"

let rec print_list (format_element : 'a -> string) (l : 'a list) : unit =
  (* prints every element of the list *)
  match l with
  | [] -> ()
  | x :: xs ->
      print_endline (format_element x);
      print_list format_element xs

(* --LEXER IMPLEMENTATION-- *)
let merge_partial_token (acc : accum) : token option =
  (* tries to combines partial tokens into a complete token *)
  match acc with
  | Empty -> None
  | Accum (chars, tok_type) -> (
      match tok_type with
      | DIGIT -> Some (NUMBER (int_of_char_list (List.rev chars)))
      | NEGATIVE_DIGIT -> Some (NUMBER (-int_of_char_list (List.rev chars)))
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
      | 't' -> (
          (* "true" *)
          match match_characters rest (string_to_char_list "rue") with
          | Some remaining -> (Some (BOOLEAN true), remaining)
          | None -> get_token rest (Accum ([ c ], CHAR)))
      | 'f' -> (
          (* "false" *)
          match match_characters rest (string_to_char_list "alse") with
          | Some remaining -> (Some (BOOLEAN false), remaining)
          | None -> get_token rest (Accum ([ c ], CHAR)))
      (* words *)
      | 'a' .. 'z' | 'A' .. 'Z' -> get_token rest (Accum ([ c ], CHAR))
      (* numbers *)
      | '0' .. '9' -> (
          match acc with
          | Accum (_, NEGATIVE_DIGIT) ->
              get_token rest (Accum ([ c ], NEGATIVE_DIGIT))
          | _ -> get_token rest (Accum ([ c ], DIGIT)))
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
      | '-' -> get_token rest (Accum ([], NEGATIVE_DIGIT))
      | '*' -> (Some MULTIPLY, rest)
      | '/' -> (Some DIVIDE, rest)
      | '^' -> (Some POW, rest)
      | '%' -> (Some MOD, rest)
      | '=' -> (Some ASSIGNMENT, rest)
      | '>' -> (Some GREATER_THAN, rest)
      | '<' -> (Some LESS_THAN, rest)
      | _ -> (None, rest))
  | c :: rest, Accum (partials, tok_type) -> (
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' -> (
          match tok_type with
          | CHAR -> get_token rest (Accum (c :: partials, tok_type))
          | _ -> (merge_partial_token acc, c :: rest))
      | '0' .. '9' -> (
          match tok_type with
          | DIGIT -> get_token rest (Accum (c :: partials, tok_type))
          | NEGATIVE_DIGIT -> get_token rest (Accum (c :: partials, tok_type))
          | _ -> (merge_partial_token acc, c :: rest))
      | ' ' | '\t' | '\n' | '\r' -> (
          match tok_type with
          | WS -> get_token rest (Accum (c :: partials, tok_type))
          | _ -> (merge_partial_token acc, c :: rest))
      | _ -> (merge_partial_token acc, c :: rest))

let rec tokenize_helper (accum : located_token list) (prog : located_str list) :
    located_token list =
  (* helps tokenizes input list *)
  match prog with
  | [] -> accum
  | (word, pos) :: rest -> (
      match word with
      (* keywords *)
      | "for" -> tokenize_helper ((FOR, pos) :: accum) rest
      | "while" -> tokenize_helper ((WHILE, pos) :: accum) rest
      | "return" -> tokenize_helper ((RETURN, pos) :: accum) rest
      | "mut" -> tokenize_helper ((MUT, pos) :: accum) rest
      | "if" -> tokenize_helper ((IF, pos) :: accum) rest
      | "else" -> tokenize_helper ((ELSE, pos) :: accum) rest
      (* syntax *)
      | "int" -> tokenize_helper ((DATATYPE INT, pos) :: accum) rest
      | "bool" -> tokenize_helper ((DATATYPE BOOL, pos) :: accum) rest
      | "void" -> tokenize_helper ((VOID, pos) :: accum) rest
      | "Option" -> tokenize_helper ((OPTION, pos) :: accum) rest
      | "Some" -> tokenize_helper ((SOME, pos) :: accum) rest
      | "None" -> tokenize_helper ((NONE, pos) :: accum) rest
      (* operations *)
      | "-" -> tokenize_helper ((SUBTRACT, pos) :: accum) rest
      | "==" -> tokenize_helper ((EQUALS, pos) :: accum) rest
      | "!=" -> tokenize_helper ((NOT_EQUALS, pos) :: accum) rest
      | "<=" -> tokenize_helper ((LESS_THAN_EQUALS, pos) :: accum) rest
      | ">=" -> tokenize_helper ((GREATER_THAN_EQUALS, pos) :: accum) rest
      | "and" -> tokenize_helper ((LOGICAL_AND, pos) :: accum) rest
      | "or" -> tokenize_helper ((LOGICAL_OR, pos) :: accum) rest
      | "not" -> tokenize_helper ((NOT, pos) :: accum) rest
      | _ -> (
          (* data *)
          match get_token (string_to_char_list word) Empty with
          | None, [] -> tokenize_helper ((UNKNOWN word, pos) :: accum) rest
          | None, r ->
              tokenize_helper
                ((UNKNOWN word, pos) :: accum)
                ((string_of_char_list r, pos) :: rest)
          | Some t, [] -> tokenize_helper ((t, pos) :: accum) rest
          | Some t, r ->
              tokenize_helper ((t, pos) :: accum)
                ((string_of_char_list r, pos) :: rest)))

(* --PUBLIC INTERFACE-- *)
let tokenize (filename : string) : located_token list * lexer_errors =
  (* read file content & split based on newline  *)
  let lines = String.split_on_char '\n' (Utils.read_file filename) in

  (* process each line *)
  let rec process_line (lines : string list) (line_num : int)
      (accum : located_str list) : located_str list =
    match lines with
    | [] -> accum
    | l :: ls ->
        (* gather all strings in the line, and add positional information *)
        let strs = String.split_on_char ' ' l in
        let located_strs = List.map (fun tok -> (tok, line_num)) strs in
        process_line ls (line_num + 1) (accum @ located_strs)
  in

  let tokens =
    process_line lines 1 []
    |> List.filter (fun (str, pos) -> str <> "") (* remove empty strings *)
    |> tokenize_helper [] |> List.rev (* tokenize *)
  in

  (* accumulate unknown tokens *)
  let errors =
    List.filter
      (fun (x, pos) -> match x with UNKNOWN a -> true | _ -> false)
      tokens
  in
  (tokens, errors)
