(* test/test_parser.ml *)
open OUnit2
open Parser
open Lexer

(* EXPRESSION PARSING *)
let tokens =
  [
    NUMBER 2;
    MULTIPLY;
    NUMBER 3;
    ADD;
    NUMBER 5;
    DIVIDE;
    NUMBER 7;
    ADD;
    NUMBER 10;
    DIVIDE;
    NUMBER 11;
  ]

let expected =
  BINARY
    ( ADD,
      BINARY
        (ADD, BINARY (MULTIPLY, INT 2, INT 3), BINARY (DIVIDE, INT 5, INT 7)),
      BINARY (DIVIDE, INT 10, INT 11) )

let test_match_expr _ =
  match match_expr tokens with
  | [], _ -> assert_failure "not found"
  | e :: _, _rest -> assert_equal ~printer:format_expr expected e

let tokens2 =
  [
    NUMBER 2;
    MULTIPLY;
    NUMBER 3;
    ADD;
    NUMBER 5;
    DIVIDE;
    NUMBER 7;
    ADD;
    NUMBER 10;
    DIVIDE;
    NUMBER 11;
    ADD;
    L_PAREN;
    NUMBER 17;
    ADD;
    NUMBER 110;
    SUBTRACT;
    NUMBER 111;
    R_PAREN;
  ]

let expected2 =
  BINARY
    ( ADD,
      BINARY
        ( ADD,
          BINARY
            (ADD, BINARY (MULTIPLY, INT 2, INT 3), BINARY (DIVIDE, INT 5, INT 7)),
          BINARY (DIVIDE, INT 10, INT 11) ),
      BINARY (SUBTRACT, BINARY (ADD, INT 17, INT 110), INT 111) )

let test_match_expr2 _ =
  match match_expr tokens2 with
  | [], _ -> assert_failure "not found"
  | e :: _, _rest -> assert_equal ~printer:format_expr expected2 e

(* ASSIGNMENT *)
let asst_tokens =
  [ Lexer.IDENTIFIER "x"; Lexer.ASSIGNMENT; Lexer.NUMBER 5; Lexer.SEMICOLON ]

let test_match_asst _ =
  match match_asst asst_tokens with
  | None, _ -> assert_failure "not found"
  | Some a, _rest -> assert_equal "x = INT(5)" (format_asst (Some a))

(* VARDECL *)
let vardecl_tokens1 =
  [
    Lexer.DATATYPE INT;
    Lexer.IDENTIFIER "x";
    Lexer.COMMA;
    Lexer.IDENTIFIER "a";
    Lexer.COMMA;
    Lexer.IDENTIFIER "b";
    Lexer.COMMA;
    Lexer.IDENTIFIER "c";
    Lexer.COMMA;
    Lexer.IDENTIFIER "y";
    Lexer.SEMICOLON;
  ]

let test_match_vardecl_multi _ =
  match match_vardecl vardecl_tokens1 with
  | Some _, _ -> ()
  | None, _ -> assert_failure "not found vardecl (multi)"

let vardecl_tokens2 =
  [ Lexer.DATATYPE INT; Lexer.IDENTIFIER "x"; Lexer.SEMICOLON ]

let test_match_vardecl_single _ =
  match match_vardecl vardecl_tokens2 with
  | Some _, _ -> ()
  | None, _ -> assert_failure "not found vardecl (single)"

(* ARGLIST *)
let arglist_tokens =
  [
    Lexer.BOOLEAN true;
    Lexer.COMMA;
    Lexer.BOOLEAN true;
    Lexer.COMMA;
    Lexer.IDENTIFIER "x";
  ]

let test_match_arglist _ =
  match match_arglist arglist_tokens with
  | Some _, _ -> ()
  | None, _ -> assert_failure "not found arglist"

let empty_arglist_tokens = []

let test_match_arglist_empty _ =
  match match_arglist empty_arglist_tokens with
  | Some _, _ -> ()
  | None, _ -> assert_failure "not found arglist (empty)"

(* FUNC CALL*)
let funcall_tokens_empty =
  [ Lexer.IDENTIFIER "main"; Lexer.L_PAREN; Lexer.R_PAREN; Lexer.SEMICOLON ]

let test_match_funcall_empty _ =
  match match_funcall funcall_tokens_empty with
  | Some _, _ -> ()
  | None, _ -> assert_failure "not found funcall (empty args)"

let funcall_tokens_args =
  [
    Lexer.IDENTIFIER "main";
    Lexer.L_PAREN;
    Lexer.BOOLEAN true;
    Lexer.COMMA;
    Lexer.BOOLEAN true;
    Lexer.COMMA;
    Lexer.IDENTIFIER "p3";
    Lexer.R_PAREN;
    Lexer.SEMICOLON;
  ]

let test_match_funcall_args _ =
  match match_funcall funcall_tokens_args with
  | Some _, _ -> ()
  | None, _ -> assert_failure "not found funcall (with args)"

let suite =
  "parser tests"
  >::: [
         "match_expr tokens" >:: test_match_expr;
         "match_expr tokens2 (with parens)" >:: test_match_expr2;
         "match_asst asst_tokens" >:: test_match_asst;
         "match_vardecl multi" >:: test_match_vardecl_multi;
         "match_vardecl single" >:: test_match_vardecl_single;
         "match_arglist arglist_tokens" >:: test_match_arglist;
         "match_arglist empty" >:: test_match_arglist_empty;
         "match_funcall empty args" >:: test_match_funcall_empty;
         "match_funcall with args" >:: test_match_funcall_args;
       ]

let () = run_test_tt_main suite
