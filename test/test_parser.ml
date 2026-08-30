(* test/test_parser.ml *)
open OUnit2
open Parser
open Lexer

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

let suite =
  "parser tests"
  >::: [
         "match_expr tokens" >:: test_match_expr;
         "match_expr tokens2 (with parens)" >:: test_match_expr2;
       ]

let () = run_test_tt_main suite
