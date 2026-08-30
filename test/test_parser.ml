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

let suite = "parser tests" >::: [ "match_expr tokens" >:: test_match_expr ]
let () = run_test_tt_main suite
