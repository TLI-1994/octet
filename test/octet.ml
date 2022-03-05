open OUnit2
open Obuffer

let insert_test
    (name : string)
    (input_line : string)
    (i : int)
    (c : char)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (insert_into_line input_line i c)
    ~printer:(fun x -> x)

let obuffer_tests =
  [
    insert_test "insert at start of string" "hello world" 0 'h'
      "hhello world";
  ]

let tests = "test suite for project" >::: List.flatten [ obuffer_tests ]
let _ = run_test_tt_main tests
