open OUnit2
open Util

let string_of_string_list (input : string list) : string =
  input |> List.map (fun s -> s ^ ", ") |> List.fold_left ( ^ ) ""

let insert_test
    (name : string)
    (input_line : string)
    (i : int)
    (c : char)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (insert_at_n input_line i c)
    ~printer:String.escaped

let split_test
    (name : string)
    (input_line : string)
    (i : int)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (split_at_n input_line i)
    ~printer:string_of_string_list

let delete_test
    (name : string)
    (input_line : string)
    (i : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (delete_nth input_line i)
    ~printer:String.escaped

let util_tests =
  [
    insert_test "insert at start of string" "hello world" 0 'g'
      "ghello world";
    insert_test "insert into empty string" "" 0 'g' "g";
    insert_test "insert into newline-terminated string" "hello world\n"
      5 't' "hellot world\n";
    insert_test "insert into end of newline-terminated string"
      "hello world\n" 11 '!' "hello world!\n";
    split_test "split in middle of string" "hello world" 5
      [ "hello"; " world" ];
    split_test "split at beginning of string" "hello world" 0
      [ ""; "hello world" ];
    split_test "split at end of string" "hello world" 11
      [ "hello world"; "" ];
    split_test "split empty string" "" 0 [ ""; "" ];
    delete_test "delete single-char string" "0" 0 "";
    delete_test "delete middle of string" "abcd" 1 "acd";
    delete_test "delete end of string" "abcd" 3 "abc";
  ]

let tests = "test suite for project" >::: List.flatten [ util_tests ]
let _ = run_test_tt_main tests
