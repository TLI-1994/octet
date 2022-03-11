open OUnit2

let insert_test
    (name : string)
    (input_line : string)
    (i : int)
    (c : char)
    (expected_output : string) : test =
  let open Util in
  name >:: fun _ ->
  assert_equal expected_output
    (insert_at_n input_line i c)
    ~printer:String.escaped

(** TODO: test Obuffer.break_line. *)

(** TODO: test Obuffer.nth_line_len. *)

(** TODO: test Obuffer.delete_from_line. *)

let obuffer_tests =
  [
    insert_test "insert at start of string" "hello world" 0 'g'
      "ghello world";
    insert_test "insert into empty string" "" 0 'g' "g";
    insert_test "insert into newline-terminated string" "hello world\n"
      5 't' "hellot world\n";
    insert_test "insert into end of newline-terminated string"
      "hello world\n" 11 '!' "hello world!\n";
  ]

let tests = "test suite for project" >::: List.flatten [ obuffer_tests ]
let _ = run_test_tt_main tests
