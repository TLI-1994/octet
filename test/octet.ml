open OUnit2
open Util

let string_of_string_list (input : string list) : string =
  input |> List.map (fun s -> s ^ ", ") |> List.fold_left ( ^ ) ""

module type Tests = sig
  val tests : test list
end

module type String = sig
  val x : int
end

module type MUT_BUFFER_TEST_ENV = sig
  include Obuffer.MUT_BUFFER

  val buffer_string_test : string -> string -> t -> test
  val insert_buffer_test : string -> char -> string -> t -> test
  val left_buffer_test : string -> string -> t -> OUnitTest.test
  val right_buffer_test : string -> string -> t -> OUnitTest.test
  val delete_buffer_test : string -> string -> t -> OUnitTest.test

  type buffer_op =
    | Read
    | Left
    | Right
    | Insert of char
    | Delete

  val insert_test : string -> string -> int -> char -> string -> test
  val split_test : string -> string -> int -> string list -> test
  val delete_test : string -> string -> int -> string -> test

  val make_sequence_test :
    t -> (buffer_op * string * string) list -> test list
end

module TestEnv_of_Buffer (Buffer : Obuffer.MUT_BUFFER) :
  MUT_BUFFER_TEST_ENV = struct
  include Buffer

  let buffer_string_test
      (name : string)
      (expected : string)
      (buf : Buffer.t) =
    name >:: fun _ ->
    assert_equal expected (Buffer.to_string buf) ~printer:(fun x -> x)

  let insert_buffer_test
      (name : string)
      (insert : char)
      (expected : string)
      (buf : Buffer.t) =
    name >:: fun _ ->
    assert_equal expected
      (Buffer.insert buf insert;
       Buffer.to_string buf)
      ~printer:(fun x -> x)

  let left_buffer_test
      (name : string)
      (expected : string)
      (buf : Buffer.t) =
    name >:: fun _ ->
    assert_equal expected
      (Buffer.left buf;
       Buffer.to_string buf)
      ~printer:(fun x -> x)

  let right_buffer_test
      (name : string)
      (expected : string)
      (buf : Buffer.t) =
    name >:: fun _ ->
    assert_equal expected
      (Buffer.right buf;
       Buffer.to_string buf)
      ~printer:(fun x -> x)

  let delete_buffer_test
      (name : string)
      (expected : string)
      (buf : Buffer.t) =
    name >:: fun _ ->
    assert_equal expected
      (Buffer.delete buf;
       Buffer.to_string buf)
      ~printer:(fun x -> x)

  type buffer_op =
    | Read
    | Left
    | Right
    | Insert of char
    | Delete

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

  let make_sequence_test buf steps =
    List.mapi
      (fun i (op, e1, e2) ->
        let expected = e1 ^ e2 in
        let name = Printf.sprintf "sequence test: step %i" i in
        match op with
        | Read -> buffer_string_test name expected buf
        | Left -> left_buffer_test name expected buf
        | Right -> right_buffer_test name expected buf
        | Insert i -> insert_buffer_test name i expected buf
        | Delete -> delete_buffer_test name expected buf)
      steps
end

module Buffer_Tests (Buffer : Obuffer.MUT_BUFFER) : Tests = struct
  open TestEnv_of_Buffer (Buffer)

  let basic_tests =
    let buf = make "ab" 5 in
    [
      buffer_string_test "initial contents are \"ab\"" "ab" buf;
      insert_buffer_test "insert c to get \"abc\"" 'c' "abc" buf;
      insert_buffer_test "insert d to get \"abcd\"" 'd' "abcd" buf;
      delete_buffer_test "delete to get \"abc\"" "abc" buf;
      delete_buffer_test "delete to get \"ab\"" "ab" buf;
    ]

  let util_tests =
    [
      insert_test "insert at start of string" "hello world" 0 'g'
        "ghello world";
      insert_test "insert into empty string" "" 0 'g' "g";
      insert_test "insert into newline-terminated string"
        "hello world\n" 5 't' "hellot world\n";
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

  let sequence_test =
    make_sequence_test (make "abcd" 3)
      [
        (Read, "abcd", "");
        (Insert 'i', "abcdi", "");
        (Insert 'j', "abcdij", "");
        (Left, "abcdi", "j");
        (Left, "abcd", "ij");
        (Insert 'k', "abcdk", "ij");
        (Read, "abcdk", "ij");
        (Insert 'l', "abcdkl", "ij");
        (Left, "abcdk", "lij");
        (Left, "abcd", "klij");
      ]

  let tests = List.flatten [ basic_tests; util_tests; sequence_test ]
end

module BytebufferTests = Buffer_Tests (Bytebuffer)
module GapbufferTests = Buffer_Tests (Gapbuffer)

let tests =
  "test suite for project"
  >::: List.flatten [ BytebufferTests.tests; GapbufferTests.tests ]

let _ = run_test_tt_main tests
