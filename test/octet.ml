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
  (** [buffer_string_test name expected buf] creates an OUnit test with
      label [name] to assert that the contents of [buf] match [expected] *)

  val insert_buffer_test : string -> char -> string -> t -> test
  (** [insert_buffer_test name c expected buf] creates an OUnit test
      with label [name] which inserts [c] into [buf] and checks that the
      contents match [expected] *)

  val left_buffer_test : string -> string -> t -> OUnitTest.test
  (** [left_buffer_test name expected buf] creates an OUnit test with
      label [name] which moves the cursor of [buf] to the left and
      checks that the contents match [expected] *)

  val right_buffer_test : string -> string -> t -> OUnitTest.test
  (** [right_buffer_test name expected buf] creates an OUnit test with
      label [name] which moves the cursor of [buf] to the right and
      checks that the contents match [expected] *)

  val delete_buffer_test : string -> string -> t -> OUnitTest.test
  (** [delete_buffer_test name expected buf] creates an OUnit test with
      label [name] which deletes the character at the cursor of [buf] to
      and checks that the contents match [expected] *)

  (** operations that can be done in a series of tests *)
  type buffer_op =
    | Read
    | Left
    | Right
    | Insert of char
    | Delete

  val make_sequence_test :
    t -> (buffer_op * string * string) list -> test list
  (** [make_sequence_test buf ops] creates a list of OUnit tests that
      executes each of the steps in order. Each test is specified by an
      action and the expected buffer contents to the left and right of
      the cursor after this action. *)
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

  let sequence_test =
    make_sequence_test (make "abcd" 3)
      [
        (Read, "abcd", "");
        (Right, "abcd", "");
        (Insert 'i', "abcdi", "");
        (Insert 'j', "abcdij", "");
        (Left, "abcdi", "j");
        (Left, "abcd", "ij");
        (Insert 'k', "abcdk", "ij");
        (Read, "abcdk", "ij");
        (Insert 'm', "abcdkm", "ij");
        (Insert 'l', "abcdkml", "ij");
        (Left, "abcdkm", "lij");
        (Delete, "abcdk", "lij");
        (Left, "abcd", "klij");
        (Left, "abc", "dklij");
        (Left, "ab", "cdklij");
        (Left, "a", "bcdklij");
        (Left, "", "abcdklij");
        (Left, "", "abcdklij");
        (Right, "a", "bcdklij");
        (Right, "ab", "cdklij");
        (Right, "abc", "dklij");
        (Right, "abcd", "klij");
        (Right, "abcdk", "lij");
        (Right, "abcdkl", "ij");
        (Right, "abcdkli", "j");
        (Right, "abcdklij", "");
      ]

  let tests = List.flatten [ basic_tests; sequence_test ]
end

module BytebufferTests = Buffer_Tests (Bytebuffer)
module GapbufferTests = Buffer_Tests (Gapbuffer)

module UtilTests : Tests = struct
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

  let tests =
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
end

module FilebufferTests : Tests = struct
  module Filebuffer = Filebuffer.Make (Gapbuffer)

  let contents_test name expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let insert_test name c expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.insert_char fb c |> ignore;
       Filebuffer.contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let insert_newline_test name expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.insert_newline fb |> ignore;
       Filebuffer.contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let rec mv_up_test name c n expected fb =
    name >:: fun _ ->
    assert_equal expected
      (mv_up_n fb n;
       Filebuffer.insert_char fb c |> ignore;
       Filebuffer.contents fb)
      ~printer:(Util.string_of_list String.escaped)

  and mv_up_n fb = function
    | 0 -> ()
    | n -> mv_up_n (Filebuffer.mv_up fb) (n - 1)

  let rec mv_down_test name c n expected fb =
    name >:: fun _ ->
    assert_equal expected
      (mv_down_n fb n;
       Filebuffer.insert_char fb c |> ignore;
       Filebuffer.contents fb)
      ~printer:(Util.string_of_list String.escaped)

  and mv_down_n fb = function
    | 0 -> ()
    | n -> mv_down_n (Filebuffer.mv_down fb) (n - 1)

  let fb = Filebuffer.empty ()

  let tests =
    [
      contents_test "empty buffer has no contents" [ "" ] fb;
      insert_test "insert first character into buffer" 'a' [ "a" ] fb;
      insert_test "insert second character into buffer" 'b' [ "ab" ] fb;
      insert_newline_test "insert new line into buffer" [ "ab"; "" ] fb;
      insert_test "insert second character into buffer" 'c'
        [ "ab"; "c" ] fb;
      insert_newline_test "insert new line into buffer"
        [ "ab"; "c"; "" ] fb;
      insert_test "insert second character into buffer" 'd'
        [ "ab"; "c"; "d" ] fb;
      insert_test "insert second character into buffer" 'e'
        [ "ab"; "c"; "de" ] fb;
      mv_up_test "insert second character into buffer" 'f' 1
        [ "ab"; "cf"; "de" ] fb;
      mv_down_test "insert second character into buffer" 'g' 1
        [ "ab"; "cf"; "deg" ] fb;
      mv_down_test "insert second character into buffer" 'h' 100
        [ "ab"; "cf"; "degh" ] fb;
      mv_up_test "insert second character into buffer" 'i' 100
        [ "abi"; "cf"; "degh" ] fb;
      insert_newline_test "insert new line into buffer"
        [ "abi"; ""; "cf"; "degh" ]
        fb;
      mv_down_test "insert second character into buffer" 'j' 1
        [ "abi"; ""; "jcf"; "degh" ]
        fb;
      mv_down_test "insert second character into buffer" 'k' 1
        [ "abi"; ""; "jcf"; "dkegh" ]
        fb;
      mv_up_test "insert second character into buffer" 'l' 3
        [ "abli"; ""; "jcf"; "dkegh" ]
        fb;
      mv_down_test "insert second character into buffer" 'm' 3
        [ "abli"; ""; "jcf"; "dkemgh" ]
        fb;
    ]
end

let tests =
  "test suite for project"
  >::: List.flatten
         [
           BytebufferTests.tests;
           GapbufferTests.tests;
           UtilTests.tests;
           FilebufferTests.tests;
         ]

let _ = run_test_tt_main tests
