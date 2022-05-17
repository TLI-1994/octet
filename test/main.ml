open OUnit2
open Octet
(* Testing plan:

   We have tested our backend using OUnit test cases. We developed our
   test cases using black box testing (paths through the spec) and then
   added glass box test cases to increase coverage. Tests are
   functorized, minimizing code duplication and testing multiple
   implementations of the same interface, some of which provide
   amortized or worst-case performance gains over earlier versions.
   Although much of our functionality is implemented in helper functions
   which are not exposed, we are able to test them anyway by creating
   Notty keystrokes and passing them into [update_on_key].

   We manually tested the rendering code (creating terminal images and
   syntax highlighting) and the integration of the entire system, but
   these test cases were key to ensuring the correctness and consistency
   of our underlying data structures. *)

module type Tests = sig
  val tests : test list
end

module type String = sig
  val x : string
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
    | Moveto of int
    | Delete

  val make_sequence_test :
    t -> (buffer_op * string * string) list -> test list
  (** [make_sequence_test buf ops] creates a list of OUnit tests that
      executes each of the steps in order. Each test is specified by an
      action and the expected buffer contents to the left and right of
      the cursor after this action. *)
end

module TestEnv_of_Buffer (Buffer : Obuffer.MUT_BUFFER) :
  MUT_BUFFER_TEST_ENV with type t = Buffer.t = struct
  include Buffer

  let contents_size_test
      (name : string)
      (expected : int)
      (buf : Buffer.t) =
    name ^ " (contents size test)" >:: fun _ ->
    assert_equal expected
      (Buffer.content_size buf)
      ~printer:string_of_int

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

  let moveto_buffer_test
      (name : string)
      (index : int)
      (expected : string)
      (buf : Buffer.t) =
    name >:: fun _ ->
    assert_equal expected
      (Buffer.move_to buf index;
       Buffer.to_string buf)
      ~printer:(fun x -> x)

  type buffer_op =
    | Read
    | Left
    | Right
    | Insert of char
    | Moveto of int
    | Delete

  let make_sequence_test buf steps =
    List.mapi
      (fun i (op, e1, e2) ->
        let expected = e1 ^ e2 in
        let name = Printf.sprintf "sequence test: step %i" i in
        (match op with
        | Read -> buffer_string_test name expected buf
        | Left -> left_buffer_test name expected buf
        | Right -> right_buffer_test name expected buf
        | Insert i -> insert_buffer_test name i expected buf
        | Delete -> delete_buffer_test name expected buf
        | Moveto i -> moveto_buffer_test name i expected buf)
        :: [ contents_size_test name (String.length expected) buf ])
      steps
    |> List.flatten
end

module Buffer_Tests (Buffer : Obuffer.MUT_BUFFER) : Tests = struct
  open TestEnv_of_Buffer (Buffer)

  let basic_tests =
    Util.pam (make "ab" 5)
      [
        buffer_string_test "initial contents are \"ab\"" "ab";
        insert_buffer_test "insert c to get \"abc\"" 'c' "abc";
        insert_buffer_test "insert d to get \"abcd\"" 'd' "abcd";
        delete_buffer_test "delete to get \"abc\"" "abc";
        delete_buffer_test "delete to get \"ab\"" "ab";
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
        (Moveto 3, "abc", "dklij");
        (Insert 'x', "abcx", "dklij");
        (Moveto 7, "abcxdkl", "ij");
        (Delete, "abcxdk", "ij");
        (Moveto 0, "", "abcxdkij");
        (Delete, "", "abcxdkij");
        (Moveto 1, "a", "bcxdkij");
        (Delete, "", "bcxdkij");
      ]

  let tests = List.flatten [ basic_tests; sequence_test ]
end

module UtilTests : Tests = struct
  open Util

  let split_test
      (name : string)
      (input_line : string)
      (i : int)
      (expected_output : string list) : test =
    name >:: fun _ ->
    assert_equal expected_output
      (split_at_n input_line i)
      ~printer:(string_of_list String.escaped)

  let tests =
    [
      split_test "split in middle of string" "hello world" 5
        [ "hello"; " world" ];
      split_test "split at beginning of string" "hello world" 0
        [ ""; "hello world" ];
      split_test "split at end of string" "hello world" 11
        [ "hello world"; "" ];
      split_test "split empty string" "" 0 [ ""; "" ];
    ]
end

module type FILE_BUFFER_TEST_ENV = sig
  include Obuffer.MUT_FILEBUFFER

  val contents_test : string -> string list -> t -> test
  (** [contents_test name expected fb] creates an OUnit test with label
      [name] to check that [buffer_contents fb] matches [expected]. *)

  val insert_test : string -> char -> string list -> t -> test
  (** [insert_test name c expected fb] creates an OUnit test with label
      [name] to insert [c] into [fb] and then check that
      [buffer_contents fb] matches [expected]. *)

  val delete_test : string -> string list -> t -> test
  (** [delete_test name expected fb] creates an OUnit test with label
      [name] to delete a character at the current cursor position of
      [fb] and then check that [buffer_contents fb] matches [expected]. *)

  val insert_newline_test : string -> string list -> t -> test
  (** [insert_newline_test name expected fb] creates an OUnit test with
      label [name] to insert ['\n'] into [fb] and then check that
      [buffer_contents fb] matches [expected]. *)

  val mv_insert_test :
    string ->
    [ `Down | `Left | `Right | `Up ] ->
    char ->
    int ->
    string list ->
    t ->
    test
  (** [mv_insert_test name dir c n expected fb] creates an OUnit test
      with label [name] to move in the direction [dir] [n] times, and
      then insert [c] and check that the [buffer_contents fb] matches
      [expected]. *)
end

module TestEnv_of_FileBuffer (Filebuffer : Obuffer.MUT_FILEBUFFER) :
  FILE_BUFFER_TEST_ENV with type t = Filebuffer.t = struct
  include Filebuffer

  let contents_test name expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.buffer_contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let insert_test name c expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.update_on_key fb (`ASCII c, []) |> ignore;
       Filebuffer.buffer_contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let delete_test name expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.update_on_key fb (`Backspace, []) |> ignore;
       Filebuffer.buffer_contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let insert_newline_test name expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.update_on_key fb (`Enter, []) |> ignore;
       Filebuffer.buffer_contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let mv_insert_test name direxn c n expected fb =
    let mv_fun fb = Filebuffer.update_on_key fb (`Arrow direxn, []) in
    let rec mv_n fb = function
      | 0 -> ()
      | n -> mv_n (mv_fun fb) (n - 1)
    in
    name >:: fun _ ->
    assert_equal expected
      (mv_n fb n;
       Filebuffer.update_on_key fb (`ASCII c, []) |> ignore;
       Filebuffer.buffer_contents fb)
      ~printer:(Util.string_of_list String.escaped)
end

module FilebufferTests (Filebuffer : Obuffer.MUT_FILEBUFFER) : Tests =
struct
  open TestEnv_of_FileBuffer (Filebuffer)

  let read_tests =
    Util.pam
      (from_file "test/test_input_1.txt")
      [
        contents_test "read from input file"
          [ "hello world! it is a nice day"; "" ];
        insert_newline_test "insert another new line"
          [ ""; "hello world! it is a nice day"; "" ];
        mv_insert_test
          "move to end of file (with extra down keypresses) and insert \
           'n'"
          `Down 'n' 5
          [ ""; "hello world! it is a nice day"; "n" ];
      ]

  let sequence_tests =
    Util.pam (Filebuffer.empty ())
      [
        contents_test "empty buffer has no contents" [ "" ];
        insert_test "insert first character into buffer" 'a' [ "a" ];
        insert_test "insert second character into buffer" 'b' [ "ab" ];
        delete_test "delete last character" [ "a" ];
        mv_insert_test "move to left of line" `Left 'z' 1 [ "za" ];
        delete_test "delete first character" [ "a" ];
        delete_test "deleting at index 0 has no effect" [ "a" ];
        mv_insert_test "move right and re-insert 'b'" `Right 'b' 1
          [ "ab" ];
        insert_newline_test "insert new line into buffer" [ "ab"; "" ];
        insert_test "insert into newline" 'c' [ "ab"; "c" ];
        insert_newline_test "insert another new line" [ "ab"; "c"; "" ];
        insert_test "insert into another new line" 'd'
          [ "ab"; "c"; "d" ];
        insert_test "insert second character into third line" 'x'
          [ "ab"; "c"; "dx" ];
        insert_test "insert third character into new line" 'e'
          [ "ab"; "c"; "dxe" ];
        mv_insert_test "move left and insert 'y'" `Left 'y' 1
          [ "ab"; "c"; "dxye" ];
        insert_test "insert 'z' in third line" 'z'
          [ "ab"; "c"; "dxyze" ];
        delete_test "delete fourth character of third line"
          [ "ab"; "c"; "dxye" ];
        delete_test "delete third character of third line"
          [ "ab"; "c"; "dxe" ];
        delete_test "delete second character of third line"
          [ "ab"; "c"; "de" ];
        mv_insert_test "move up 1" `Up 'f' 1 [ "ab"; "cf"; "de" ];
        mv_insert_test "move down 1" `Down 'g' 1 [ "ab"; "cf"; "deg" ];
        mv_insert_test "move down 100" `Down 'h' 100
          [ "ab"; "cf"; "degh" ];
        mv_insert_test "move up 100" `Up 'i' 100 [ "abi"; "cf"; "degh" ];
        insert_newline_test "insert new line after move up"
          [ "abi"; ""; "cf"; "degh" ];
        mv_insert_test "move down respects position cache 1" `Down 'j' 1
          [ "abi"; ""; "jcf"; "degh" ];
        mv_insert_test "move down respects position cache 2" `Down 'k' 1
          [ "abi"; ""; "jcf"; "dkegh" ];
        mv_insert_test "move up respects position cache" `Up 'l' 3
          [ "abli"; ""; "jcf"; "dkegh" ];
        mv_insert_test "move down respects position cache 3" `Down 'm' 3
          [ "abli"; ""; "jcf"; "dkemgh" ];
        mv_insert_test "mv left" `Left 'n' 1
          [ "abli"; ""; "jcf"; "dkenmgh" ];
        mv_insert_test "mv left 100" `Left 'o' 100
          [ "abli"; ""; "jcf"; "odkenmgh" ];
        mv_insert_test "mv right" `Right 'p' 1
          [ "abli"; ""; "jcf"; "odpkenmgh" ];
        mv_insert_test "mv right 100" `Right 'q' 100
          [ "abli"; ""; "jcf"; "odpkenmghq" ];
        mv_insert_test "mv left 5" `Left 'r' 5
          [ "abli"; ""; "jcf"; "odpkernmghq" ];
        insert_newline_test "insert newline in the middle of a line"
          [ "abli"; ""; "jcf"; "odpker"; "nmghq" ];
        delete_test "deleting at start of line collapses the two lines"
          [ "abli"; ""; "jcf"; "odpkernmghq" ];
        insert_newline_test "re-insert newline in the middle of a line"
          [ "abli"; ""; "jcf"; "odpker"; "nmghq" ];
        insert_test "insert works well after inserting newline" 's'
          [ "abli"; ""; "jcf"; "odpker"; "snmghq" ];
        mv_insert_test "mv up works well after inserting newline" `Up
          't' 1
          [ "abli"; ""; "jcf"; "otdpker"; "snmghq" ];
      ]

  let tests = read_tests @ sequence_tests
end

let buffer_tests =
  [
    (module Bytebuffer : Obuffer.MUT_BUFFER);
    (module Gapbuffer : Obuffer.MUT_BUFFER);
    (module Stringbuffer : Obuffer.MUT_BUFFER);
  ]
  |> List.map (fun (m : (module Obuffer.MUT_BUFFER)) ->
         let module M = (val m : Obuffer.MUT_BUFFER) in
         let module N = Buffer_Tests (M) in
         let module N' = FilebufferTests (Filebuffer.Make (M)) in
         N.tests @ N'.tests)
  |> List.flatten

let render_test name input expected =
  name >:: fun _ ->
  assert_equal expected (Orender.char_tags_of_string_debug input)
    ~printer:(fun x -> x)

let rendering_tests =
  [
    render_test "test" "let 1 = 3" "KlKeKtO N1O S=O N3";
    render_test "test" "(* An alias for the type of lists. *)"
      "S(S*O OAOnO OaOlOiOaOsO KfKoKrO OtOhOeO KtKyKpKeO KoKfO \
       OlOiOsOtOsS.O S*S)";
  ]

let tests =
  "test suite for project"
  >::: List.flatten [ buffer_tests; rendering_tests ]

let _ = run_test_tt_main tests
