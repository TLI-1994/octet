open OUnit2
open Octet
(* Testing plan:

   We have tested our backend using OUnit test cases. We developed our
   test cases using black box testing (paths through the spec) and then
   added glass box test cases to increase coverage. Tests are
   functorized, minimizing code duplication and testing multiple
   implementations of the same interface, some of which provide
   amortized or worst-case performance gains over earlier versions.
   Although much of our functionality for editing files is implemented
   in helper functions which are not exposed, we are able to test them
   anyway by creating values for Notty keystrokes and passing them into
   [update_on_key].

   We manually tested the rendering code (creating terminal images and
   syntax highlighting) and the integration of the entire system, but
   these test cases were key to ensuring the correctness and consistency
   of our underlying data structures. *)

module type Tests = sig
  val tests : test list
end

module type MUT_BUFFER_TEST_ENV = sig
  include Obuffer.MUT_BUFFER

  val buffer_string_test : string -> string -> t -> test
  (** [buffer_string_test name expected buf] creates an OUnit test with
      label [name] to assert that the contents of [buf] match [expected] *)

  val insert_buffer_test : string -> char -> string -> t -> test
  (** [insert_buffer_test name c expected buf] creates an OUnit test
      with label [name] which inserts [c] into [buf] and then checks
      that the contents match [expected] *)

  val left_buffer_test : string -> string -> t -> OUnitTest.test
  (** [left_buffer_test name expected buf] creates an OUnit test with
      label [name] which moves the cursor of [buf] to the left and then
      checks that the contents match [expected] *)

  val right_buffer_test : string -> string -> t -> OUnitTest.test
  (** [right_buffer_test name expected buf] creates an OUnit test with
      label [name] which moves the cursor of [buf] to the right and then
      checks that the contents match [expected] *)

  val delete_buffer_test : string -> string -> t -> OUnitTest.test
  (** [delete_buffer_test name expected buf] creates an OUnit test with
      label [name] which deletes the character at the cursor of [buf] to
      and then checks that the contents match [expected] *)

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
      then insert [c] and check that [buffer_contents fb] matches
      [expected]. *)

  val fancy_mv_test :
    string -> Notty.Unescape.key -> char -> string list -> t -> test
  (** [fancy_mv_test name key c expected fb] creates an OUnit test case
      with label [name] that moves the cursor of [fb] according to the
      key [key], inserts the character [c], and checks that
      [buffer_contents fb] matches [expected]. *)

  val mv_search_test :
    string -> string -> char -> string list -> t -> test
  (** [mv_search_test name target c expected fb] creates an OUnit test
      case with label [name] that moves the cursor of [fb] by
      [mv_search fb target], inserts the character [c], and checks that
      [buffer_contents fb] matches [expected]. *)
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

  let fancy_mv_test name key c expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.update_on_key fb key |> ignore;
       Filebuffer.update_on_key fb (`ASCII c, []) |> ignore;
       Filebuffer.buffer_contents fb)
      ~printer:(Util.string_of_list String.escaped)

  let mv_search_test name target c expected fb =
    name >:: fun _ ->
    assert_equal expected
      (Filebuffer.mv_search fb target |> ignore;
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

  let fancy_mv_tests =
    let fb = Filebuffer.empty () in
    let chars_to_insert =
      [
        'c'; 's'; ' '; '3'; '1'; '1'; '0'; ' '; 'o'; 'c'; 't'; 'e'; 't';
      ]
    in
    let rec insert_chars fb lst =
      match lst with
      | [] -> ()
      | h :: t ->
          Filebuffer.update_on_key fb (`ASCII h, []) |> ignore;
          insert_chars fb t
    in
    insert_chars fb chars_to_insert;
    let backward_word_key = (`ASCII 'B', [ `Ctrl ]) in
    let forward_word_key = (`ASCII 'F', [ `Ctrl ]) in
    let backward_kill_key = (`Backspace, [ `Meta ]) in
    let forward_kill_key = (`ASCII 'd', [ `Meta ]) in
    let mv_to_end = (`ASCII 'E', [ `Ctrl ]) in
    let mv_to_begin = (`ASCII 'A', [ `Ctrl ]) in
    Util.pam fb
      [
        contents_test "initial contents is \"cs 3110 octet\""
          [ "cs 3110 octet" ];
        fancy_mv_test
          "backward word when char before cursor is not space"
          backward_word_key ' ' [ "cs 3110  octet" ];
        fancy_mv_test "backward word when char before cursor is space"
          backward_word_key 'c' [ "cs c3110  octet" ];
        fancy_mv_test "forward word when char after cursor is not space"
          forward_word_key 's' [ "cs c3110s  octet" ];
        fancy_mv_test "forward word when char after cursor is space"
          forward_word_key '9' [ "cs c3110s  octet9" ];
        fancy_mv_test
          "forward kill does nothing when cursor is at the end"
          forward_kill_key ' '
          [ "cs c3110s  octet9 " ];
        fancy_mv_test "backward kill when char before cursor is space"
          backward_kill_key 'b' [ "cs c3110s  b" ];
        fancy_mv_test
          "backward kill when char before cursor is not space"
          backward_kill_key 'B' [ "cs c3110s  B" ];
        fancy_mv_test "backward word to set up" backward_word_key 'W'
          [ "cs c3110s  WB" ];
        fancy_mv_test "forward kill when char after cursor is not space"
          forward_kill_key ' ' [ "cs c3110s  W " ];
        fancy_mv_test "backward word to set up" backward_word_key 'V'
          [ "cs c3110s  VW " ];
        fancy_mv_test "forward word to set up" forward_word_key 'U'
          [ "cs c3110s  VWU " ];
        fancy_mv_test "forward kill when char after cursor is space"
          forward_kill_key 'E' [ "cs c3110s  VWUE" ];
        fancy_mv_test "move to beginning" mv_to_begin 'a'
          [ "acs c3110s  VWUE" ];
        fancy_mv_test "move to end" mv_to_end 'e'
          [ "acs c3110s  VWUEe" ];
      ]

  let mv_search_tests =
    Util.pam
      (from_file "test/test_input_2.txt")
      [
        contents_test "read from input file"
          [
            "The fantasy of mutability is that its easy to reason";
            "about: the machine does this, then this, etc.";
            "";
          ];
        mv_search_test "search for string that does not exit" "cs3110"
          '*'
          [
            "*The fantasy of mutability is that its easy to reason";
            "about: the machine does this, then this, etc.";
            "";
          ];
        mv_search_test "search for string that exits" "this" '*'
          [
            "*The fantasy of mutability is that its easy to reason";
            "about: the machine does *this, then this, etc.";
            "";
          ];
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

  let tests =
    read_tests @ fancy_mv_tests @ sequence_tests @ mv_search_tests
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

(** [render_test name input expected] creates an OUnit test case which
    computes the tags of the characters in [char_tags_of_string input]
    and checks that they match [expected]. The format for [expected] is
    the characters of [input], each preceded by the first letter of the
    category they belong to: [Keyword | Symbol | Number | Other]. *)
let render_test name input expected =
  name >:: fun _ ->
  assert_equal expected (Orender.char_tags_of_string_verbose input)
    ~printer:(fun x -> x)

(* We use https://github.com/ocaml/ocaml/blob/trunk/stdlib/list.ml as
   sample code for most of our rendering tests *)
let rendering_tests =
  [
    render_test "OCaml List comment"
      "(* An alias for the type of lists. *)"
      "S(S*O OAOnO OaOlOiOaOsO KfKoKrO OtOhOeO KtKyKpKeO KoKfO \
       OlOiOsOtOsS.O S*S)";
    render_test "OCaml List type"
      "type 'a t = 'a list = [] | (::) of 'a * 'a list"
      "KtKyKpKeO S'OaO OtO S=O S'OaO OlOiOsOtO S=O S[S]O S|O S(S:S:S)O \
       KoKfO S'OaO S*O S'OaO OlOiOsOt";
    render_test "OCaml List length function"
      "let rec length_aux len = function [] -> len | _::l -> \
       length_aux (len + 1) l"
      "KlKeKtO KrKeKcO OlOeOnOgOtOhS_OaOuOxO OlOeOnO S=O \
       KfKuKnKcKtKiKoKnO S[S]O S-S>O OlOeOnO S|O S_S:S:OlO S-S>O \
       OlOeOnOgOtOhS_OaOuOxO S(OlOeOnO S+O N1S)O Ol";
    render_test "OCaml List cons function" "let cons a l = a::l"
      "KlKeKtO OcOoOnOsO OaO OlO S=O OaS:S:Ol";
    render_test "OCaml List nth function line 1" "let nth l n ="
      "KlKeKtO OnOtOhO OlO OnO S=";
    render_test "OCaml List nth function line 2"
      {|  if n < 0 then invalid_arg "List.nth" else|}
      "O O KiKfO OnO S<O N0O KtKhKeKnO OiOnOvOaOlOiOdS_OaOrOgO \
       O\"OLOiOsOtS.OnOtOhO\"O KeKlKsKe";
    render_test "OCaml List nth function line 3"
      {|  let rec nth_aux l n =|}
      "O O KlKeKtO KrKeKcO OnOtOhS_OaOuOxO OlO OnO S=";
    render_test "OCaml List nth function line 4" {|    match l with|}
      "O O O O KmKaKtKcKhO OlO KwKiKtKh";
    render_test "OCaml List nth function line 5"
      {|    | [] -> failwith "nth"|}
      "O O O O S|O S[S]O S-S>O OfOaOiOlOwOiOtOhO O\"OnOtOhO\"";
    render_test "OCaml List nth function line 6"
      {|    | a::l -> if n = 0 then a else nth_aux l (n-1)|}
      "O O O O S|O OaS:S:OlO S-S>O KiKfO OnO S=O N0O KtKhKeKnO OaO \
       KeKlKsKeO OnOtOhS_OaOuOxO OlO S(OnS-N1S)";
    render_test "OCaml List nth function line 7" {|  in nth_aux l n|}
      "O O KiKnO OnOtOhS_OaOuOxO OlO On";
    render_test "OCaml List comparison long comment"
      "(* Note: we are *not* shortcutting the list by using \
       [List.compare_lengths] first; this may be slower on long lists \
       immediately start with distinct elements. It is also incorrect \
       for [compare] below, and it is better (principle of least \
       surprise) to use the same approach for both functions. *)"
      "S(S*O ONOoOtOeS:O OwOeO OaOrOeO S*OnOoOtS*O \
       OsOhOoOrOtOcOuOtOtOiOnOgO OtOhOeO OlOiOsOtO ObOyO OuOsOiOnOgO \
       S[OLOiOsOtS.OcOoOmOpOaOrOeS_OlOeOnOgOtOhOsS]O OfOiOrOsOtS;O \
       OtOhOiOsO OmOaOyO ObOeO OsOlOoOwOeOrO OoOnO OlOoOnOgO \
       OlOiOsOtOsO OiOmOmOeOdOiOaOtOeOlOyO OsOtOaOrOtO KwKiKtKhO \
       OdOiOsOtOiOnOcOtO OeOlOeOmOeOnOtOsS.O OIOtO OiOsO OaOlOsOoO \
       OiOnOcOoOrOrOeOcOtO KfKoKrO S[OcOoOmOpOaOrOeS]O ObOeOlOoOwS,O \
       KaKnKdO OiOtO OiOsO ObOeOtOtOeOrO S(OpOrOiOnOcOiOpOlOeO KoKfO \
       OlOeOaOsOtO OsOuOrOpOrOiOsOeS)O KtKoO OuOsOeO OtOhOeO OsOaOmOeO \
       OaOpOpOrOoOaOcOhO KfKoKrO ObOoOtOhO OfOuOnOcOtOiOoOnOsS.O S*S)";
    render_test "1-10 and random decimal numbers"
      "1 2 3 4 5 6 7 8 9 10 0.2871517527973204 -0.7263030123420913 \
       -0.019244505098450194 0.2501678809785825"
      "N1O N2O N3O N4O N5O N6O N7O N8O N9O N1N0O \
       N0N.N2N8N7N1N5N1N7N5N2N7N9N7N3N2N0N4O \
       N-N0N.N7N2N6N3N0N3N0N1N2N3N4N2N0N9N1N3O \
       N-N0N.N0N1N9N2N4N4N5N0N5N0N9N8N4N5N0N1N9N4O \
       N0N.N2N5N0N1N6N7N8N8N0N9N7N8N5N8N2N5";
    render_test "python float array with 50 elements"
      "array([ 0.38758848,  0.02545175, -0.27866952, -0.15696317, \
       -1.0021448 ,\n\
      \        0.14710676,  0.55861375, -1.27055305, -0.45388927,  \
       0.58829975,\n\
      \       -0.2781081 ,  1.47049902,  0.72041451, -0.35261133, \
       -1.08957482,\n\
      \        0.63800226, -1.89433254, -0.02725236,  0.1197188 , \
       -0.02507692,\n\
      \       -0.36778606, -0.50465987, -1.57201766, -0.44084905,  \
       0.8125005 ,\n\
      \        1.02582686,  1.09046691,  1.13310522, -0.09720492, \
       -1.51662344,\n\
      \        0.92278023,  0.64809181,  0.92832483,  0.90741031,  \
       0.99969728,\n\
      \       -0.17410895,  1.20512911, -0.14561397,  0.30256571, \
       -1.29425083,\n\
      \        0.1684995 , -0.62846886, -0.80046916,  0.22361884, \
       -1.85172568,\n\
      \        0.51293315, -0.77500675,  0.31978637,  1.11771287, \
       -0.30027837])"
      "OaOrOrOaOyS(S[O N0S.N3N8N7N5N8N8N4N8S,O O \
       N0S.N0N2N5N4N5N1N7N5S,O S-N0S.N2N7N8N6N6N9N5N2S,O \
       S-N0S.N1N5N6N9N6N3N1N7S,O N-N1N.N0N0N2N1N4N4N8O S,S\n\
       O O O O O O O O N0S.N1N4N7N1N0N6N7N6S,O O \
       N0S.N5N5N8N6N1N3N7N5S,O S-N1S.N2N7N0N5N5N3N0N5S,O \
       S-N0S.N4N5N3N8N8N9N2N7S,O O N0S.N5N8N8N2N9N9N7N5S,O\n\
       O O O O O O O N-N0N.N2N7N8N1N0N8N1O S,O O \
       N1S.N4N7N0N4N9N9N0N2S,O O N0S.N7N2N0N4N1N4N5N1S,O \
       S-N0S.N3N5N2N6N1N1N3N3S,O S-N1S.N0N8N9N5N7N4N8N2S,O\n\
       O O O O O O O O N0S.N6N3N8N0N0N2N2N6S,O \
       S-N1S.N8N9N4N3N3N2N5N4S,O S-N0S.N0N2N7N2N5N2N3N6S,O O \
       N0N.N1N1N9N7N1N8N8O S,O S-N0S.N0N2N5N0N7N6N9N2S,O\n\
       O O O O O O O S-N0S.N3N6N7N7N8N6N0N6S,O \
       S-N0S.N5N0N4N6N5N9N8N7S,O S-N1S.N5N7N2N0N1N7N6N6S,O \
       S-N0S.N4N4N0N8N4N9N0N5S,O O N0N.N8N1N2N5N0N0N5O S,S\n\
       O O O O O O O O N1S.N0N2N5N8N2N6N8N6S,O O \
       N1S.N0N9N0N4N6N6N9N1S,O O N1S.N1N3N3N1N0N5N2N2S,O \
       S-N0S.N0N9N7N2N0N4N9N2S,O S-N1S.N5N1N6N6N2N3N4N4S,O\n\
       O O O O O O O O N0S.N9N2N2N7N8N0N2N3S,O O \
       N0S.N6N4N8N0N9N1N8N1S,O O N0S.N9N2N8N3N2N4N8N3S,O O \
       N0S.N9N0N7N4N1N0N3N1S,O O N0S.N9N9N9N6N9N7N2N8S,O\n\
       O O O O O O O S-N0S.N1N7N4N1N0N8N9N5S,O O \
       N1S.N2N0N5N1N2N9N1N1S,O S-N0S.N1N4N5N6N1N3N9N7S,O O \
       N0S.N3N0N2N5N6N5N7N1S,O S-N1S.N2N9N4N2N5N0N8N3S,O\n\
       O O O O O O O O N0N.N1N6N8N4N9N9N5O S,O \
       S-N0S.N6N2N8N4N6N8N8N6S,O S-N0S.N8N0N0N4N6N9N1N6S,O O \
       N0S.N2N2N3N6N1N8N8N4S,O S-N1S.N8N5N1N7N2N5N6N8S,O\n\
       O O O O O O O O N0S.N5N1N2N9N3N3N1N5S,O \
       S-N0S.N7N7N5N0N0N6N7N5S,O O N0S.N3N1N9N7N8N6N3N7S,O O \
       N1S.N1N1N7N7N1N2N8N7S,O S-N0S.N3N0N0N2N7N8N3N7S]S)";
  ]

(** [split_test name input_str input_idx expected] creats an OUnit test
    case to check that [split_at_n input_str input_idx] matches
    [expected]. *)
let split_test
    (name : string)
    (input_line : string)
    (i : int)
    (expected_output : string list) : test =
  let open Util in
  name >:: fun _ ->
  assert_equal expected_output
    (split_at_n input_line i)
    ~printer:(string_of_list String.escaped)

let util_tests =
  [
    split_test "split in middle of string" "hello world" 5
      [ "hello"; " world" ];
    split_test "split at beginning of string" "hello world" 0
      [ ""; "hello world" ];
    split_test "split at end of string" "hello world" 11
      [ "hello world"; "" ];
    split_test "split empty string" "" 0 [ ""; "" ];
  ]

let tests =
  "test suite for project"
  >::: List.flatten [ buffer_tests; rendering_tests; util_tests ]

let _ = run_test_tt_main tests
