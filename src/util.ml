(** Helper functions. *)

(** [insert_at_n s n c] is the first [n] characters of [s], followed by
    [c], followed by the remainder of [s]. *)
let insert_at_n s n c =
  String.sub s 0 n ^ Char.escaped c
  ^ String.sub s n (String.length s - n)

(** [split_at_n s n] is the list of two elements where the first is the
    first [n] characters of [s], the second is the remainder of [s].

    Examples:

    - [split_at_n "hello world" 5] is \["hello"; " world"\].
    - [split_at_n "hello world" 0] is \[""; "hello world"\].
    - [split_at_n "" 0] is \[""; ""\]. *)
let split_at_n s n =
  [ String.sub s 0 n; String.sub s n (String.length s - n) ]

(** [length_of_nth lst n] is the length of the [n]th element in [lst]. *)
let length_of_nth lst n = List.nth lst n |> String.length

(** [delete_nth s n] is [s] with the [n]th character removed.

    Raises: [Invalid_argument] if [n >= String.length s] *)
let delete_nth s n =
  String.sub s 0 n ^ String.sub s (n + 1) (String.length s - n - 1)

(** [from i j] is a list of integers from [i] to [j], inclusive. *)
let from i j =
  let rec from_aux i j l =
    if i > j then l else from_aux i (j - 1) (j :: l)
  in
  from_aux i j []

(** from A4: [string_of_list printer lst] is a string representing
    [lst], with elements separated by [";"] and enclosed in square
    brackets. *)
let string_of_list
    ?(open_delim = "[")
    ?(close_delim = "]")
    ?(sep = "; ")
    string_of_elt
    lst =
  let len = List.length lst in
  let open Buffer in
  (* As a rough lower bound assume that each element takes a minimum of
     3 characters to represent including a separator, e.g., ["v, "]. The
     buffer will grow as needed, so it's okay if that estimate is
     low. *)
  let buf = create (3 * len) in
  add_string buf open_delim;
  List.iteri
    (fun i v ->
      add_string buf (string_of_elt v);
      if i < len - 1 then add_string buf sep)
    lst;
  add_string buf close_delim;
  contents buf

(** [list_from_nth n lst] is lst but without the first n elements. *)
let rec list_from_nth n lst =
  match (n, lst) with
  | 0, _ -> lst
  | n, _ :: t -> list_from_nth (n - 1) t
  | _, [] -> invalid_arg "too few elements"

(** [read_file file_path] is a string containing the contents of
    [file_path]. Returns [""] if [file_path] is not a path to a valid
    file. *)
let read_file (file_name : string) =
  try
    let in_channel = open_in file_name in
    let rec read_all prefix =
      let prefix = if prefix = "" then "" else prefix ^ "\n" in
      match input_line in_channel with
      | (s : string) -> (read_all [@tailcall]) (prefix ^ s)
      | exception End_of_file -> prefix
    in
    read_all ""
  with Sys_error _ -> ""

(** [pad_to (width, height) contents] pads [contents] to create a
    rectangle of text of width [width] and height [height] *)
let pad_to ((width, height) : int * int) contents =
  let row_padded =
    let l = List.length contents in
    if l >= height then contents
    else contents @ List.map (fun _ -> "") (from 0 (height - l))
  in
  List.map
    (fun s -> s ^ String.make (width - (String.length s mod width)) ' ')
    row_padded