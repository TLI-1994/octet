(** Helper functions. *)

(** [split_at_n s n] is the list of two elements where the first is the
    first [n] characters of [s], the second is the remainder of [s].

    Examples:

    - [split_at_n "hello world" 5] is \["hello"; " world"\].
    - [split_at_n "hello world" 0] is \[""; "hello world"\].
    - [split_at_n "" 0] is \[""; ""\]. *)
let split_at_n s n =
  [ String.sub s 0 n; String.sub s n (String.length s - n) ]

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

(** [pam x \[f1; f2; ...; fn\]] applies functions [f1, f2, ..., fn] [x]
    and builds the list [f1 x; f2 x; ...; fn x].

    named so because it's like [map] but it's backwards. *)
let pam x = List.map (fun f -> f x)

(** [string_of_char c] returns a string consisting of only the character
    [c]*)
let string_of_char = String.make 1

(** [string_list_of_string s] returns a list consisting of strings of
    each character of [s] in order

    Examples:

    - [string_list_of_string ""] is \[\].
    - [string_list_of_string "test" is \["t"; "e"; "s"; "t"\]]*)
let rec string_list_of_string s =
  match s with
  | "" -> []
  | s ->
      String.get s 0 |> string_of_char |> fun x ->
      x
      :: (string_list_of_string @@ String.sub s 1
         @@ (String.length s - 1))

(** [string_of_string_list l] returns a string with all the elements of
    [l] concatenated

    Examples:

    - [string_list_of_string \[\]] is "".
    - [string_list_of_string \["t"; "e"; "s"; "t"\] is "test"]*)
let string_of_string_list = List.fold_left ( ^ ) ""