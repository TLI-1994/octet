(** Helper functions. *)

(** [split_at_n s n] is a list of two elements where the first is the
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

(** [drop n lst] is lst but without the first n elements. *)
let rec drop n lst =
  match (n, lst) with
  | 0, _ -> lst
  | n, _ :: t -> drop (n - 1) t
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

    named so because it's like [List.map] but backwards. *)
let pam x = List.map (fun f -> f x)

(** [string_list_of_string s] returns a list consisting of strings of
    each character of [s] in order

    Examples:

    - [string_list_of_string ""] is \[\].
    - [string_list_of_string "test" is \["t"; "e"; "s"; "t"\]] *)
let string_list_of_string s =
  let l = String.length s in
  List.fold_left
    (fun acc i -> String.sub s (l - i - 1) 1 :: acc)
    []
    (from 0 (String.length s - 1))

(** [string_of_string_list l] returns a string with all the elements of
    [l] concatenated

    Examples:

    - [string_list_of_string \[\]] is "".
    - [string_list_of_string \["t"; "e"; "s"; "t"\]] is "test" *)
let string_of_string_list lst =
  let open Buffer in
  let buf = create (List.length lst) in
  List.iter (fun s -> add_string buf s) lst;
  contents buf

(** [log s] writes the current time and the message [s] to the file
    ["./log"]. Will create a new log file if it does not exist. *)
let log s =
  let out_chan = open_out_gen [ Open_creat; Open_append ] 0o666 "log" in
  Printf.fprintf out_chan "Time: %f\n" (Unix.gettimeofday ());
  Printf.fprintf out_chan "Message: %s\n\n" s;
  close_out out_chan

(** [iter_rev_from f i j] behaves the same as
    [(from i j) |> List.rev |> List.iter f] *)
let rec iter_rev_from f i j =
  if i > j then ()
  else begin
    f j;
    iter_rev_from f i (j - 1)
  end

(** [search range target] is a list of indices in [range] such that the
    next [String.length target] characters in [range] match those of
    [target]. *)
let search range target =
  let n = String.length target in
  let ans = ref [] in
  iter_rev_from
    (fun i -> if String.sub range i n = target then ans := i :: !ans)
    0
    (String.length range - n);
  !ans

(** from 2018 final exam: [interleave s1 s2] interleaves the two
    sequences [s1] and [s2].

    Examples (elements of sequence are in angle brackets):

    - [interleave <1; 2; 3> <4; 5; 6>] is [<1; 2; 3; 4; 5; 6>]
    - [interleave <1; 2> <9; 10; 11; 12>] is [<1; 9; 2; 10; 11; 12>]. *)
let rec interleave (s1 : 'a Seq.t) (s2 : 'a Seq.t) : 'a Seq.t =
 fun () ->
  match (s1 (), s2 ()) with
  | Seq.Cons (s1, t1), Seq.Cons (s2, t2) ->
      Seq.Cons (s1, fun () -> Cons (s2, fun () -> interleave t1 t2 ()))
  | Seq.Nil, s -> s
  | s, Seq.Nil -> s
