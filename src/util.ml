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

let from i j =
  let rec from_aux i j l =
    if i > j then l else from_aux i (j - 1) (j :: l)
  in
  from_aux i j []
