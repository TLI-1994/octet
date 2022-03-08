type t = {
  cursor_line : int;
  cursor_pos : int;
  contents : string list;
  cursor_pos_cache : int;
}

let empty : t =
  {
    cursor_line = 0;
    cursor_pos = 0;
    contents = [ "" ];
    cursor_pos_cache = 0;
  }

(** [insert_into_line line i c] is the first [i] characters of [line],
    followed by [c], followed by the remainder of [line].

    @raise [Invalid_argument] if [i > String.length line] *)
let insert_into_line line i c =
  try
    String.sub line 0 i ^ Char.escaped c
    ^ String.sub line i (String.length line - i)
  with Invalid_argument _ ->
    raise (Invalid_argument "invalid slice of input string")

let rec insert_aux_tr cursor_line cursor_pos contents_hd contents_tl c =
  match cursor_line with
  | 0 ->
      contents_hd
      @ insert_into_line (List.hd contents_tl) cursor_pos c
        :: List.tl contents_tl
  | lines_left ->
      insert_aux_tr (lines_left - 1) cursor_pos
        (contents_hd @ [ List.hd contents_tl ])
        (List.tl contents_tl) c

let insert_ascii t c =
  {
    t with
    contents = insert_aux_tr t.cursor_line t.cursor_pos [] t.contents c;
    cursor_pos = t.cursor_pos + 1;
    cursor_pos_cache = t.cursor_pos + 1;
  }

let rec insert_aux cursor_line cursor_pos contents c acc =
  match (cursor_line, contents) with
  | 0, h :: t ->
      (* insert c into line h *)
      List.rev_append acc (insert_into_line h cursor_pos c :: t)
  | 0, [] ->
      (* insert c into new line*)
      List.rev_append acc [ Char.escaped c ]
  | lines_left, h :: t ->
      (insert_aux [@tailcall]) (lines_left - 1) cursor_pos t c (h :: acc)
  | _, [] -> raise (Invalid_argument "not enough lines to insert")

let insert_ascii2 (buffer : t) c =
  {
    buffer with
    contents =
      insert_aux buffer.cursor_line buffer.cursor_pos buffer.contents c
        [];
    cursor_pos = buffer.cursor_pos + 1;
    cursor_pos_cache = buffer.cursor_pos + 1;
  }

(** [break_line line pos] is the list containing two strings which are
    [line] divided at [pos] with order preserved. Examples:

    - [break_line "hello world" 5] is \["hello"; " world"\].
    - [break_line "hello world" 0] is \[""; "hello world"\].
    - [break_line "" 0] is \[""; ""\]. *)
let break_line line pos =
  let sub_str1 = String.sub line 0 pos in
  let sub_str2 = String.sub line pos (String.length line - pos) in
  [ sub_str1; sub_str2 ]

let rec insert_newline_aux
    (line_number : int)
    (pos : int)
    (acc : string list)
    (contents : string list) =
  match (line_number, contents) with
  | 0, h :: t ->
      let line_split = break_line h pos in
      List.rev_append (List.rev_append line_split acc) t
  | lines_left, h :: t ->
      (insert_newline_aux [@tailcall]) (lines_left - 1) pos (h :: acc) t
  | _, [] -> raise (Invalid_argument "contents cannot be empty list")

let insert_newline t =
  {
    cursor_line = t.cursor_line + 1;
    cursor_pos = 0;
    contents =
      insert_newline_aux t.cursor_line t.cursor_pos [] t.contents;
    cursor_pos_cache = 0;
  }

let nth_line_len contents line = List.nth contents line |> String.length

<<<<<<< HEAD
(** [move_vertical buffer offset] is buffer with cursor moved vertically
    by [offset]. Requires: [buffer.cursor_line] cannot be either 0 or
    [List.length buffer.contents - 1]. *)
=======
(** [move_vertical buffer offset] is [buffer] with cursor moved
    vertically by [offset].

    Requires:

    - [buffer.cursor_line] is not [0] if [offset] is [~-1]
    - [buffer.cursor_line] is not [List.length buffer.contents - 1] if
      [offset] is [1] . *)
>>>>>>> 00aeccb5e4f70bee17f6e0c4ac8f3054f0b8f5b0
let move_vertical (buffer : t) (offset : int) =
  let new_cursor_line = buffer.cursor_line + offset in
  {
    buffer with
    cursor_line = new_cursor_line;
    cursor_pos =
      min buffer.cursor_pos_cache
        (nth_line_len buffer.contents new_cursor_line);
  }

<<<<<<< HEAD
(** [move_horizontal buffer offset] is buffer with cursor moved
    horizontally by [offset]. Requires: [buffer.cursor_pos] cannot be
    either 0 or the length of [buffer.cursor_line]th element in
    [buffer.contents]. *)
=======
(** [move_horizontal buffer offset] is [buffer] with cursor moved
    horizontally by [offset].

    Requires:

    - [0] < [buffer.cursor_pos] if [offset] is [~-1]
    - [buffer.cursor_pos] < the length of [buffer.cursor_line]th line in
      [buffer.contents] if [offset] is [1]. *)
>>>>>>> 00aeccb5e4f70bee17f6e0c4ac8f3054f0b8f5b0
let move_horizontal (buffer : t) (offset : int) =
  let new_cursor_pos = buffer.cursor_pos + offset in
  {
    buffer with
    cursor_pos = new_cursor_pos;
    cursor_pos_cache = new_cursor_pos;
  }

(** [cursor_jump buffer direxn] is [buffer] with cursor jumpping to

    - the end of previous line if [direxn] is [`Left]
    - the beginning of the next line if [direxn] is [`Right].

    Requires:

    - [0] < [buffer.cursor_line] if [direxn] is [`Left]
    - [buffer.cursor_line] < [List.length buffer.contents - 1] if
      [direxn] is [`Right]. *)
let cursor_jump (buffer : t) direxn =
  let new_cursor_line, new_cursor_pos =
    match direxn with
    | `Left ->
        ( buffer.cursor_line - 1,
          nth_line_len buffer.contents (buffer.cursor_line - 1) )
    | `Right -> (buffer.cursor_line + 1, 0)
  in
  {
    buffer with
    cursor_line = new_cursor_line;
    cursor_pos = new_cursor_pos;
    cursor_pos_cache = new_cursor_pos;
  }

let mv_cursor (buffer : t) direxn =
  match direxn with
  | `Up ->
      if buffer.cursor_line = 0 then buffer
      else move_vertical buffer ~-1
  | `Down ->
      if buffer.cursor_line = List.length buffer.contents - 1 then
        buffer
      else move_vertical buffer 1
  | `Left ->
      if buffer.cursor_pos = 0 then
        if buffer.cursor_line = 0 then buffer
        else cursor_jump buffer `Left
      else move_horizontal buffer ~-1
  | `Right ->
      if
        buffer.cursor_pos
        = nth_line_len buffer.contents buffer.cursor_line
      then
        if buffer.cursor_line = List.length buffer.contents - 1 then
          buffer
        else cursor_jump buffer `Right
      else move_horizontal buffer 1

(** [delete_from_line line i] is [line] with the [i]th character
    removed.

    Raises: [Invalid_argument] if [i >= String.length line] *)
let delete_from_line line i =
  String.sub line 0 i
  ^ String.sub line (i + 1) (String.length line - i - 1)

let rec delete_aux
    (line_number : int)
    (pos : int)
    (acc : string list)
    (contents : string list) =
  match (line_number, contents) with
  | 0, h :: t ->
      if pos = 0 then
        match acc with
        (* cursor is at the beginning of the first line. *)
        | [] -> contents
        (* cursor is at the beginning of some other line. *)
        | ah :: at -> List.rev_append ((ah ^ h) :: at) t
      else List.rev_append (delete_from_line h (pos - 1) :: acc) t
  | lines_left, h :: t ->
      (delete_aux [@tailcall]) (lines_left - 1) pos (h :: acc) t
  | _, [] -> raise (Invalid_argument "contents cannot be empty list")

let delete (buffer : t) =
  let nb = mv_cursor buffer `Left in
  {
    cursor_line = nb.cursor_line;
    cursor_pos = nb.cursor_pos;
    contents =
      delete_aux buffer.cursor_line buffer.cursor_pos [] buffer.contents;
    cursor_pos_cache = nb.cursor_pos_cache;
  }
