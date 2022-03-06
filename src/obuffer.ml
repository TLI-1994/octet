type t = {
  cursor_line : int;
  cursor_pos : int;
  contents : string list;
}

let empty : t = { cursor_line = 0; cursor_pos = 0; contents = [ "" ] }

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

let rec insert_newline_aux_tr
    (line_number : int)
    (pos : int)
    (acc : string list)
    (contents : string list) =
  match contents with
  | [] -> raise (Invalid_argument "contents cannot be empty list")
  | h :: t -> (
      match line_number with
      | 0 -> acc @ break_line h pos @ t
      | lines_left ->
          insert_newline_aux_tr (lines_left - 1) pos (acc @ [ h ]) t)

let insert_newline t =
  {
    cursor_line = t.cursor_line + 1;
    cursor_pos = 0;
    contents =
      insert_newline_aux_tr t.cursor_line t.cursor_pos [] t.contents;
  }
