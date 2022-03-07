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

let rec mv_cursor_memless t direxn =
  match direxn with
  | `Up ->
      if t.cursor_line = 0 then t
      else
        let new_cursor_line = t.cursor_line - 1 in
        {
          t with
          cursor_line = new_cursor_line;
          cursor_pos =
            nth_line_len t.contents new_cursor_line |> min t.cursor_pos;
        }
  | `Down ->
      if t.cursor_line = List.length t.contents - 1 then t
      else
        let new_cursor_line = t.cursor_line + 1 in
        {
          t with
          cursor_line = new_cursor_line;
          cursor_pos =
            nth_line_len t.contents new_cursor_line |> min t.cursor_pos;
        }
  | `Left ->
      if t.cursor_pos = 0 then
        let t_up = mv_cursor_memless t `Up in
        if t.cursor_line = 0 then t_up
        else
          {
            t_up with
            cursor_pos = nth_line_len t_up.contents t_up.cursor_line;
          }
      else { t with cursor_pos = t.cursor_pos - 1 }
  | `Right ->
      if t.cursor_pos = nth_line_len t.contents t.cursor_line then
        let t_down = mv_cursor_memless t `Down in
        if t.cursor_line = List.length t.contents - 1 then t_down
        else { t_down with cursor_pos = 0 }
      else { t with cursor_pos = t.cursor_pos + 1 }
