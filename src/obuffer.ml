type t = {
  cursor_line : int;
  cursor_pos : int;
  contents : string list;
}

let empty : t = { cursor_line = 0; cursor_pos = 0; contents = [ "" ] }

let insert_into_line line i c =
  String.sub line 0 i ^ Char.escaped c
  ^ String.sub line i (String.length line - i)

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
