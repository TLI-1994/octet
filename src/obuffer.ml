type state = {
  cursor_loc : int * int;
  contents : string list;
  top_loc : int;
}

let empty : state =
  { cursor_loc = (0, 0); contents = [ "" ]; top_loc = 0 }

let insert_into_line line i c =
  String.sub line 0 i ^ Char.escaped c
  ^ String.sub line i (String.length line - i)

let rec insert_aux cursor_loc contents c =
  match cursor_loc with
  | 0, line_loc ->
      insert_into_line (List.hd contents) line_loc c :: List.tl contents
  | lines_left, line_loc ->
      List.hd contents
      :: insert_aux (lines_left - 1, line_loc) (List.tl contents) c

let insert_ascii t c =
  let new_cursor_loc = (fun (a, b) -> (a, b + 1)) t.cursor_loc in
  let new_contents = insert_aux t.cursor_loc t.contents c in
  { t with cursor_loc = new_cursor_loc; contents = new_contents }
