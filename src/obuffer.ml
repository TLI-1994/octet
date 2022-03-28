open Notty
open Notty.Infix

type t = {
  cursor_line : int;
  cursor_pos : int;
  contents : string list;
  cursor_pos_cache : int;
  desc : string;
  mark_line : int;
  mark_pos : int;
  mark_active : bool;
}

let empty : t =
  {
    cursor_line = 0;
    cursor_pos = 0;
    contents = [ "" ];
    cursor_pos_cache = 0;
    desc = "Empty Buffer";
    mark_line = 0;
    mark_pos = 0;
    mark_active = false;
  }

let read_file (file_name : string) =
  let in_channel = open_in file_name in
  let rec read_all init =
    try
      read_all
        ((if init = "" then "" else init ^ "\n") ^ input_line in_channel)
    with End_of_file -> init
  in
  read_all ""

let from_string s =
  { empty with contents = String.split_on_char '\n' s }

let from_file s =
  let ans = s |> read_file |> from_string in
  { ans with desc = s }

let to_string buffer = String.concat "\n" buffer.contents

let write_to_file buffer =
  let out_channel = open_out buffer.desc in
  Printf.fprintf out_channel "%s\n" (to_string buffer);
  close_out out_channel

(* TODO: deleted this function when appropriate. *)
let buffer_contents buffer = buffer.contents

let rec insert_aux cursor_line cursor_pos contents c acc =
  match (cursor_line, contents) with
  | 0, h :: t ->
      (* insert c into line h *)
      List.rev_append acc (Util.insert_at_n h cursor_pos c :: t)
  | 0, [] ->
      (* insert c into new line*)
      List.rev_append acc [ Char.escaped c ]
  | lines_left, h :: t ->
      (insert_aux [@tailcall]) (lines_left - 1) cursor_pos t c (h :: acc)
  | _, [] -> raise (Invalid_argument "not enough lines to insert")

let insert_ascii (buffer : t) c =
  {
    buffer with
    contents =
      insert_aux buffer.cursor_line buffer.cursor_pos buffer.contents c
        [];
    cursor_pos = buffer.cursor_pos + 1;
    cursor_pos_cache = buffer.cursor_pos + 1;
  }

let rec insert_newline_aux
    (line_number : int)
    (pos : int)
    (acc : string list)
    (contents : string list) =
  match (line_number, contents) with
  | 0, h :: t ->
      let line_split = Util.split_at_n h pos in
      List.rev_append (List.rev_append line_split acc) t
  | lines_left, h :: t ->
      (insert_newline_aux [@tailcall]) (lines_left - 1) pos (h :: acc) t
  | _, [] -> raise (Invalid_argument "contents cannot be empty list")

let insert_newline t =
  {
    t with
    cursor_line = t.cursor_line + 1;
    cursor_pos = 0;
    contents =
      insert_newline_aux t.cursor_line t.cursor_pos [] t.contents;
    cursor_pos_cache = 0;
  }

(** [move_vertical buffer offset] is [buffer] with cursor moved
    vertically by [offset].

    Requires:

    - [buffer.cursor_line] is not [0] if [offset] is [~-1]
    - [buffer.cursor_line] is not [List.length buffer.contents - 1] if
      [offset] is [1] . *)
let move_vertical (buffer : t) (offset : int) =
  let new_cursor_line = buffer.cursor_line + offset in
  {
    buffer with
    cursor_line = new_cursor_line;
    cursor_pos =
      min buffer.cursor_pos_cache
        (Util.length_of_nth buffer.contents new_cursor_line);
  }

(** [move_horizontal buffer offset] is [buffer] with cursor moved
    horizontally by [offset].

    Requires:

    - [0] < [buffer.cursor_pos] if [offset] is [~-1]
    - [buffer.cursor_pos] < the length of [buffer.cursor_line]th line in
      [buffer.contents] if [offset] is [1]. *)
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
          Util.length_of_nth buffer.contents (buffer.cursor_line - 1) )
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
        = Util.length_of_nth buffer.contents buffer.cursor_line
      then
        if buffer.cursor_line = List.length buffer.contents - 1 then
          buffer
        else cursor_jump buffer `Right
      else move_horizontal buffer 1

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
      else List.rev_append (Util.delete_nth h (pos - 1) :: acc) t
  | lines_left, h :: t ->
      (delete_aux [@tailcall]) (lines_left - 1) pos (h :: acc) t
  | _, [] -> raise (Invalid_argument "contents cannot be empty list")

let delete (buffer : t) =
  let nb = mv_cursor buffer `Left in
  {
    buffer with
    cursor_line = nb.cursor_line;
    cursor_pos = nb.cursor_pos;
    contents =
      delete_aux buffer.cursor_line buffer.cursor_pos [] buffer.contents;
    cursor_pos_cache = nb.cursor_pos_cache;
  }

let toggle_mark (buffer : t) =
  {
    buffer with
    mark_active = not buffer.mark_active;
    mark_line = buffer.cursor_line;
    mark_pos = buffer.cursor_pos;
  }

let update_on_key (buffer : t) (key : Unescape.key) =
  match key with
  | `Enter, _ -> insert_newline buffer
  | `ASCII 'P', [ `Ctrl ] -> toggle_mark buffer
  | `ASCII ch, _ -> insert_ascii buffer ch
  | `Backspace, _ | `Delete, _ -> delete buffer
  | `Arrow direxn, _ -> mv_cursor buffer direxn
  | _ -> buffer

let rec list_from_nth lst = function
  | 0 -> lst
  | n -> list_from_nth (List.tl lst) @@ (n - 1)

let wrap_to width img =
  let rec go off =
    I.hcrop off 0 img
    :: (if I.width img - off > width then go (off + width) else [])
  in
  go 0 |> I.vcat |> I.hsnap ~align:`Left width

let cursor_icon = " "

let cursor_image width =
  I.void width 1 <|> I.string A.(bg lightblack) cursor_icon

let modeline_to_image (buffer : t) (width : int) =
  I.string
    A.(fg black ++ bg white)
    (buffer.desc ^ "  Cursor Line: "
    ^ string_of_int buffer.cursor_line
    ^ "  Col: "
    ^ string_of_int buffer.cursor_pos
    ^ " Mark Line: "
    ^ string_of_int buffer.mark_line
    ^ "  Col: "
    ^ string_of_int buffer.mark_pos)
  </> I.char A.(fg black ++ bg white) ' ' width 1

let add_cursor (line : image) (cursor_pos : int) =
  cursor_image cursor_pos </> line

let render_unselected (line : string) : image = I.string A.empty line

let render_selected (line : string) : image =
  I.string A.(bg lightblack) line

let render_beginning_selected (line : string) (pos : int) : image =
  let parts = Util.split_at_n line pos in
  render_selected (List.nth parts 0)
  <|> render_unselected (List.nth parts 1)

let render_end_selected (line : string) (pos : int) : image =
  let parts = Util.split_at_n line pos in
  render_unselected (List.nth parts 0)
  <|> render_selected (List.nth parts 1)

let render_selected_in_line (line : string) (s : int) (e : int) : image
    =
  let parts = Util.split_at_n line e in
  render_end_selected (List.nth parts 0) s
  <|> render_unselected (List.nth parts 1)

let _ = render_beginning_selected
let _ = render_end_selected

let render_line_without_cursor
    (buffer : t)
    (absolute_line : int)
    (line : string) =
  if buffer.mark_active then
    let min_select = min buffer.cursor_line buffer.mark_line in
    let max_select = max buffer.cursor_line buffer.mark_line in
    let min_select_pos = min buffer.cursor_pos buffer.mark_pos in
    let max_select_pos = max buffer.cursor_pos buffer.mark_pos in
    let start_pos =
      if buffer.cursor_line < buffer.mark_line then buffer.cursor_pos
      else buffer.mark_pos
    in
    let end_pos =
      if buffer.cursor_line > buffer.mark_line then buffer.cursor_pos
      else buffer.mark_pos
    in
    if min_select = max_select && absolute_line = min_select then
      (* one line *)
      render_selected_in_line line min_select_pos max_select_pos
    else if absolute_line < min_select then render_unselected line
    else if absolute_line > max_select then render_unselected line
    else if absolute_line = min_select then
      render_end_selected line start_pos
    else if absolute_line = max_select then
      render_beginning_selected line end_pos
    else render_selected line
  else render_unselected line

let render_line
    (buffer : t)
    (top_line : int)
    (show_cursor : bool)
    (i : int)
    (elt : string) =
  let absolute_location = i + top_line in
  if absolute_location = buffer.cursor_line && show_cursor then
    add_cursor
      (render_line_without_cursor buffer absolute_location elt)
      buffer.cursor_pos
  else render_line_without_cursor buffer absolute_location elt

let to_image
    (buffer : t)
    (top_line : int)
    ((width, height) : int * int)
    (show_cursor : bool) =
  let height = height - 1 in
  let remaining = list_from_nth buffer.contents top_line in
  let superimposed =
    List.mapi (render_line buffer top_line show_cursor) remaining
  in
  let widthcropped = I.vcat (List.map (wrap_to width) superimposed) in
  let heightcropped =
    I.vcrop 0 (I.height widthcropped - height) widthcropped
  in
  heightcropped <-> modeline_to_image buffer width
