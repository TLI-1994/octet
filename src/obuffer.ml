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
    desc = "data/Empty Buffer";
    mark_line = 0;
    mark_pos = 0;
    mark_active = false;
  }

let read_file (file_name : string) =
  try
    let in_channel = open_in file_name in
    let rec read_all init =
      try
        read_all
          ((if init = "" then "" else init ^ "\n")
          ^ input_line in_channel)
      with End_of_file -> init
    in
    read_all ""
  with Sys_error _ -> ""

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

let rec forward_word (buffer : t) =
  let curr_line = List.nth buffer.contents buffer.cursor_line in
  if
    buffer.cursor_pos = String.length curr_line
    && buffer.cursor_line <> List.length buffer.contents - 1
  then
    forward_word
      {
        buffer with
        cursor_line = buffer.cursor_line + 1;
        cursor_pos = 0;
      }
  else
    match
      try String.index_from_opt curr_line buffer.cursor_pos ' '
      with Invalid_argument _ -> failwith "cursor out of bound"
    with
    | None -> { buffer with cursor_pos = String.length curr_line }
    | Some i ->
        if i = buffer.cursor_pos then
          forward_word { buffer with cursor_pos = i + 1 }
        else { buffer with cursor_pos = i }

let rec backward_word (buffer : t) =
  let curr_line = List.nth buffer.contents buffer.cursor_line in
  if buffer.cursor_pos = 0 && buffer.cursor_line <> 0 then
    let cursor_line = buffer.cursor_line - 1 in
    backward_word
      {
        buffer with
        cursor_line;
        cursor_pos =
          List.nth buffer.contents cursor_line |> String.length;
      }
  else
    match
      try String.rindex_from_opt curr_line (buffer.cursor_pos - 1) ' '
      with Invalid_argument _ -> failwith "cursor out of bound"
    with
    | None -> { buffer with cursor_pos = 0 }
    | Some i ->
        if not (i = buffer.cursor_pos - 1) then
          { buffer with cursor_pos = i + 1 }
        else backward_word { buffer with cursor_pos = i }

let rec delete_nth_to (buffer : t) n j =
  match j - buffer.cursor_pos with
  | 0 -> buffer
  | m when m < 0 -> raise (Invalid_argument "j")
  | _ ->
      let contents = delete_aux n j [] buffer.contents in
      delete_nth_to { buffer with contents } n (j - 1)

let rec kill_word (buffer : t) =
  let curr_line = List.nth buffer.contents buffer.cursor_line in
  if
    buffer.cursor_pos = String.length curr_line
    && buffer.cursor_line <> List.length buffer.contents - 1
  then
    let cursor_line, cursor_pos = (buffer.cursor_line + 1, 0) in
    kill_word (delete { buffer with cursor_line; cursor_pos })
  else
    match
      try String.index_from_opt curr_line buffer.cursor_pos ' '
      with Invalid_argument _ -> failwith "cursor out of bound"
    with
    | None ->
        delete_nth_to buffer buffer.cursor_line
        @@ String.length curr_line
    | Some i ->
        if i <> buffer.cursor_pos then
          delete_nth_to buffer buffer.cursor_line i
        else
          delete_nth_to buffer buffer.cursor_line (i + 1) |> kill_word

let rec delete_nth_until (buffer : t) n j =
  match buffer.cursor_pos - j with
  | 0 -> buffer
  | m when m < 0 -> raise (Invalid_argument "j")
  | _ -> delete_nth_until (delete buffer) n j

let rec bkill_word (buffer : t) =
  if buffer.cursor_pos = 0 && buffer.cursor_line <> 0 then
    bkill_word (delete buffer)
  else
    let curr_line = List.nth buffer.contents buffer.cursor_line in
    match
      try String.rindex_from_opt curr_line (buffer.cursor_pos - 1) ' '
      with Invalid_argument _ -> failwith "cursor out of bound"
    with
    | None -> delete_nth_until buffer buffer.cursor_line 0
    | Some i ->
        if not (i = buffer.cursor_pos - 1) then
          delete_nth_until buffer buffer.cursor_line (i + 1)
        else delete_nth_until buffer buffer.cursor_line i |> bkill_word

let to_end_of_line (buffer : t) =
  {
    buffer with
    cursor_pos =
      List.nth buffer.contents buffer.cursor_line |> String.length;
  }

let to_begin_of_line (buffer : t) = { buffer with cursor_pos = 0 }

let update_on_key (buffer : t) (key : Unescape.key) =
  match key with
  | `Enter, _ -> insert_newline buffer
  | `ASCII 'P', [ `Ctrl ] -> toggle_mark buffer
  | `ASCII 'f', [ `Meta ] -> forward_word buffer
  | `ASCII 'b', [ `Meta ] -> backward_word buffer
  | `ASCII 'd', [ `Meta ] -> kill_word buffer
  | `Backspace, [ `Meta ] -> bkill_word buffer
  | `ASCII 'E', [ `Ctrl ] -> to_end_of_line buffer
  | `ASCII 'A', [ `Ctrl ] -> to_begin_of_line buffer
  | `ASCII ch, [] -> insert_ascii buffer ch
  (* | `ASCII c, [ `Meta ] -> insert_ascii (insert_ascii buffer c)
     'X' *)
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
  I.void width 1 <|> I.string A.(bg lightcyan ++ st blink) cursor_icon

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

let render_unselected (line : string) (active : bool) : image =
  I.string
    (if active then A.(fg lightwhite ++ bg lightblack) else A.empty)
    line

let render_selected (line : string) : image =
  I.string A.(bg lightblue) line

let render_beginning_selected
    (line : string)
    (pos : int)
    (active : bool) : image =
  let parts = Util.split_at_n line pos in
  render_selected (List.nth parts 0)
  <|> render_unselected (List.nth parts 1) active

let render_end_selected (line : string) (pos : int) (active : bool) :
    image =
  let parts = Util.split_at_n line pos in
  render_unselected (List.nth parts 0) active
  <|> render_selected (List.nth parts 1)

let render_selected_in_line
    (line : string)
    (s : int)
    (e : int)
    (active : bool) : image =
  let parts = Util.split_at_n line e in
  render_end_selected (List.nth parts 0) s active
  <|> render_unselected (List.nth parts 1) active

let _ = render_beginning_selected
let _ = render_end_selected

let render_line_without_cursor
    (buffer : t)
    (absolute_line : int)
    (line : string)
    (active : bool) =
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
      render_selected_in_line line min_select_pos max_select_pos active
    else if absolute_line < min_select then
      render_unselected line active
    else if absolute_line > max_select then
      render_unselected line active
    else if absolute_line = min_select then
      render_end_selected line start_pos active
    else if absolute_line = max_select then
      render_beginning_selected line end_pos active
    else render_selected line
  else render_unselected line active

let render_line
    (buffer : t)
    (top_line : int)
    (show_cursor : bool)
    (i : int)
    (elt : string) =
  let absolute_location = i + top_line in
  if absolute_location = buffer.cursor_line && show_cursor then
    add_cursor
      (render_line_without_cursor buffer absolute_location elt
         show_cursor)
      buffer.cursor_pos
  else
    render_line_without_cursor buffer absolute_location elt show_cursor

let to_image
    (buffer : t)
    (top_line : int)
    ((width, height) : int * int)
    (show_cursor : bool) =
  let height = height - 1 in
  let width = width - 5 in
  let buffer_contents =
    let l = List.length buffer.contents in
    if l >= height then buffer.contents
    else
      buffer.contents
      @ List.map (fun _ -> "") (Util.from 0 (height - l))
  in
  let buffer_contents =
    List.map
      (fun s ->
        if String.length s < width then
          s ^ String.make (width - String.length s) ' '
        else s)
      buffer_contents
  in
  let line_nos =
    Util.from 0 (height - 1)
    |> List.map (fun d ->
           I.string A.(bg black ++ st italic) (Printf.sprintf "% 3d " d))
    |> I.vcat
  in
  let remaining = list_from_nth buffer_contents top_line in
  let superimposed =
    List.mapi (render_line buffer top_line show_cursor) remaining
  in
  let widthcropped = I.vcat (List.map (wrap_to width) superimposed) in
  let heightcropped =
    I.vcrop 0 (I.height widthcropped - height) widthcropped
  in
  line_nos <|> heightcropped <-> modeline_to_image buffer width

let ocaml_format (buffer : t) =
  let temp_path = "data/_temp.autoformat" in
  let out_channel = open_out temp_path in
  let _ = Printf.fprintf out_channel "%s\n" (to_string buffer) in
  close_out out_channel;
  let _ = Sys.command ("ocamlformat --inplace " ^ temp_path) in
  let new_buffer = from_file temp_path in
  let _ = Sys.command ("rm " ^ temp_path) in
  { new_buffer with desc = buffer.desc }

let rec paste_from_clipboard (buffer : t) =
  let temp_path = "data/_temp.clipboard" in
  let _ = Sys.command ("rm -rf " ^ temp_path) in
  let _ = Sys.command ("touch " ^ temp_path) in
  let _ = Sys.command ("pbpaste > " ^ temp_path) in
  let s = read_file temp_path in
  let num_of_char = String.length s in
  let clst =
    List.init num_of_char (fun i ->
        if i < num_of_char then String.get s i else '_')
  in
  insert_ascii_list buffer clst

and insert_ascii_list (buffer : t) (lst : char list) =
  match lst with
  | [] -> buffer
  | h :: t -> insert_ascii_list (insert_ascii buffer h) t
