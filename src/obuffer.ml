type t = {
  cursor_line : int;
  cursor_pos : int;
  contents : string list;
  cursor_pos_cache : int;
  desc : string;
}

let empty : t =
  {
    cursor_line = 0;
    cursor_pos = 0;
    contents = [ "" ];
    cursor_pos_cache = 0;
    desc = "Empty Buffer";
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

open Notty

let update_on_key (buffer : t) (key : Unescape.key) =
  match key with
  | `Enter, _ -> insert_newline buffer
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

open Notty.Infix

let cursor_image width =
  I.void width 1 <|> I.string A.(bg lightcyan ++ st blink) cursor_icon

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
  let line_nos =
    Util.from 0 (height - 1)
    |> List.map (fun d ->
           I.string A.(bg black ++ st italic) (Printf.sprintf "% 3d " d))
    |> I.vcat
  in
  let attr =
    if show_cursor then A.(fg lightwhite ++ bg lightblack) else A.empty
  in
  let remaining = list_from_nth buffer_contents top_line in
  let superimposed =
    List.mapi
      (fun i elt ->
        if i = buffer.cursor_line - top_line && show_cursor then
          cursor_image buffer.cursor_pos </> I.string attr elt
        else I.string attr elt)
      (List.map
         (fun s ->
           let l = String.length s in
           if l >= width then s else s ^ String.make (width - l) ' ')
         remaining)
  in
  let widthcropped = I.vcat (List.map (wrap_to width) superimposed) in
  let heightcropped =
    I.vcrop 0 (I.height widthcropped - height) widthcropped
  in
  line_nos <|> heightcropped
  <-> I.string A.(fg red ++ bg white) buffer.desc
