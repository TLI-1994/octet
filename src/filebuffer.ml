module Make (LineBuffer : Obuffer.MUT_BUFFER) : Obuffer.MUT_FILEBUFFER =
struct
  type t = {
    mutable front : LineBuffer.t list;
    mutable back : LineBuffer.t list;
    mutable cursor_pos : int;
    mutable cursor_pos_cache : int;
    mutable desc : string;
    mutable cursor_line : int;
    mark_line : int;
    mark_pos : int;
    mark_active : bool;
  }
  (** AF: buffers for the lines in the text file before and including
      the line with the cursor are in [front], in reverse order; buffers
      for lines after the cursor are in [back]; [cursor_pos] and
      [cursor_pos_cache] track the horizontal position of the cursor
      along the current and past lines. [desc] is the path to which file
      buffer_contents will be written.

      RI: [cursor_pos_cache >= cursor_pos]. [h <> \[\]]. no line buffers
      contain the character ['\n']. *)

  let empty () =
    {
      front = [ LineBuffer.make "" 3 ];
      back = [];
      cursor_pos = 0;
      cursor_pos_cache = 0;
      desc = "data/Empty Buffer";
      cursor_line = 0;
      mark_line = 0;
      mark_pos = 0;
      mark_active = false;
    }

  let insert_char fb c =
    match fb.front with
    | h :: _ ->
        LineBuffer.move_to h fb.cursor_pos_cache;
        LineBuffer.insert h c;
        fb.cursor_pos <- fb.cursor_pos + 1;
        fb.cursor_pos_cache <- fb.cursor_pos;
        fb
    | [] -> failwith "no front buffer?"

  let insert_newline fb =
    match fb.front with
    | [] -> failwith "no front buffer?"
    | h :: t ->
        let s = LineBuffer.to_string h in
        let l =
          List.map
            (fun s -> LineBuffer.make s 3)
            (Util.split_at_n s fb.cursor_pos)
        in
        fb.front <- List.rev_append l t;
        fb.cursor_line <- fb.cursor_line + 1;
        fb.cursor_pos <- 0;
        fb.cursor_pos_cache <- 0;
        fb

  let mv_up fb =
    match fb.front with
    | [] -> failwith "no front buffer?"
    | [ _ ] -> fb
    | h1 :: h2 :: t ->
        LineBuffer.move_to h2 fb.cursor_pos_cache;
        fb.cursor_line <- fb.cursor_line - 1;
        fb.cursor_pos <-
          min fb.cursor_pos_cache (LineBuffer.content_size h2);
        fb.front <- h2 :: t;
        fb.back <- h1 :: fb.back;
        fb

  let mv_down fb =
    match fb.back with
    | [] -> fb
    | h :: t ->
        LineBuffer.move_to h fb.cursor_pos_cache;
        fb.cursor_line <- fb.cursor_line + 1;
        fb.cursor_pos <-
          min fb.cursor_pos_cache (LineBuffer.content_size h);
        fb.front <- h :: fb.front;
        fb.back <- t;
        fb

  let mv_left fb =
    match fb.front with
    | [] -> failwith "no front buffer?"
    | h :: _ ->
        let pos = max 0 (fb.cursor_pos - 1) in
        LineBuffer.move_to h pos;
        fb.cursor_pos <- pos;
        fb.cursor_pos_cache <- pos;
        fb

  let mv_right fb =
    match fb.front with
    | [] -> failwith "no front buffer?"
    | h :: _ ->
        let pos = min (LineBuffer.content_size h) (fb.cursor_pos + 1) in
        LineBuffer.move_to h pos;
        fb.cursor_pos <- pos;
        fb.cursor_pos_cache <- pos;
        fb

  let delete fb =
    begin
      match fb.front with
      | [] -> failwith "no front buffer?"
      | h1 :: h2 :: t when fb.cursor_pos = 0 ->
          let prev_line = LineBuffer.to_string h2 in
          let new_line = prev_line ^ LineBuffer.to_string h1 in
          let new_buf =
            LineBuffer.make new_line (2 * String.length new_line)
          in
          fb.front <- new_buf :: t;
          LineBuffer.move_to new_buf (String.length prev_line);
          fb.cursor_line <- fb.cursor_line - 1;
          fb.cursor_pos <- String.length prev_line;
          fb.cursor_pos_cache <- String.length prev_line
      | h :: _ ->
          LineBuffer.delete h;
          fb.cursor_pos <- max 0 (fb.cursor_pos - 1);
          fb.cursor_pos_cache <- max 0 (fb.cursor_pos_cache - 1)
    end;
    fb

  let buffer_contents fb =
    List.rev_append fb.back fb.front (* line buffers in reverse order *)
    |> List.rev_map LineBuffer.to_string

  let to_string fb = buffer_contents fb |> String.concat "\n"

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

  let from_file (file_name : string) =
    let ans = empty () in
    let line_list =
      read_file file_name
      |> String.split_on_char '\n'
      |> List.map (fun s -> LineBuffer.make s 80)
    in
    match line_list with
    | [] -> ans
    | [ _ ] -> { ans with front = line_list }
    | h :: t -> { ans with front = [ h ]; back = t; desc = file_name }

  let write_to_file buffer =
    let out_channel = open_out buffer.desc in
    Printf.fprintf out_channel "%s\n" (to_string buffer);
    close_out out_channel

  let ocaml_format (fb : t) =
    let temp_path = "data/_temp.autoformat" in
    let out_channel = open_out temp_path in
    Printf.fprintf out_channel "%s\n" (to_string fb) |> ignore;
    close_out out_channel;
    Sys.command ("ocamlformat --inplace " ^ temp_path) |> ignore;
    let new_buffer = from_file temp_path in
    Sys.command ("rm " ^ temp_path) |> ignore;
    { new_buffer with desc = fb.desc }

  let rec paste_from_clipboard (buffer : t) =
    let temp_path = "data/_temp.clipboard" in
    Sys.command ("rm -rf " ^ temp_path) |> ignore;
    Sys.command ("touch " ^ temp_path) |> ignore;
    Sys.command ("pbpaste > " ^ temp_path) |> ignore;
    let s = read_file temp_path in
    let num_of_char = String.length s in
    let clst =
      List.init num_of_char (fun i ->
          if i < num_of_char then String.get s i else '_')
    in
    Sys.command ("rm " ^ temp_path) |> ignore;
    insert_stream buffer clst

  and insert_stream (buffer : t) (lst : char list) =
    match lst with
    | [] -> buffer
    | '\n' :: t -> insert_stream (insert_newline buffer) t
    | h :: t -> insert_stream (insert_char buffer h) t

  let mv_cursor (buffer : t) direxn =
    match direxn with
    | `Up -> mv_up buffer
    | `Down -> mv_down buffer
    | `Left -> mv_left buffer
    | `Right -> mv_right buffer

  open Notty
  open Notty.Infix

  let wrap_to width img =
    let rec go off =
      I.hcrop off 0 img
      :: (if I.width img - off > width then go (off + width) else [])
    in
    go 0 |> I.vcat |> I.hsnap ~align:`Left width

  let cursor_icon = "⚛"

  let cursor_image width =
    I.void width 1 <|> I.string A.(bg red ++ st blink) cursor_icon

  let toggle_mark (buffer : t) =
    {
      buffer with
      mark_active = not buffer.mark_active;
      mark_line = buffer.cursor_line;
      mark_pos = buffer.cursor_pos;
    }

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
        render_selected_in_line line min_select_pos max_select_pos
          active
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
      render_line_without_cursor buffer absolute_location elt
        show_cursor

  let rec to_image
      (buffer : t)
      (top_line : int)
      ((width, height) : int * int)
      (show_cursor : bool) =
    let height = height - 1 in
    let width = width - 5 in
    let padded_contents = pad_to (width, height) buffer in
    let line_nos = line_numbers height in
    let remaining = Util.list_from_nth padded_contents top_line in
    let superimposed =
      List.mapi (render_line buffer top_line show_cursor) remaining
    in
    let widthcropped = I.vcat (List.map (wrap_to width) superimposed) in
    let heightcropped =
      I.vcrop 0 (I.height widthcropped - height) widthcropped
    in
    line_nos <|> heightcropped <-> modeline_to_image buffer width

  and line_numbers height =
    Util.from 0 (height - 1)
    |> List.map (fun d ->
           I.string A.(bg black ++ st italic) (Printf.sprintf "% 3d " d))
    |> I.vcat

  and pad_to ((width, height) : int * int) buffer =
    let row_padded =
      let contents = buffer_contents buffer in
      let l = List.length contents in
      if l >= height then contents
      else contents @ List.map (fun _ -> "") (Util.from 0 (height - l))
    in
    List.map
      (fun s ->
        s ^ String.make (width - (String.length s mod width)) ' ')
      row_padded

  let update_on_key (buffer : t) (key : Unescape.key) =
    match key with
    | `Enter, _ -> insert_newline buffer
    | `ASCII 'P', [ `Ctrl ] -> toggle_mark buffer
    (* | `ASCII 'F', [ `Ctrl ] -> forward_word buffer | `ASCII 'B', [
       `Ctrl ] -> backward_word buffer *)
    | `ASCII ch, _ -> insert_char buffer ch
    | `Backspace, _ | `Delete, _ -> delete buffer
    | `Arrow direxn, _ -> mv_cursor buffer direxn
    | _ -> buffer
end
