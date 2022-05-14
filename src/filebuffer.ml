module type MUT_FILEBUFFER = sig
  type t

  val empty : unit -> t
  val from_file : string -> t
  val write_to_file : t -> string -> unit
  val to_image : t -> int -> int * int -> bool -> Notty.I.t
  val to_string : t -> string
  val contents : t -> string list
  val ocaml_format : t -> t
  val insert_char : t -> char -> t
  val insert_newline : t -> t
  val mv_up : t -> t
  val mv_down : t -> t
  val mv_left : t -> t
  val mv_right : t -> t
end

module Make (LineBuffer : Obuffer.MUT_BUFFER) : MUT_FILEBUFFER = struct
  type t = {
    mutable front : LineBuffer.t list;
    mutable back : LineBuffer.t list;
    mutable cursor_pos : int;
    mutable cursor_pos_cache : int;
    mutable desc : string;
    mutable cursor_line : int;
  }
  (** AF: buffers for the lines in the text file before and including
      the line with the cursor are in [front], in reverse order; buffers
      for lines after the cursor are in [back]; [cursor_pos] and
      [cursor_pos_cache] track the horizontal position of the cursor
      along the current and past lines. [desc] is the path to which file
      contents will be written.

      RI: [cursor_pos_cache >= cursor_pos], no line buffers contain the
      character ['\n']. *)

  let empty () =
    {
      front = [ LineBuffer.make "" 3 ];
      back = [];
      cursor_pos = 0;
      cursor_pos_cache = 0;
      desc = "data/Empty Buffer";
      cursor_line = 0;
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

  let contents fb =
    List.rev_append fb.back fb.front (* line buffers in reverse order *)
    |> List.rev_map LineBuffer.to_string

  let to_string fb = contents fb |> String.concat "\n"

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
    {
      ans with
      back =
        read_file file_name
        |> String.split_on_char '\n'
        |> List.map (fun s -> LineBuffer.make s 80);
      desc = file_name;
    }

  let write_to_file buffer path =
    let out_channel = open_out path in
    Printf.fprintf out_channel "%s\n" (to_string buffer);
    close_out out_channel

  let to_image _ = failwith "unimplemented"

  let ocaml_format (fb : t) =
    let temp_path = "data/_temp.autoformat" in
    let out_channel = open_out temp_path in
    Printf.fprintf out_channel "%s\n" (to_string fb) |> ignore;
    close_out out_channel;
    Sys.command ("ocamlformat --inplace " ^ temp_path) |> ignore;
    let new_buffer = from_file temp_path in
    Sys.command ("rm " ^ temp_path) |> ignore;
    { new_buffer with desc = fb.desc }

  (* open Notty open Notty.Infix

     let rec to_image (buffer : t) (top_line : int) ((width, height) :
     int * int) (show_cursor : bool) = let height = height - 1 in let
     width = width - 5 in let contents = contents buffer in let
     buffer_contents = let l = List.length contents in if l >= height
     then contents else contents @ List.map (fun _ -> "") (Util.from 0
     (height - l)) in let buffer_contents = List.map (fun s -> if
     String.length s < width then s ^ String.make (width - String.length
     s) ' ' else s) buffer_contents in let line_nos = Util.from 0
     (height - 1) |> List.map (fun d -> I.string A.(bg black ++ st
     italic) (Printf.sprintf "% 3d " d)) |> I.vcat in let remaining =
     list_from_nth buffer_contents top_line in let superimposed =
     List.mapi (render_line buffer top_line show_cursor) remaining in
     let widthcropped = I.vcat (List.map (wrap_to width) superimposed)
     in let heightcropped = I.vcrop 0 (I.height widthcropped - height)
     widthcropped in line_nos <|> heightcropped <-> modeline_to_image
     buffer width

     and list_from_nth lst = function | 0 -> lst | n -> list_from_nth
     (List.tl lst) @@ (n - 1)

     and wrap_to width img = let rec go off = I.hcrop off 0 img :: (if
     I.width img - off > width then go (off + width) else []) in go 0 |>
     I.vcat |> I.hsnap ~align:`Left width

     and render_unselected (line : string) (active : bool) : image =
     I.string (if active then A.(fg lightwhite ++ bg lightblack) else
     A.empty) line

     and render_selected (line : string) : image = I.string A.(bg
     lightblue) line

     and render_beginning_selected (line : string) (pos : int) (active :
     bool) : image = let parts = Util.split_at_n line pos in
     render_selected (List.nth parts 0) <|> render_unselected (List.nth
     parts 1) active

     and render_end_selected (line : string) (pos : int) (active : bool)
     : image = let parts = Util.split_at_n line pos in render_unselected
     (List.nth parts 0) active <|> render_selected (List.nth parts 1)

     and render_selected_in_line (line : string) (s : int) (e : int)
     (active : bool) : image = let parts = Util.split_at_n line e in
     render_end_selected (List.nth parts 0) s active <|>
     render_unselected (List.nth parts 1) active

     and render_line_without_cursor (buffer : t) (absolute_line : int)
     (line : string) (active : bool) = if buffer.mark_active then let
     min_select = min buffer.cursor_line buffer.mark_line in let
     max_select = max buffer.cursor_line buffer.mark_line in let
     min_select_pos = min buffer.cursor_pos buffer.mark_pos in let
     max_select_pos = max buffer.cursor_pos buffer.mark_pos in let
     start_pos = if buffer.cursor_line < buffer.mark_line then
     buffer.cursor_pos else buffer.mark_pos in let end_pos = if
     buffer.cursor_line > buffer.mark_line then buffer.cursor_pos else
     buffer.mark_pos in if min_select = max_select && absolute_line =
     min_select then (* one line *) render_selected_in_line line
     min_select_pos max_select_pos active else if absolute_line <
     min_select then render_unselected line active else if absolute_line
     > max_select then render_unselected line active else if
     absolute_line = min_select then render_end_selected line start_pos
     active else if absolute_line = max_select then
     render_beginning_selected line end_pos active else render_selected
     line else render_unselected line active

     and render_line (buffer : t) (top_line : int) (show_cursor : bool)
     (i : int) (elt : string) = let absolute_location = i + top_line in
     if absolute_location = buffer.cursor_line && show_cursor then
     add_cursor (render_line_without_cursor buffer absolute_location elt
     show_cursor) buffer.cursor_pos else render_line_without_cursor
     buffer absolute_location elt show_cursor

     and modeline_to_image (buffer : t) (width : int) = I.string A.(fg
     black ++ bg white) (buffer.desc ^ " Cursor Line: " ^ string_of_int
     buffer.cursor_line ^ " Col: " ^ string_of_int buffer.cursor_pos ^
     "\n Mark Line: " ^ string_of_int buffer.mark_line ^ " Col: " ^
     string_of_int buffer.mark_pos) </> I.char A.(fg black ++ bg white)
     ' ' width 1 *)
end
