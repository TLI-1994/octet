(* module type MUT_FILEBUFFER = sig type t

   val empty : unit -> t val from_file : string -> t val write_to_file :
   t -> unit val to_image : t -> int -> int * int -> bool -> Notty.I.t
   val to_string : t -> string val buffer_contents : t -> string list
   val ocaml_format : t -> t val insert_char : t -> char -> t val
   insert_newline : t -> t val mv_up : t -> t val mv_down : t -> t val
   mv_left : t -> t val mv_right : t -> t

   val update_on_key : t -> Notty.Unescape.key -> t (**
   [handle_keystroke buffer key] is [buffer] updated according to the
   signal sent by [key]. *) end *)

module Make (LineBuffer : Obuffer.MUT_BUFFER) : Obuffer.MUT_FILEBUFFER =
struct
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
      buffer_contents will be written.

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
    {
      ans with
      back =
        read_file file_name
        |> String.split_on_char '\n'
        |> List.map (fun s -> LineBuffer.make s 80);
      desc = file_name;
    }

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

  let mv_cursor (buffer : t) direxn =
    match direxn with
    | `Up -> mv_up buffer
    | `Down -> mv_down buffer
    | `Left -> mv_left buffer
    | `Right -> mv_right buffer

  open Notty

  let update_on_key (buffer : t) (key : Unescape.key) =
    match key with
    | `Enter, _ -> insert_newline buffer
    (* | `ASCII 'F', [ `Ctrl ] -> forward_word buffer | `ASCII 'B', [
       `Ctrl ] -> backward_word buffer *)
    | `ASCII ch, _ -> insert_char buffer ch
    (* | `Backspace, _ | `Delete, _ -> delete buffer *)
    | `Arrow direxn, _ -> mv_cursor buffer direxn
    | _ -> buffer

  let rec list_from_nth lst = function
    | 0 -> lst
    | n -> list_from_nth (List.tl lst) @@ (n - 1)

  open Notty.Infix

  let wrap_to width img =
    let rec go off =
      I.hcrop off 0 img
      :: (if I.width img - off > width then go (off + width) else [])
    in
    go 0 |> I.vcat |> I.hsnap ~align:`Left width

  let cursor_icon = " "

  let cursor_image width =
    I.void width 1 <|> I.string A.(bg lightblack) cursor_icon

  let to_image
      (buffer : t)
      (top_line : int)
      ((width, height) : int * int)
      (show_cursor : bool) =
    let height = height - 1 in
    let remaining = list_from_nth (buffer_contents buffer) top_line in
    let superimposed =
      List.mapi
        (fun i elt ->
          if i = buffer.cursor_line - top_line && show_cursor then
            cursor_image buffer.cursor_pos </> I.string A.empty elt
          else I.string A.empty elt)
        remaining
    in
    let widthcropped = I.vcat (List.map (wrap_to width) superimposed) in
    let heightcropped =
      I.vcrop 0 (I.height widthcropped - height) widthcropped
    in
    heightcropped <-> I.string A.(fg red ++ bg white) buffer.desc
end
