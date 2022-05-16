open Notty

(** todo: rethink rep type--maybe lists for each split? *)
module type BUFFER_MANAGER = sig
  type t
  type u

  val empty_minibuffer : t
  val init : u -> t
  val toggle_focus : t -> t
  val minibuffer_on : t -> t
  val minibuffer_off : t -> t
  val perform_mb_command : t -> t
  val update_all : Notty.Unescape.key -> t -> t
  val autoformat : t -> t
  val write_all : t -> unit
  val paste_from_clipboard : t -> t
  val ( <-> ) : t -> t -> t
  val ( <|> ) : t -> t -> t
  val to_image : int * int -> t -> Notty.image
end

module Make (FileBuffer : Obuffer.MUT_FILEBUFFER) = struct
  type t =
    | Hsplit of t * t
    | Vsplit of t * t
    | Leaf of {
        buffer : u;
        active : bool;
      }
    | Minibuffer of {
        buffer : u;
        active : bool;
      }

  and u = FileBuffer.t

  let rec write_all = function
    | Hsplit (t1, t2) | Vsplit (t1, t2) ->
        write_all t1;
        write_all t2
    | Minibuffer _ -> ()
    | Leaf r -> FileBuffer.write_to_file r.buffer

  (** todo: rethink when the minibuffer gets added *)
  let empty_minibuffer =
    Minibuffer { buffer = FileBuffer.empty (); active = false }

  let init (b : FileBuffer.t) = Leaf { buffer = b; active = false }

  let rec minibuffer_on = function
    | Hsplit (a, b) -> Hsplit (minibuffer_on a, minibuffer_on b)
    | Vsplit (a, b) -> Vsplit (minibuffer_on a, minibuffer_on b)
    | Minibuffer r -> Minibuffer { r with active = true }
    | Leaf r -> Leaf { r with active = false }

  let rec get_mb_contents = function
    | Hsplit (a, b) | Vsplit (a, b) -> begin
        match (get_mb_contents a, get_mb_contents b) with
        | Some s, _ -> Some s
        | _, Some s -> Some s
        | None, None -> None
      end
    | Leaf _ -> None
    | Minibuffer r -> begin
        match FileBuffer.buffer_contents r.buffer with
        | h :: _ -> Some h
        | [] -> None
      end

  let rec clear_mb_contents = function
    | Hsplit (a, b) -> Hsplit (clear_mb_contents a, clear_mb_contents b)
    | Vsplit (a, b) -> Vsplit (clear_mb_contents a, clear_mb_contents b)
    | Leaf _ as l -> l
    | Minibuffer _ -> empty_minibuffer

  let remove_mb = function Hsplit (a, Minibuffer _) -> a | mb -> mb

  let perform_mb_command bm =
    let next =
      match get_mb_contents bm with
      | None -> bm
      | Some s -> begin
          match
            s |> String.split_on_char ' ' |> List.filter (( <> ) "")
          with
          | [ "write_all" ] ->
              write_all bm;
              bm
          | [ "open_left"; s ] ->
              Hsplit
                ( Vsplit
                    (s |> FileBuffer.from_file |> init, remove_mb bm),
                  empty_minibuffer )
          | [ "open_right"; s ] ->
              Hsplit
                ( Vsplit
                    (remove_mb bm, s |> FileBuffer.from_file |> init),
                  empty_minibuffer )
          | _ -> bm
        end
    in
    clear_mb_contents next

  let rec turn_off = function
    | Hsplit (a, b) -> Hsplit (turn_off a, turn_off b)
    | Vsplit (a, b) -> Vsplit (turn_off a, turn_off b)
    | Minibuffer r -> Minibuffer { r with active = false }
    | Leaf r -> Leaf { r with active = false }

  let rec minibuffer_off = function
    | Hsplit (a, b) -> Hsplit (minibuffer_off a, turn_off b)
    | Vsplit (a, b) -> Vsplit (minibuffer_off a, turn_off b)
    | Minibuffer r -> Minibuffer { r with active = false }
    | Leaf r -> Leaf { r with active = true }

  let rec toggle_focus bm : t =
    match bm with
    | Hsplit (a, b) -> Hsplit (toggle_focus a, toggle_focus b)
    | Vsplit (a, b) -> Vsplit (toggle_focus a, toggle_focus b)
    | Leaf r -> Leaf { r with active = not r.active }
    | Minibuffer r -> Minibuffer { r with active = false }

  let rec update_all key = function
    | Hsplit (t1, t2) -> Hsplit (update_all key t1, update_all key t2)
    | Vsplit (t1, t2) -> Vsplit (update_all key t1, update_all key t2)
    | Leaf r ->
        if r.active then
          Leaf { r with buffer = FileBuffer.update_on_key r.buffer key }
        else Leaf r
    | Minibuffer r ->
        if r.active then
          Minibuffer
            { r with buffer = FileBuffer.update_on_key r.buffer key }
        else Minibuffer r

  let rec autoformat = function
    | Hsplit (t1, t2) -> Hsplit (autoformat t1, autoformat t2)
    | Vsplit (t1, t2) -> Vsplit (autoformat t1, autoformat t2)
    | Leaf r ->
        if r.active then
          Leaf { r with buffer = FileBuffer.ocaml_format r.buffer }
        else Leaf r
    | Minibuffer _ as m -> m

  let rec write_all = function
    | Hsplit (t1, t2) | Vsplit (t1, t2) ->
        write_all t1;
        write_all t2
    | Leaf r -> FileBuffer.write_to_file r.buffer
    | Minibuffer _ -> ()

  let rec paste_from_clipboard = function
    | Hsplit (t1, t2) ->
        Hsplit (paste_from_clipboard t1, paste_from_clipboard t2)
    | Vsplit (t1, t2) ->
        Vsplit (paste_from_clipboard t1, paste_from_clipboard t2)
    | Leaf r ->
        if r.active then
          Leaf
            { r with buffer = FileBuffer.paste_from_clipboard r.buffer }
        else Leaf r
    | Minibuffer _ as m -> m

  let ( <-> ) bm1 bm2 = Hsplit (bm1, bm2)
  let ( <|> ) bm1 bm2 = Vsplit (bm1, bm2)

  let rec to_image (dim : int * int) =
    let open Notty.Infix in
    function
    | Hsplit (t, (Minibuffer _ as m)) ->
        to_image (fst dim, snd dim - 1) t <-> to_image (fst dim, 2) m
    | Hsplit (t1, t2) ->
        to_image (fst dim, snd dim / 2) t1
        <-> to_image (fst dim, snd dim / 2) t2
    | Vsplit (t1, t2) ->
        to_image ((fst dim - 1) / 2, snd dim) t1
        <|> I.char A.empty '|' 1 (snd dim)
        <|> to_image ((fst dim - 1) / 2, snd dim) t2
    | Leaf { buffer; active } -> FileBuffer.to_image buffer 0 dim active
    | Minibuffer { buffer; active } ->
        FileBuffer.to_image buffer 0 dim active
end
