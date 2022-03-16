(* type buffer_params = { location : [ `Left | `Right | `Bottom ] } *)
(* type t = { buffers : Obuffer.t list } *)

type t =
  | Hsplit of t * t
  | Vsplit of t * t
  | Leaf of {
      buffer : Obuffer.t;
      active : bool;
    }

let init (b : Obuffer.t) = Leaf { buffer = b; active = false }

let rec toggle_focus bm : t =
  match bm with
  | Hsplit (a, b) -> Hsplit (toggle_focus a, toggle_focus b)
  | Vsplit (a, b) -> Vsplit (toggle_focus a, toggle_focus b)
  | Leaf r -> Leaf { r with active = not r.active }

let rec update_all key = function
  | Hsplit (t1, t2) -> Hsplit (update_all key t1, update_all key t2)
  | Vsplit (t1, t2) -> Vsplit (update_all key t1, update_all key t2)
  | Leaf r ->
      if r.active then
        Leaf { r with buffer = Obuffer.update_on_key r.buffer key }
      else Leaf r

let rec write_all = function
  | Hsplit (t1, t2) | Vsplit (t1, t2) ->
      write_all t1;
      write_all t2
  | Leaf r -> Obuffer.write_to_file r.buffer

let ( <-> ) bm1 bm2 = Hsplit (bm1, bm2)
let ( <|> ) bm1 bm2 = Vsplit (bm1, bm2)

let rec to_image (dim : int * int) =
  let open Notty.Infix in
  function
  | Hsplit (t1, t2) ->
      to_image (fst dim, snd dim / 2) t1
      <-> to_image (fst dim, snd dim / 2) t2
  | Vsplit (t1, t2) ->
      to_image (fst dim / 2, snd dim) t1
      <|> to_image (fst dim / 2, snd dim) t2
  | Leaf b -> Obuffer.to_image b.buffer 0 dim b.active