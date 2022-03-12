open Notty

(* type buffer_params = { location : [ `Left | `Right | `Bottom ] } *)
(* type t = { buffers : Obuffer.t list } *)

type t =
  | Hsplit of t * t
  | Vsplit of t * t
  | Leaf of Obuffer.t

let init (b : Obuffer.t) = Leaf b
let ( <-> ) bm1 bm2 = Hsplit (bm1, bm2)
let ( <|> ) bm1 bm2 = Vsplit (bm1, bm2)
let to_image_one (_ : Obuffer.t) (_ : int * int) (_ : bool) = I.empty

let rec to_image (dim : int * int) =
  let open Notty.Infix in
  function
  | Hsplit (t1, t2) ->
      to_image (fst dim, snd dim / 2) t1
      <-> to_image (fst dim, snd dim / 2) t2
  | Vsplit (t1, t2) ->
      to_image (fst dim / 2, snd dim) t1
      <|> to_image (fst dim / 2, snd dim) t2
  | Leaf b -> to_image_one b dim false