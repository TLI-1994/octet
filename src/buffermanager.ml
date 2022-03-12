open Notty
(* open Notty_unix *)

type buffer_params = { location : [ `Left | `Right | `Bottom ] }
type t = { buffers : Obuffer.t list }

let empty : t = { buffers = [] }

let to_image bm =
  List.map (fun (b : Obuffer.t) -> b.contents) bm.buffers
  |> List.flatten (* only works for vertically concatenating buffers *)
  |> List.map (I.string A.empty)
  |> I.vcat

let add_buffer bm buf = { buffers = buf :: bm.buffers }