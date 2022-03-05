open Notty
open Notty_unix

let image_of_buffer (buffer : Obuffer.state) =
  List.map (I.string A.empty) buffer.contents |> I.vcat
