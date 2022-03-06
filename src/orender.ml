open Notty

let image_of_buffer (buffer : Obuffer.t) =
  List.map (I.string A.empty) buffer.contents |> I.vcat
