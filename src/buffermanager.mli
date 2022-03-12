type t
(** representation type of the buffer manager *)

val empty : t

type buffer_params = {
  location : [ `Left | `Right | `Bottom ];
  size : int * int;
}
(** paramters for inserting buffer *)

val add_buffer : t -> Obuffer.t -> t
(** add buffer to this manager *)

val to_image : t -> Notty.image
(** convert to image *)