type t
(** representation type of the buffer manager *)

val init : Obuffer.t -> t
(** initialize a buffermanager with a single buffer *)

val ( <-> ) : t -> t -> t
(** vertically stack buffers *)

val ( <|> ) : t -> t -> t
(*** horizontally stack buffers *)

val to_image : int * int -> t -> Notty.image
(** convert to image *)