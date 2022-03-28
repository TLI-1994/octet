type t
(** representation type of the buffer manager *)

val init : Obuffer.t -> t
(** initialize a buffermanager with a single buffer *)

val toggle_focus : t -> t
(** changes whether keystrokes are received by this buffer *)

val update_all : Notty.Unescape.key -> t -> t
(** applies a keystroke to all buffers *)

val autoformat : t -> t
(** autoformats the buffer *)

val write_all : t -> unit
(** write all buffer contents to files *)

val ( <-> ) : t -> t -> t
(** vertically stack buffers *)

val ( <|> ) : t -> t -> t
(*** horizontally stack buffers *)

val to_image : int * int -> t -> Notty.image
(** convert to image *)