type t

val empty : t
(** The empty buffer. *)

val from_file : string -> t
(** [from_file s] is a buffer containing the contents of the path [s]. *)

val write_to_file : t -> unit
(** write the contents of the buffer to the file in its name *)

val update_on_key : t -> Notty.Unescape.key -> t
(** [handle_keystroke buffer key] is [buffer] updated according to the
    signal sent by [key]. *)

val to_image : t -> int -> int * int -> bool -> Notty.I.t
(** [to_image buffer top_line (h, w) show_cursor] is the image of
    [buffer] starting from [top_line], wrapped by width [w], cropped to
    height of [h] with cursor if [show_cursor] and without cursor if not
    [show_cursor]. *)
(* TODO: simplify this function. *)

val buffer_contents : t -> string list
(** [buffer_contents buffer] is the contents of [buffer]. *)
(*TODO: remove this function when the implementation of to_image is
  complete.*)

val ocaml_format : t -> t
(** [ocaml_format buffer] is the buffer with the ocaml format applied. *)

val paste_from_clipboard : t -> t
