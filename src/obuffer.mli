type t

val empty : t
(** The empty buffer. *)

val from_string : string -> t
(** [from_string s] is a buffer containing [s]. *)

val update_on_key : t -> Notty.Unescape.key -> t
(** [handle_keystroke buffer key] is [buffer] updated according to the
    signal sent by [key]. *)

val to_image : t -> int -> int * int -> bool -> Notty.I.t
(** [to_image buffer top_line (h, w) show_cursor] is the image of
    [buffer] starting from [top_line], wrapped by width [w], cropped to
    height of [h] with cursor shown if [show_cursor] and with cursor not
    showing if not [show_cursor]. *)
(* TODO: simplify this function. *)

val buffer_contents : t -> string list
(** [buffer_contents buffer] is the contents of [buffer]. *)
(*TODO: remove this function when the implementation of to_image is
  complete.*)

val insert_ascii2 : t -> char -> t
