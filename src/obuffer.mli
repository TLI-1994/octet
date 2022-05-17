(** Buffer types. *)

module type MUT_BUFFER = sig
  type t
  (** [t] is the type of a mutable buffer for the contents of a line. *)

  val make : string -> int -> t
  (** [make str len] creates a buffer that can support strings of size
      up to [len] before any resizing is needed, and is initialized to
      contain [str] with the cursor directly after the string. *)

  val insert : t -> char -> unit
  (** [insert buf c] inserts [c] at the location of the cursor and
      shifts the cursor to be after [c]. *)

  val delete : t -> unit
  (** [delete buf] deletes the character at the location of the cursor. *)

  val to_string : t -> string
  (** [to_string buf] is a string with the contents of [buf]. *)

  val left : t -> unit
  (** [left buf] moves the cursor left one character. *)

  val right : t -> unit
  (** move the cursor right *)

  val move_to : t -> int -> unit
  (** move the cursor to a given position *)

  val content_size : t -> int
  (** query how many characters are there in a buffer *)
end

module type MUT_FILEBUFFER = sig
  type t
  (** [t] is the type of a buffer representing the contents of a file. *)

  val empty : unit -> t
  (** [empty ()] is an empty buffer. *)

  val from_file : string -> t
  (** [from_file s] is a buffer containing the contents of the path [s]. *)

  val write_to_file : t -> unit
  (** [write_to_file buffer] writes the contents of the buffer to the
      file in its name. *)

  val to_image : t -> int ref -> int * int -> bool -> Notty.I.t
  (** [to_image buffer top_line (h, w) show_cursor] is the image of
      [buffer] starting from [top_line], wrapped by width [w], cropped
      to height of [h] with cursor if [show_cursor] and without cursor
      if not [show_cursor]. *)

  (* TODO: simplify this function (?). *)

  (* val to_string : t -> string *)
  val buffer_contents : t -> string list
  (** [buffer_contents buffer] is the contents of [buffer]. *)
  (* TODO: remove this function when the implementation of to_image is
     complete.*)

  val ocaml_format : t -> t
  (** [ocaml_format buffer] is the buffer with the ocaml format applied. *)

  val update_on_key : t -> Notty.Unescape.key -> t
  (** [update_on_key buffer key] is [buffer] updated according to the
      signal sent by [key]. *)

  val paste_from_clipboard : t -> t
end
