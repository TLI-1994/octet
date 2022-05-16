module type MUT_BUFFER = sig
  type t

  val make : string -> int -> t
  val insert : t -> char -> unit
  val delete : t -> unit
  val to_string : t -> string
  val left : t -> unit

  val right : t -> unit
  (** move the cursor right *)

  val move_to : t -> int -> unit
  (** move the cursor to a given position *)

  val content_size : t -> int
  (** query how many characters are there in a buffer *)
end

module type MUT_FILEBUFFER = sig
  type t

  val empty : unit -> t
  val from_file : string -> t
  val write_to_file : t -> unit
  val to_image : t -> int -> int * int -> bool -> Notty.I.t

  (* val to_string : t -> string *)
  val buffer_contents : t -> string list
  val ocaml_format : t -> t

  val update_on_key : t -> Notty.Unescape.key -> t
  (** [handle_keystroke buffer key] is [buffer] updated according to the
      signal sent by [key]. *)

  val paste_from_clipboard : t -> t
end
