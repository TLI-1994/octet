module type MUT_BUFFER = sig
  type t

  val make : string -> int -> t
  val insert : t -> char -> unit
  val delete : t -> unit
  val to_string : t -> string
  val left : t -> unit
  val right : t -> unit
  val move_to : t -> int -> unit
  val content_size : t -> int
end

module type MUT_FILEBUFFER = sig
  type t

  val empty : unit -> t
  val from_file : string -> t
  val write_to_file : t -> unit
  val to_image : t -> int -> int * int -> bool -> Notty.I.t
  val buffer_contents : t -> string list
  val ocaml_format : t -> t
  val update_on_key : t -> Notty.Unescape.key -> t
  val paste_from_clipboard : t -> t
end
