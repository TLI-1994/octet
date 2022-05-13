module type MUT_FILEBUFFER = sig
  type t

  val empty : unit -> t
  val from_file : string -> t
  val write_to_file : t -> string -> unit
  val to_image : t -> int -> int * int -> bool -> Notty.I.t
  val to_string : t -> string
  val contents : t -> string list
  val ocaml_format : t -> t
  val insert_char : t -> char -> t
  val insert_newline : t -> t
end

module Make (_ : Obuffer.MUT_BUFFER) : MUT_FILEBUFFER
