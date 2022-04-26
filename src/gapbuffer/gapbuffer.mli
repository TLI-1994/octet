module type GapBuffer = sig
  type t

  val make : string -> int -> t
  val insert : t -> char -> t
  val delete : t -> unit
  val left : t -> unit
  val right : t -> unit
  val to_string : t -> string
end