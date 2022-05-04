type t

val make : string -> int -> t
(** [make str len] creates a gap buffer that can support strings of size
    up to [len] and which is initialized to contain [str], and which has
    the cursor directly after the string *)

val insert : t -> char -> unit
(** inserts a character at the location of the cursor *)

val delete : t -> unit
(** deletes a character at the location of the cursor *)

val to_string : t -> string
(** convert the contents of the buffer to a string *)

val left : t -> unit
(* val right : t -> unit *)