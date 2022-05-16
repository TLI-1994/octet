module type BUFFER_MANAGER = sig
  type t
  (** representation type of the buffer manager *)

  type u
  (** representation type of the underlying buffer *)

  val empty_minibuffer : t

  val init : u -> t
  (** initialize a buffermanager with a single buffer *)

  val toggle_focus : t -> t
  (** changes whether keystrokes are received by this buffer *)

  val minibuffer_on : t -> t
  (** turns on the minibuffer and turns off all other buffers *)

  val minibuffer_off : t -> t
  (** turns off the minibuffer and turns on the top left buffer *)

  val perform_mb_command : t -> t
  (** performs the command in the minibuffer and clears it *)

  val update_all : Notty.Unescape.key -> t -> t
  (** applies a keystroke to all buffers *)

  val autoformat : t -> t
  (** autoformats the buffer *)

  val write_all : t -> unit
  (** write all buffer contents to files *)

  val paste_from_clipboard : t -> t
  (* val paste_from_clipboard : t -> t *)

  val ( <-> ) : t -> t -> t
  (** vertically stack buffers *)

  val ( <|> ) : t -> t -> t
  (*** horizontally stack buffers *)

  val to_image : int * int -> t -> Notty.image
  (** convert to image *)
end

module Make : functor (FileBuffer : Obuffer.MUT_FILEBUFFER) ->
  BUFFER_MANAGER with type u = FileBuffer.t
