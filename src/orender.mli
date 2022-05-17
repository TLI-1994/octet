(** Helper functions for rendering. *)

val image_of_string :
  (int * int) option -> int option -> string -> Notty.image

val make_line_numbers : int -> Notty.image
(** [make_line_numbers h] creates an image of line numbers from [0] to
    [h - 1] to be displayed in the gutter of the editor. *)

val crop_to : int * int -> Notty.image list -> Notty.image
(** [crop_to (width, height) img_lst] is an image of size [width] by
    [height] with all the lines of text in [img_lst] *)

val char_tags_of_string_debug : string -> string
