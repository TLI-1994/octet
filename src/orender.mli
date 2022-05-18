(** Helper functions for rendering. *)

val image_of_string :
  (int * int) option -> int option -> string -> Notty.image
(** [image_of_string (Some (hl_start, hl_end)) (Some cursor_loc) s]
    returns a Notty image of [s] with syntax highlighting. If
    [(hl_start, hl_end)] is specified, [s] will be rendered with the
    characters from [hl_start] to [hl_end] being highlighted. If
    [cursor_loc] is specified, a cursor will be rendered at position
    [cursor_loc]. *)

val make_line_numbers : int -> int -> Notty.image
(** [make_line_numbers s h] creates an image of [h] line numbers,
    starting from [s], to be displayed in the gutter of the editor. *)

val crop_to : int * int -> Notty.image list -> Notty.image
(** [crop_to (width, height) img_lst] is an image of size [width] by
    [height] with all the lines of text in [img_lst]. *)

val char_tags_of_string_verbose : string -> string
(** [char_tags_of_string_verbose s] prepends each character in [s] with
    the first character of what tag of
    [Keyword | Symbol | Number | Other] it belongs to *)
