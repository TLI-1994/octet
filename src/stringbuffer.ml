type t = {
  mutable contents : string;
  mutable index : int;
}
(** AF: A string [s1 ^ s2] with the cursor between [s1] and [s2] is
    represented if [s1] equals the string formed by the first [l_pos]
    characters of [contents_L], and [s2] equals the string formed by the
    characters after the first [r_pos] characters of [contents_R]. *)

let make (str : string) (_ : int) =
  { contents = str; index = String.length str }

let insert (buf : t) (c : char) : unit =
  buf.contents <-
    String.sub buf.contents 0 buf.index
    ^ Char.escaped c
    ^ String.sub buf.contents buf.index
        (String.length buf.contents - buf.index);
  buf.index <- buf.index + 1

let delete (buf : t) : unit =
  if buf.index <> 0 then begin
    buf.index <- max 0 (buf.index - 1);
    buf.contents <-
      String.sub buf.contents 0 buf.index
      ^ String.sub buf.contents (buf.index + 1)
          (String.length buf.contents - 1 - buf.index)
  end

let left (buf : t) : unit = buf.index <- max 0 (buf.index - 1)

let right (buf : t) : unit =
  buf.index <- min (String.length buf.contents) (buf.index + 1)

let to_string (buf : t) : string = buf.contents
let content_size buf = String.length buf.contents

let move_to buf l =
  let l = max 0 l in
  let l = min l (content_size buf) in
  while buf.index > l do
    left buf
  done;
  while buf.index < l do
    right buf
  done
