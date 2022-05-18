type t = {
  mutable contents_L : Bytes.t;
  mutable contents_R : Bytes.t;
  mutable l_pos : int;
  mutable r_pos : int;
}
(** AF: A string [s1 ^ s2] with the cursor between [s1] and [s2] is
    represented by [bf] if [s1] equals the string formed by the first
    [bf.l_pos] characters of [bf.contents_L], and [s2] equals the string
    formed by the characters after the first [bf.r_pos] characters of
    [bf.contents_R].

    RI: [0 <= l_pos <= Bytes.length contents_L] and
    [0 <= r_pos <= Bytes.length contents_R ]. *)

let debug = true

let check_ri buf =
  0 <= buf.l_pos
  && buf.l_pos <= Bytes.length buf.contents_L
  && 0 <= buf.r_pos
  && buf.r_pos <= Bytes.length buf.contents_R
  [@@coverage off]

let rep_ok buf =
  if (not debug) || check_ri buf then buf else failwith "RI violated!"
  [@@coverage off]

let make (str : string) (l : int) =
  let buffer_L =
    if l - String.length str > 0 then
      Bytes.extend (Bytes.of_string str) 0 (l - String.length str)
    else Bytes.of_string str
  in
  {
    contents_L = buffer_L;
    contents_R = Bytes.make l ' ';
    l_pos = String.length str;
    r_pos = l;
  }

let make str l = rep_ok @@ make str l

let resize_left (buf : t) : unit =
  buf.contents_L <-
    Bytes.extend buf.contents_L 0 (Bytes.length buf.contents_L)

let resize_left buf =
  resize_left (rep_ok buf);
  ignore (rep_ok buf)

let resize_right (buf : t) : unit =
  buf.contents_R <-
    Bytes.extend buf.contents_R (Bytes.length buf.contents_R) 0;
  buf.r_pos <- buf.r_pos + (Bytes.length buf.contents_R / 2)

let resize_right buf =
  resize_right (rep_ok buf);
  ignore (rep_ok buf)

let insert (buf : t) (c : char) : unit =
  if buf.l_pos = Bytes.length buf.contents_L then resize_left buf
  else ();
  Bytes.set buf.contents_L buf.l_pos c;
  buf.l_pos <- buf.l_pos + 1

let insert buf c =
  insert (rep_ok buf) c;
  ignore (rep_ok buf)

let delete (buf : t) : unit = buf.l_pos <- max 0 (buf.l_pos - 1)

let delete buf =
  delete (rep_ok buf);
  ignore (rep_ok buf)

let left (buf : t) : unit =
  if buf.l_pos > 0 then begin
    if buf.r_pos = 0 then resize_right buf else ();
    Bytes.set buf.contents_R (buf.r_pos - 1)
      (Bytes.get buf.contents_L (buf.l_pos - 1));
    buf.l_pos <- buf.l_pos - 1;
    buf.r_pos <- buf.r_pos - 1
  end

let left buf =
  left (rep_ok buf);
  ignore (rep_ok buf)

let right (buf : t) : unit =
  if buf.r_pos < Bytes.length buf.contents_R then begin
    if buf.l_pos + 1 >= Bytes.length buf.contents_L then resize_left buf
    else ();
    Bytes.set buf.contents_L buf.l_pos
      (Bytes.get buf.contents_R buf.r_pos);
    buf.l_pos <- buf.l_pos + 1;
    buf.r_pos <- buf.r_pos + 1
  end

let right buf =
  right (rep_ok buf);
  ignore (rep_ok buf)

let to_string (buf : t) : string =
  Bytes.to_string (Bytes.sub buf.contents_L 0 buf.l_pos)
  ^ Bytes.to_string
      (Bytes.sub buf.contents_R buf.r_pos
         (Bytes.length buf.contents_R - buf.r_pos))

let to_string buf = to_string (rep_ok buf)

let content_size buf =
  buf.l_pos + (Bytes.length buf.contents_R - buf.r_pos)

let content_size buf = content_size (rep_ok buf)

let move_to buf l =
  let l = max 0 l in
  let l = min l (content_size buf) in
  while buf.l_pos > l do
    left buf
  done;
  while buf.l_pos < l do
    right buf
  done

let move_to buf l =
  move_to (rep_ok buf) l;
  ignore (rep_ok buf)
