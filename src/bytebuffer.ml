type t = {
  mutable contents_L : Bytes.t;
  mutable contents_R : Bytes.t;
  mutable l_pos : int;
  mutable r_pos : int;
}
(** RI: [contents_L\[0:l_pos - 1\]] and
    [contents_R\[r_pos : length - 1\]] store the string *)

let make str l =
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

let resize_left buf =
  buf.contents_L <-
    Bytes.extend buf.contents_L 0 (Bytes.length buf.contents_L)

let resize_right buf =
  buf.contents_R <-
    Bytes.extend buf.contents_R (Bytes.length buf.contents_R) 0;
  buf.r_pos <- buf.r_pos + (Bytes.length buf.contents_R / 2)

let insert buf c =
  if buf.l_pos = Bytes.length buf.contents_L then resize_left buf
  else ();
  Bytes.set buf.contents_L buf.l_pos c;
  buf.l_pos <- buf.l_pos + 1

let delete buf = buf.l_pos <- buf.l_pos - 1

let left buf =
  if buf.l_pos > 0 then begin
    if buf.r_pos = 0 then resize_right buf else ();
    Bytes.set buf.contents_R (buf.r_pos - 1)
      (Bytes.get buf.contents_L (buf.l_pos - 1));
    buf.l_pos <- buf.l_pos - 1;
    buf.r_pos <- buf.r_pos - 1
  end

let right buf =
  if buf.r_pos < Bytes.length buf.contents_R then begin
    if buf.l_pos + 1 >= Bytes.length buf.contents_L then resize_left buf
    else ();
    Bytes.set buf.contents_L buf.l_pos
      (Bytes.get buf.contents_R (buf.r_pos + 1));
    buf.l_pos <- buf.l_pos + 1;
    buf.r_pos <- buf.r_pos + 1
  end

let to_string buf =
  (* Printf.printf "%d %d\n" (Bytes.length buf.contents_L) (Bytes.length
     buf.contents_R); *)
  Bytes.to_string (Bytes.sub buf.contents_L 0 buf.l_pos)
  ^ Bytes.to_string
      (Bytes.sub buf.contents_R buf.r_pos
         (Bytes.length buf.contents_R - buf.r_pos))
