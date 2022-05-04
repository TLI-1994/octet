type gap_char =
  | Gap
  | Char of char

type t = {
  mutable contents : gap_char Array.t;
  mutable gap_start : int;
  mutable gap_end : int;
}

let make str len =
  let arr = Array.make len Gap in
  String.iteri (fun i c -> arr.(i) <- Char c) str;
  { contents = arr; gap_start = String.length str; gap_end = len - 1 }

let insert gb c =
  if gb.gap_start <= gb.gap_end then begin
    gb.contents.(gb.gap_start) <- Char c;
    gb.gap_start <- gb.gap_start + 1
  end
  else
    let length = Array.length gb.contents in
    let arr = Array.make (2 * length) Gap in
    Array.iteri
      (fun i c ->
        if i <= gb.gap_start then arr.(i) <- c
        else arr.(i + length) <- c)
      arr;
    gb.contents <- arr;
    gb.gap_end <- gb.gap_end + length;
    gb.contents.(gb.gap_start) <- Char c;
    gb.gap_start <- gb.gap_start + 1

let delete gb =
  if gb.gap_start > 0 then begin
    gb.contents.(gb.gap_start - 1) <- Gap;
    gb.gap_start <- gb.gap_start - 1
  end

let to_string gb =
  let b = Buffer.create 16 in
  Array.iter
    (function Gap -> () | Char c -> Buffer.add_char b c)
    gb.contents;
  Buffer.contents b

let left gb = gb.gap_start <- gb.gap_start - 1
