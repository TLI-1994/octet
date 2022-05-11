type t = {
  mutable main_array : char array;
  mutable gap_len : int;
  mutable gap_left : int;
  mutable gap_right : int;
  mutable num_of_char : int;
}

let make str len =
  let num_of_char = String.length str in
  let len = max (num_of_char + 1) len in
  let main_array =
    Array.init len (fun i ->
        if i < num_of_char then String.get str i else '_')
  in
  let gap_len = len in
  let gap_left = num_of_char in
  let gap_right = len - 1 in
  { main_array; gap_len; gap_left; gap_right; num_of_char }

let to_string gb =
  let buf =
    Buffer.create (gb.gap_len - (gb.gap_right - gb.gap_left + 1))
  in
  for i = 0 to gb.gap_left - 1 do
    Buffer.add_char buf gb.main_array.(i)
  done;
  for i = gb.gap_right + 1 to gb.gap_len - 1 do
    Buffer.add_char buf gb.main_array.(i)
  done;
  Buffer.contents buf

let grow gb pos =
  let temp_main_array = Array.sub gb.main_array 0 gb.gap_len in
  for i = 0 to pos - 1 do
    gb.main_array.(i) <- temp_main_array.(i)
  done;
  for i = 0 to gb.gap_len - 1 do
    gb.main_array.(pos + i) <- '_'
  done;
  for i = pos to Array.length temp_main_array - 1 do
    gb.main_array.(gb.gap_len + i) <- temp_main_array.(i)
  done;
  gb.gap_right <- gb.gap_right + gb.gap_len;
  gb.gap_len <- 2 * gb.gap_len

let mv_left gb pos =
  while gb.gap_left <> pos do
    gb.gap_left <- gb.gap_left - 1;
    gb.gap_right <- gb.gap_right - 1;
    gb.main_array.(gb.gap_right + 1) <- gb.main_array.(gb.gap_left);
    gb.main_array.(gb.gap_left) <- '_'
  done

let mv_right gb pos =
  while gb.gap_left <> pos do
    gb.gap_left <- gb.gap_left + 1;
    gb.gap_right <- gb.gap_right + 1;
    gb.main_array.(gb.gap_left - 1) <- gb.main_array.(gb.gap_right);
    gb.main_array.(gb.gap_right) <- '_'
  done

let mv_gap gb pos =
  if pos < gb.gap_left then mv_left gb pos else mv_right gb pos

let resize gb =
  let old_length = Array.length gb.main_array in
  let new_main_array =
    Array.init (2 * old_length) (fun i ->
        if i < old_length then gb.main_array.(i) else '_')
  in
  gb.main_array <- new_main_array

let insert_string gb instring pos =
  if
    String.length instring + gb.num_of_char >= gb.gap_len
    && Array.length gb.main_array < 2 * gb.gap_len
    (* Array.length gb.main_array < gb.num_of_char + gb.gap_len +
       String.length instring *)
  then resize gb
  else ();
  if gb.gap_left <> pos then mv_gap gb pos else ();
  for i = 0 to String.length instring - 1 do
    if gb.gap_left = gb.gap_right then grow gb (pos + i) else ();
    gb.main_array.(pos + i) <- instring.[i];
    gb.gap_left <- gb.gap_left + 1;
    gb.num_of_char <- gb.num_of_char + 1
  done

let delete_at_pos gb pos =
  if gb.gap_left <> pos then mv_gap gb pos else ();
  gb.gap_right <- gb.gap_right + 1;
  gb.main_array.(gb.gap_right) <- '_';
  gb.num_of_char <- gb.num_of_char - 1

let left gb =
  if gb.gap_left <> 0 then mv_left gb (gb.gap_left - 1) else ()

let right gb =
  if gb.gap_left <> gb.num_of_char then mv_right gb (gb.gap_left + 1)
  else ()

let insert gb c = insert_string gb (Char.escaped c) gb.gap_left

let delete gb =
  if gb.gap_left <> 0 then delete_at_pos gb (gb.gap_left - 1) else ()
