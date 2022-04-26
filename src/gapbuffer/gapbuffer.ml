module GapBuffer = struct
  type t = {
    main_array : char array;
    mutable gap_len : int;
    mutable gap_left : int;
    mutable gap_right : int;
    gap_incr : int;
    mutable cur : int;
  }

  let init buffer_length gap_length : t =
    let gap_len = gap_length in
    let gap_left = 0 in
    {
      main_array = Array.make buffer_length '_';
      gap_len;
      gap_left;
      gap_right = gap_left + gap_len - 1;
      gap_incr = gap_length;
      cur = 0;
    }

  let make s i = raise (Failure "Unimplemented")

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
    for i = 0 to gb.gap_incr - 1 do
      gb.main_array.(pos + i) <- '_'
    done;
    for i = pos to Array.length temp_main_array - 1 do
      gb.main_array.(gb.gap_incr + i) <- temp_main_array.(i)
    done;
    gb.gap_right <- gb.gap_right + gb.gap_incr;
    gb.gap_len <- gb.gap_len + gb.gap_incr

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

  let insert_string gb instring pos =
    if gb.gap_left <> pos then mv_gap gb pos else ();
    for i = 0 to String.length instring - 1 do
      if gb.gap_left = gb.gap_right then grow gb (pos + i) else ();
      gb.main_array.(pos + i) <- instring.[i];
      gb.gap_left <- gb.gap_left + 1
    done

  let delete_at_pos gb pos =
    if gb.gap_left <> pos then mv_gap gb pos else ();
    gb.gap_right <- gb.gap_right + 1;
    gb.main_array.(gb.gap_right) <- '_'

  let left gb = gb.cur <- max (gb.cur - 1) 0
  let right gb = gb.cur <- min (gb.cur + 1) (gb.gap_len - 1)
  let insert gb c = insert_string gb (Char.escaped c) gb.cur
  let delete gb = delete_at_pos gb gb.cur

  (* let make s ll = *)
end

(* open GapBuffer

   let () = let gb = init 50 10 in print_endline (to_string gb);
   insert_string gb "octet" 0; print_endline (to_string gb);
   delete_at_pos gb 4; print_endline (to_string gb); insert_string gb
   "OCTET" 0; print_endline (to_string gb); insert_string gb "*" 0;
   print_endline (to_string gb); insert gb '*'; print_endline (to_string
   gb); insert gb '&'; print_endline (to_string gb); right gb; right gb;
   right gb; insert gb '#'; print_endline (to_string gb) *)
