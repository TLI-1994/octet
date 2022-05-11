module type MUT_FILEBUFFER = sig
  type t

  val empty : unit -> t
  val from_file : string -> t
  val write_to_file : t -> string -> unit
  val to_image : t -> int -> int * int -> bool -> Notty.I.t
  val to_string : t -> string
  val contents : t -> string list
  val ocaml_format : t -> unit
  val insert_char : t -> char -> unit
end

module Make (LineBuffer : Obuffer.MUT_BUFFER) : MUT_FILEBUFFER = struct
  type t = {
    front : LineBuffer.t list;
    back : LineBuffer.t list;
    cursor_pos : int;
    cursor_pos_cache : int;
    desc : string;
  }
  (** AF: buffers for the lines in the text file before and including
      the line with the cursor are in [front], in reverse order; buffers
      for lines after the cursor are in [back]; [cursor_pos] and
      [cursor_pos_cache] track the horizontal position of the cursor
      along the current and past lines. [desc] is the path to which file
      contents will be written.

      RI: [cursor_pos_cache >= cursor_pos], no line buffers contain the
      character ['\n']. *)

  let empty () =
    {
      front = [ LineBuffer.make "" 3 ];
      back = [];
      cursor_pos = 0;
      cursor_pos_cache = 0;
      desc = "data/Empty Buffer";
    }

  let insert_char fb c =
    match fb.front with
    | h :: _ -> LineBuffer.insert h c
    | [] -> failwith "no front buffer?"

  let contents fb =
    List.rev_append fb.back fb.front (* line buffers in reverse order *)
    |> List.rev_map LineBuffer.to_string

  let to_string fb = contents fb |> String.concat "\n"

  let read_file (file_name : string) =
    try
      let in_channel = open_in file_name in
      let rec read_all prefix =
        let prefix = if prefix = "" then "" else prefix ^ "\n" in
        match input_line in_channel with
        | (s : string) -> (read_all [@tailcall]) (prefix ^ s)
        | exception End_of_file -> prefix
      in
      read_all ""
    with Sys_error _ -> ""

  let from_file (file_name : string) =
    let ans = empty () in
    {
      ans with
      back =
        read_file file_name
        |> String.split_on_char '\n'
        |> List.map (fun s -> LineBuffer.make s 80);
      desc = file_name;
    }

  let write_to_file _ = failwith "unimplemented"
  let to_image _ = failwith "unimplemented"
  let ocaml_format _ = failwith "unimplemented"
end
