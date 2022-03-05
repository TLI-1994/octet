type state = {
  cursor_loc : int * int;
  contents : string list;
  top_loc : int;
}

let empty : state =
  { cursor_loc = (0, 0); contents = [ "" ]; top_loc = 0 }

let insert_into_line line i c =
  String.sub line 0 i ^ Char.escaped c
  ^ String.sub line i (String.length line - i)
