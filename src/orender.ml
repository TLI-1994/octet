open Notty

type label =
  | Keyword of string
  | Symbol of string
  | Number of string
  | Other of string

let keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let symbols =
  [
    "!=";
    "#";
    "&";
    "&&";
    "'";
    "(";
    ")";
    "*";
    "+";
    ",";
    "-";
    "-.";
    "->";
    ".";
    "..";
    ".~";
    ":";
    "::";
    ":=";
    ":>";
    ";";
    ";;";
    "<";
    "<-";
    "=";
    ">";
    ">]";
    ">}";
    "?";
    "[";
    "[<";
    "[>";
    "[|";
    "]";
    "_";
    "`";
    "{";
    "{<";
    "|";
    "|]";
    "||";
    "}";
    "~";
  ]

let string_of_char = String.make 1

let rec string_list_of_string s =
  match s with
  | "" -> []
  | s ->
      String.get s 0 |> string_of_char |> fun x ->
      x
      :: (string_list_of_string @@ String.sub s 1
         @@ (String.length s - 1))

let tag_of_word w =
  if List.mem w keywords then Keyword w
  else if List.mem w symbols then Symbol w
  else
    match (int_of_string_opt w, float_of_string_opt w) with
    | None, None -> Other w
    | _ -> Number w

let rec insert_spaces l =
  match l with [] -> [] | h :: t -> h :: " " :: insert_spaces t

let tag_of_string s =
  String.split_on_char ' ' s |> insert_spaces |> List.map tag_of_word

let char_tags_of_other w =
  let split = string_list_of_string w in
  List.map tag_of_word split

let char_tags_of_word w =
  match w with
  | Keyword w ->
      List.map (fun c -> Keyword c) @@ string_list_of_string w
  | Symbol w -> List.map (fun c -> Symbol c) @@ string_list_of_string w
  | Number w -> List.map (fun c -> Number c) @@ string_list_of_string w
  | Other w -> char_tags_of_other w

let char_tags_of_string s =
  tag_of_string s |> List.map char_tags_of_word |> List.flatten

let label_to_image = function
  | Keyword w -> I.string A.(fg yellow ++ bg black) w
  | Symbol w -> I.string A.(fg red ++ bg black) w
  | Number w -> I.string A.(fg green ++ bg black) w
  | Other w -> I.string A.(fg white ++ bg black) w

let label_to_image_hl_cursor hl cursor label =
  if cursor then
    match label with
    | Keyword w -> I.string A.(fg yellow ++ bg white) w
    | Symbol w -> I.string A.(fg red ++ bg white) w
    | Number w -> I.string A.(fg green ++ bg white) w
    | Other w -> I.string A.(fg white ++ bg white) w
  else if hl then
    match label with
    | Keyword w -> I.string A.(fg yellow ++ bg blue) w
    | Symbol w -> I.string A.(fg red ++ bg blue) w
    | Number w -> I.string A.(fg green ++ bg blue) w
    | Other w -> I.string A.(fg white ++ bg blue) w
  else label_to_image label

let image_of_string hl_opt cursor_opt s =
  let tagged = char_tags_of_string s in
  let imlist =
    match (hl_opt, cursor_opt) with
    | None, None ->
        tagged |> List.map (label_to_image_hl_cursor false false)
    | None, Some loc ->
        tagged
        |> List.mapi (fun i ->
               label_to_image_hl_cursor false (i == loc))
    | Some (st, en), None ->
        tagged
        |> List.mapi (fun i ->
               label_to_image_hl_cursor (i >= st && i <= en) false)
    | Some (st, en), Some loc ->
        tagged
        |> List.mapi (fun i ->
               label_to_image_hl_cursor (i >= st && i <= en) (i == loc))
  in
  imlist |> I.hcat
