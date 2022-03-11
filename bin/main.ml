(*open Notty*)
open Notty_unix

let init = Obuffer.empty
let img = Orender.image_of_buffer

let main =
  let rec update t state =
    Term.image t (img state);
    loop t state
  and loop t state =
    match Term.event t with
    | `Key (`ASCII 'X', [ `Ctrl ]) -> (
        match Term.event t with
        | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
        | _ -> loop t state)
    | `Key key -> update t (Obuffer.update_on_key state key)
    | _ -> loop t state
  in
  let t = Term.create () in
  update t init;
  Term.release t

let () = main
