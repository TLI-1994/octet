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
    | `Key (`Enter, _) -> ()
    | `Key (`ASCII ch, _) -> update t (Obuffer.insert_ascii state ch)
    (* | `Key (`Arrow a, _) -> update t (Obuffer.move state a)*)
    | _ -> loop t state
  in
  let t = Term.create () in
  update t init;
  Term.release t

let () = main
