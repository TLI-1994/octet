(*open Notty*)
open Notty_unix

let init = Obuffer.empty
let img = Orender.image_of_buffer
let img2 buffer = Obuffer.to_image buffer 0 (24, 80) true

let main =
  let rec update t state =
    Term.image t (img2 state);
    loop t state
  and loop t state =
    match Term.event t with
    | `Key (`ASCII 'X', [ `Ctrl ]) -> (
        match Term.event t with
        | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
        | _ -> loop t state)
    | `Key (`Backspace, _) -> update t (Obuffer.delete state)
    | `Key (`Enter, _) -> update t (Obuffer.insert_newline state)
    | `Key (`Arrow direxn, _) ->
        update t (Obuffer.mv_cursor state direxn)
    | `Key (`ASCII ch, _) ->
        update t (Obuffer.insert_ascii state ch)
        (* | `Key (`Arrow a, _) -> update t (Obuffer.move state a)*)
    | _ -> loop t state
  in
  let t = Term.create () in
  update t init;
  Term.release t

let () = main
