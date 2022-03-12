(*open Notty*)
open Notty_unix
open Buffermanager

let init_l = Obuffer.empty |> init |> toggle_focus
let init_r = Obuffer.empty |> init |> toggle_focus
let init_bm = init_l <|> init_r

let main =
  let rec update t state =
    Term.image t (to_image (Term.size t) state);
    loop t state
  and loop t state =
    match Term.event t with
    | `Key (`ASCII 'X', [ `Ctrl ]) -> (
        match Term.event t with
        | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
        | _ -> loop t state)
    | `Key key -> update t (update_all key state)
    | `Resize _ -> update t state
    | _ -> loop t state
  in
  let t = Term.create () in
  update t init_bm;
  Term.release t

let () = main
