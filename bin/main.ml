open Notty
open Notty_unix

let a = Obuffer.empty

let wrap2 width img =
  let rec go off =
    I.hcrop off 0 img
    :: (if I.width img - off > width then go (off + width) else [])
  in
  go 0 |> I.vcat |> I.hsnap ~align:`Left width

let img str = I.string A.empty str

let main =
  let rec update t state =
    Term.image t (wrap2 80 (img state));
    loop t state
  and loop t state =
    match Term.event t with
    | `Key (`Enter, _) -> ()
    | `Key (`Arrow `Left, _) -> update t (state ^ "left")
    | `Key (`Arrow `Right, _) -> update t (state ^ "right")
    | `Key (`ASCII 'X', [ `Ctrl ]) -> (
        match Term.event t with
        | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
        | `Key (`ASCII 'F', [ `Ctrl ]) -> update t (state ^ "findfile")
        | `Key (`ASCII 'S', [ `Ctrl ]) -> update t (state ^ "savefile")
        | _ -> loop t state)
    | `Key (`ASCII ch, _) -> update t (state ^ String.make 1 ch)
    | `Key (`Backspace, _) ->
        update t (String.sub state 0 (String.length state - 1))
    | _ -> loop t state
  in
  let t = Term.create () in
  update t "";
  Term.release t

let () = main
