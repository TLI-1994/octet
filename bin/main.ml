open Notty_unix
open Octet
module FileBuffer = Filebuffer.Make (Bytebuffer)

(* module FileBuffer = Obuffer *)
open Buffermanager.Make (FileBuffer)

let init_l =
  FileBuffer.from_file "data/input.txt" |> init |> toggle_focus

let init_r = FileBuffer.empty () |> init
let init_bm = init_l <|> init_r <-> empty_minibuffer

let main =
  let rec update t state =
    Term.image t (to_image (Term.size t) state);
    loop t state
  and loop t state =
    match Term.event t with
    | `Key (`ASCII 'N', [ `Ctrl ]) -> update t (autoformat state)
    | `Key (`ASCII 'P', [ `Ctrl ]) ->
        update t (paste_from_clipboard state)
    | `Key (`ASCII 'X', [ `Ctrl ]) -> (
        match Term.event t with
        | `Key (`ASCII 'B', [ `Ctrl ]) -> update t (minibuffer_on state)
        | `Key (`ASCII 'N', [ `Ctrl ]) ->
            update t (minibuffer_off (perform_mb_command state))
        | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
        | `Key (`ASCII 'S', [ `Ctrl ]) -> update t (toggle_focus state)
        | _ -> loop t state)
    | `Key key -> update t (update_all key state)
    | `Resize _ -> update t state
    | _ -> loop t state
  in
  let t = Term.create () in
  update t init_bm;
  Term.release t

let () = main