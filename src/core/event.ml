let discrete = function
  | "click" | "input" | "change" | "submit" | "keydown" ->
      true
  | _ ->
      false

let bubbles = function
  | "mouseenter"
  | "mouseleave"
  | "scroll"
  | "wheel"
  | "touchstart"
  | "touchmove" ->
      false
  | _ ->
      true

let delegated_events =
  [ "click"
  ; "input"
  ; "change"
  ; "submit"
  ; "keydown"
  ; "keyup"
  ; "focusin"
  ; "focusout"
  ; "compositionstart"
  ; "compositionend" ]

let direct_events =
  ["mouseenter"; "mouseleave"; "scroll"; "wheel"; "touchstart"; "touchmove"]

let walk ~parent ~cell target name f =
  let up = bubbles name in
  let rec go node =
    let stop =
      match Option.bind (cell node) (fun c -> Dom.handler c name) with
      | Some (Dom.Handler h as handler) ->
          f handler ; h.stop
      | None ->
          false
    in
    if up && not stop then match parent node with Some p -> go p | None -> ()
  in
  go target
