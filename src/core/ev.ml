(* The events (design sections 08 and 11). The signature and its text are in
   kilim.mli. *)

type 'msg t = 'msg View.handler

(* Event.bubbles has the one list of the events that do not bubble. *)
let make event payload prevent_default stop f =
  { View.event
  ; bubbles= Event.bubbles event
  ; prevent_default
  ; stop
  ; filter= Dom.Any
  ; decode= View.Decode (payload, f) }

let click ?(prevent_default = false) ?(stop = false) m =
  make "click" Dom.Unit prevent_default stop (fun () -> m)

let check ?(prevent_default = false) ?(stop = false) f =
  make "click" Dom.Bool prevent_default stop f

let input ?(prevent_default = false) ?(stop = false) f =
  make "input" Dom.String prevent_default stop f

let change ?(prevent_default = false) ?(stop = false) f =
  make "change" Dom.String prevent_default stop f

let submit ?(prevent_default = false) ?(stop = false) m =
  make "submit" Dom.Unit prevent_default stop (fun () -> m)

let keydown ?(prevent_default = false) ?(stop = false) f =
  make "keydown" Dom.Key prevent_default stop f

let keyup ?(prevent_default = false) ?(stop = false) f =
  make "keyup" Dom.Key prevent_default stop f

let focus ?(prevent_default = false) ?(stop = false) m =
  make "focusin" Dom.Unit prevent_default stop (fun () -> m)

let blur ?(prevent_default = false) ?(stop = false) m =
  make "focusout" Dom.Unit prevent_default stop (fun () -> m)

let mouseenter ?(prevent_default = false) ?(stop = false) m =
  make "mouseenter" Dom.Unit prevent_default stop (fun () -> m)

let mouseleave ?(prevent_default = false) ?(stop = false) m =
  make "mouseleave" Dom.Unit prevent_default stop (fun () -> m)

let scroll ?(prevent_default = false) ?(stop = false) m =
  make "scroll" Dom.Unit prevent_default stop (fun () -> m)

let wheel ?(prevent_default = false) ?(stop = false) m =
  make "wheel" Dom.Unit prevent_default stop (fun () -> m)

let touchstart ?(prevent_default = false) ?(stop = false) m =
  make "touchstart" Dom.Unit prevent_default stop (fun () -> m)

let touchmove ?(prevent_default = false) ?(stop = false) m =
  make "touchmove" Dom.Unit prevent_default stop (fun () -> m)

(* A fixed rule of the listener, as prevent_default: no user code decides. *)
let link_click ?(stop = false) m =
  {(make "click" Dom.Unit true stop (fun () -> m)) with filter= Dom.Plain_click}
