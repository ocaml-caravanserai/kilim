(* The events (design sections 08 and 11). The signature and its text are in
   kilim.mli. *)

type 'msg t = 'msg View.handler

let make event bubbles payload prevent_default stop f =
  {View.event; bubbles; prevent_default; stop; decode= View.Decode (payload, f)}

let click ?(prevent_default = false) ?(stop = false) m =
  make "click" true Dom.Unit prevent_default stop (fun () -> m)

let check ?(prevent_default = false) ?(stop = false) f =
  make "click" true Dom.Bool prevent_default stop f

let input ?(prevent_default = false) ?(stop = false) f =
  make "input" true Dom.String prevent_default stop f

let change ?(prevent_default = false) ?(stop = false) f =
  make "change" true Dom.String prevent_default stop f

let submit ?(prevent_default = false) ?(stop = false) m =
  make "submit" true Dom.Unit prevent_default stop (fun () -> m)

let keydown ?(prevent_default = false) ?(stop = false) f =
  make "keydown" true Dom.Key prevent_default stop f

let keyup ?(prevent_default = false) ?(stop = false) f =
  make "keyup" true Dom.Key prevent_default stop f

let focus ?(prevent_default = false) ?(stop = false) m =
  make "focusin" true Dom.Unit prevent_default stop (fun () -> m)

let blur ?(prevent_default = false) ?(stop = false) m =
  make "focusout" true Dom.Unit prevent_default stop (fun () -> m)

let mouseenter ?(prevent_default = false) ?(stop = false) m =
  make "mouseenter" false Dom.Unit prevent_default stop (fun () -> m)

let mouseleave ?(prevent_default = false) ?(stop = false) m =
  make "mouseleave" false Dom.Unit prevent_default stop (fun () -> m)

let scroll ?(prevent_default = false) ?(stop = false) m =
  make "scroll" false Dom.Unit prevent_default stop (fun () -> m)
