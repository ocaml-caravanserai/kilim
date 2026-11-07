type t =
  | App :
      { init: unit -> 'model * 'msg Cmd.t
      ; update: 'model -> 'msg -> 'model * 'msg Cmd.t
      ; view: 'model -> 'msg View.t
      ; fallback: (exn -> 'msg View.t) option }
      -> t

let make ~init ~update ~view ?fallback () = App {init; update; view; fallback}
