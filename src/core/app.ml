type t =
  | App :
      { init: unit -> 'model * 'msg Cmd.t
      ; update: 'model -> 'msg -> 'model * 'msg Cmd.t
      ; view: 'model -> 'msg View.t
      ; subs: ('model -> 'msg Sub.t) option
      ; fallback: (exn -> 'msg View.t) option }
      -> t

let make ~init ~update ~view ?subs ?fallback () =
  App {init; update; view; subs; fallback}
