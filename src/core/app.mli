(** An application (design section 08): the root instance. *)

type t =
  | App :
      { init: unit -> 'model * 'msg Cmd.t
      ; update: 'model -> 'msg -> 'model * 'msg Cmd.t
      ; view: 'model -> 'msg View.t
      ; subs: ('model -> 'msg Sub.t) option
      ; fallback: (exn -> 'msg View.t) option }
      -> t

val make :
     init:(unit -> 'model * 'msg Cmd.t)
  -> update:('model -> 'msg -> 'model * 'msg Cmd.t)
  -> view:('model -> 'msg View.t)
  -> ?subs:('model -> 'msg Sub.t)
  -> ?fallback:(exn -> 'msg View.t)
  -> unit
  -> t
