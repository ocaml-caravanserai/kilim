let () =
  Kilim.Private.set_error_handler Js.error ;
  Kilim.Private.set_warning_handler Js.warn

module Private = struct
  let host = Browser_host.host

  let live_timers () = !Browser_host.live

  let cell = Listen.cell

  type app = Mount.app

  let start el app = Mount.start (Brr.El.to_jv el) app

  let mounted el = Mount.mounted (Brr.El.to_jv el)

  let ops (a : app) = a.ops

  let runtime (a : app) = a.rt

  let stop = Mount.stop
end

let run = Mount.run
