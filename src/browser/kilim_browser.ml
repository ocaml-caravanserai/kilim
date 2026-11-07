let () =
  Kilim.Private.set_error_handler Js.error ;
  Kilim.Private.set_warning_handler Js.warn

module Private = struct
  let host = Browser_host.host

  let live_timers () = !Browser_host.live
end
