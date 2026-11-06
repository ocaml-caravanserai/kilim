(* The one error handler of the runtime: it gets the exceptions that have no
   error scope (design section 09). The default does nothing. *)

let handler : (exn -> unit) ref = ref (fun _ -> ())

let error e = !handler e
