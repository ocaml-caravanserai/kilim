(* The one error handler of the runtime: it gets the exceptions that have no
   error scope (design section 09). The default does nothing. *)

let handler : (exn -> unit) ref = ref (fun _ -> ())

let error e = !handler e

(* The warning handler: it gets the warnings of the runtime, for example 10
   stops at the time limit in 1 s (design section 11). The default does
   nothing. *)

let warning_handler : (string -> unit) ref = ref (fun _ -> ())

let warn s = !warning_handler s
