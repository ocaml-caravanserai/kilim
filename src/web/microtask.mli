(** [queueMicrotask].

    Web API: [queueMicrotask(f)] of the HTML standard. Brr 0.0.9 has no binding
    for it. The browser host of kilim.browser queues its microtasks with it, and
    the trap listener of the wasm_of_ocaml build queues its abort with it. *)

val queue : Jv.t -> unit
(** [queue f] calls [queueMicrotask(f)]. [f] is a JS function, for example the
    result of [Jv.callback ~arity:1]. The browser calls [f] when the JS stack is
    empty, in the order of the calls of [queue]. If [f] throws, the browser
    reports the error, and the next microtask runs. *)
