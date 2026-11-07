(* The browser host (design section 12): one host for the page, shared by all
   apps. One FIFO for each queue keeps the order of each app. *)

module Host = Kilim.Private.Host
module Runtime = Kilim.Private.Runtime

let live = ref 0

let now_ms () = Jv.to_float (Jv.call (Lazy.force Js.performance) "now" [||])

(* The count goes up only after setTimeout gives its id, and down at the run or
   at the first clear. A cleared timer does not run. *)
let set_timer ~ms f =
  let pending = ref false in
  let run _ =
    pending := false ;
    decr live ;
    f ()
  in
  let id =
    Jv.call Jv.global "setTimeout" [|Jv.callback ~arity:1 run; Jv.of_int ms|]
  in
  pending := true ;
  incr live ;
  fun () ->
    if !pending then begin
      pending := false ;
      decr live ;
      ignore (Jv.call Jv.global "clearTimeout" [|id|])
    end

let request_frame f =
  ignore
    (Jv.call Jv.global "requestAnimationFrame"
       [|Jv.callback ~arity:1 (fun _ -> f ())|] )

(* One JS microtask for each item keeps the FIFO order, with one JS function for
   all items. The JS call comes first: if it raises, no item waits with no
   callback. *)
let microtasks = Queue.create ()

let run_microtask =
  lazy (Jv.callback ~arity:1 (fun _ -> (Queue.take microtasks) ()))

let queue_microtask f =
  ignore (Jv.call Jv.global "queueMicrotask" [|Lazy.force run_microtask|]) ;
  Queue.push f microtasks

(* The same form on one MessageChannel, made at the first post. A hidden page
   does not throttle its messages. *)
let tasks = Queue.create ()

let port =
  lazy
    (let ch = Jv.new' (Jv.get Jv.global "MessageChannel") [||] in
     Jv.set (Jv.get ch "port1") "onmessage"
       (Jv.callback ~arity:1 (fun _ -> (Queue.take tasks) ())) ;
     Jv.get ch "port2" )

let post_task f =
  ignore (Jv.call (Lazy.force port) "postMessage" [|Jv.null|]) ;
  Queue.push f tasks

let host = {Host.now_ms; set_timer; request_frame; queue_microtask; post_task}

let hidden_js = lazy (Jv.of_string "hidden")

let hidden () =
  Jv.strict_equal
    (Jv.get (Lazy.force Js.document) "visibilityState")
    (Lazy.force hidden_js)

(* The model compare. Under js_of_ocaml, [==] is [===], which does not keep 0.
   and -0. apart: thus [Object.is]. Under wasm_of_ocaml, a float is a box and
   [==] compares the boxes. *)
type same = {same: 'a. 'a -> 'a -> bool}

let object_is a b =
  Jv.to_bool (Jv.call (Lazy.force Js.object_) "is" [|Jv.repr a; Jv.repr b|])

let same =
  match Sys.backend_type with
  | Other "js_of_ocaml" ->
      {same= object_is}
  | Native | Bytecode | Other _ ->
      {same= ( == )}

let env = {Runtime.host; hidden; cost= ignore; same= same.same}
