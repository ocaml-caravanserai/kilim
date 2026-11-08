(* The start, the stop and [run] (design section 08). *)

module Event = Kilim.Private.Event
module Runtime = Kilim.Private.Runtime

type app =
  { st: Listen.state
  ; ops: Jv.t Kilim.Private.Dom.t
  ; rt: Jv.t Runtime.t
  ; delegated: (string * Jv.t) list
  ; visibility: Jv.t  (** The [visibilitychange] listener. *) }

let add target name f =
  ignore (Jv.call target "addEventListener" [|Jv.of_string name; f|])

let remove target name f =
  ignore (Jv.call target "removeEventListener" [|Jv.of_string name; f|])

(* A trap of wasm_of_ocaml (a [WebAssembly.RuntimeError]) goes past each OCaml
   catch and leaves its turn open. The browser reports it with an [error] event
   on window (or an [unhandledrejection] event for a trap in a promise
   reaction). The listener can run inside a live turn: for example, a listener
   of the page that update reaches by a synthetic event traps, and the update
   goes on. Thus the listener queues [Runtime.abort] in a microtask: a microtask
   runs only when the JS stack is empty, then each open turn is dead. One
   listener for the page, at the first start, only in the wasm_of_ocaml build
   and if [WebAssembly] exists. The js_of_ocaml build has no traps, and a page
   with no WebAssembly (for example Chrome with no JIT) runs only that build.

   WebKit 27.2 gives a trap of a [wasm:js-string] function (the cast of a value
   that is not a string) as an [error] event whose [error] is null, with the
   message "Script error.". Thus for a null [error], the listener queues
   [Runtime.abort_open]: it aborts only if a turn is still open. In the
   microtask, each open turn is dead, thus this is correct for each event with a
   null [error], also for an error of a script of another origin. With no open
   turn, it gives no report. *)
let object_js = Jstr.v "object"

let string_js = Jstr.v "string"

let traps =
  lazy
    ( match Sys.backend_type with
    | Other "wasm_of_ocaml"
      when Jstr.equal (Jv.typeof (Jv.get Jv.global "WebAssembly")) object_js ->
        let runtime_error =
          Jv.get (Jv.get Jv.global "WebAssembly") "RuntimeError"
        in
        let later f =
          Kilim_web.Microtask.queue (Jv.callback ~arity:1 (fun _ -> f ()))
        in
        let on e =
          if Jv.instanceof e ~cons:runtime_error then
            later (fun () -> Runtime.abort (Jv.Error (Jv.to_error e)))
        in
        add Jv.global "error"
          (Jv.callback ~arity:1 (fun ev ->
               let e = Jv.get ev "error" in
               if Jv.is_null e then
                 let m = Jv.get ev "message" in
                 let m =
                   if Jstr.equal (Jv.typeof m) string_js then Jv.to_jstr m
                   else Jstr.v "Script error."
                 in
                 later (fun () -> Runtime.abort_open (Jv.Error (Jv.Error.v m)))
               else on e ) ) ;
        add Jv.global "unhandledrejection"
          (Jv.callback ~arity:1 (fun ev -> on (Jv.get ev "reason")))
    | Native | Bytecode | Other _ ->
        () )

(* The mark of a mount element with a live app: the app. *)
let mark = "__kilimApp"

let mounted root =
  let a = Jv.get root mark in
  if Jv.is_undefined a then None else Some (Jv.Id.of_jv a : app)

(* The children of the element go, then the app, its mark and its listeners
   come, then the first turn renders the app. Thus a trap in the first turn
   leaves an app that [stop] can end. *)
let start root app =
  if not (Jv.is_undefined (Jv.get root mark)) then
    invalid_arg "Kilim_browser: the mount element has a live app" ;
  Lazy.force traps ;
  ignore (Jv.call root "replaceChildren" [||]) ;
  let st = {Listen.root; live= None} in
  let ops = Ops.make st in
  let rt = Runtime.create Browser_host.env ops ~parent:root app in
  st.live <- Some (Listen.env st ops, rt) ;
  let delegated =
    List.map
      (fun name ->
        let f = Listen.delegated st name in
        add root name f ; (name, f) )
      Event.delegated_events
  in
  let visibility =
    Jv.callback ~arity:1 (fun _ -> Runtime.visibility_changed rt)
  in
  add (Lazy.force Js.document) "visibilitychange" visibility ;
  let a = {st; ops; rt; delegated; visibility} in
  Jv.set root mark (Jv.repr a) ;
  Runtime.init rt ;
  a

(* The direct listeners stay on the removed elements, where the runtime does
   nothing after the stop. *)
let stop a =
  Runtime.stop a.rt ;
  if Jv.strict_equal (Jv.get a.st.root mark) (Jv.repr a) then
    Jv.set a.st.root mark Jv.undefined ;
  List.iter (fun (name, f) -> remove a.st.root name f) a.delegated ;
  remove (Lazy.force Js.document) "visibilitychange" a.visibility

let loading_js = Jv.of_string "loading"

(* A strict compare, not a conversion: an element with the name [readyState]
   hides the property of the document. *)
let loading () =
  Jv.strict_equal (Jv.get (Lazy.force Js.document) "readyState") loading_js

let find id =
  Jv.call (Lazy.force Js.document) "getElementById" [|Jv.of_string id|]

let missing id = Invalid_argument ("Kilim_browser.run: no element with id " ^ id)

(* The first readystatechange comes at the state "interactive": the listener
   removes itself and looks again. *)
let run ~mount app =
  let el = find mount in
  if not (Jv.is_null el) then ignore (start el app)
  else if loading () then begin
    let document = Lazy.force Js.document in
    let self = ref Jv.undefined in
    let wait _ =
      remove document "readystatechange" !self ;
      let el = find mount in
      if Jv.is_null el then Kilim.Private.error (missing mount)
      else ignore (start el app)
    in
    self := Jv.callback ~arity:1 wait ;
    add document "readystatechange" !self
  end
  else raise (missing mount)
