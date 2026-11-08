type ctx = {cancel: Cancel.t; host: Host.t; run: Run_queue.t}

type 'a t = ctx -> ok:('a -> unit) -> error:(exn -> unit) -> unit

let start ~host ~run cancel t ~ok ~error = t {cancel; host; run} ~ok ~error

let return x : 'a t = fun _ ~ok ~error:_ -> ok x

let fail e : 'a t = fun _ ~ok:_ ~error -> error e

(* The trampoline can defer a start or a result. A deferred item examines its
   token when it runs, thus nothing starts and no result arrives after a
   cancel. *)
let run_if ctx tok f =
  Run_queue.step ctx.run (fun () -> if not (Cancel.cancelled tok) then f ())

(* The trampoline protects the start of [t] (a left-nested chain) and the
   continuation (a right-nested chain). *)
let bind (t : 'a t) (f : 'a -> 'b t) : 'b t =
 fun ctx ~ok ~error ->
  run_if ctx ctx.cancel (fun () ->
      t ctx ~error ~ok:(fun x ->
          run_if ctx ctx.cancel (fun () ->
              match f x with t' -> t' ctx ~ok ~error | exception e -> error e ) ) )

let map f t = bind t (fun x -> return (f x))

(* The callback keeps only [cell]. The end of the step clears it, thus a JS
   callback that never runs does not keep the continuation alive. *)
let started (start : Cancel.t -> ok:('a -> unit) -> error:(exn -> unit) -> unit)
    ctx ~ok ~error =
  if not (Cancel.cancelled ctx.cancel) then begin
    let step = Cancel.child ctx.cancel in
    let cell = ref (Some (ok, error)) in
    Cancel.on_end step (fun () -> cell := None) ;
    (* A cancel of the step clears [cell] last, thus an end action of the user
       can call ok during the cancel. The step is cancelled only through its
       parent, ctx.cancel: the check at the run then drops that result. *)
    let finish pick v =
      match !cell with
      | Some ks ->
          cell := None ;
          Cancel.release step ;
          let k = pick ks in
          (* Never inside the callback. An end action of the release can cancel
             the token (for example a stop of the page): then nothing goes on
             the run queue. A cancelled token never becomes live again, and the
             continuation examines the token again at its run. *)
          if not (Cancel.cancelled ctx.cancel) then
            Run_queue.push ctx.run (fun () ->
                if not (Cancel.cancelled ctx.cancel) then k v )
      | _ ->
          ()
    in
    try start step ~ok:(finish fst) ~error:(finish snd)
    with e ->
      (* The runtime drops the error of a cancelled task. An exception after ok
         or error goes to the error handler. *)
      if Cancel.cancelled step then ()
      else if Option.is_none !cell then Report.error e
      else finish snd e
  end

(* In the release profile, js_of_ocaml can inline a function into its caller,
   for example [make] into the [init] of a component. If the closures of the
   step were in the JS function of the caller, they would share its JS context
   (V8 keeps one shared context for the closures of one JS function). Then the
   callback of a promise that never settles would keep the variables of the
   caller (for example the model), also after the cancel clears [cell]. Thus the
   closures of the step are made in [started], a separate function that
   js_of_ocaml 6.4.1 does not inline into its caller, because it is too large.
   This depends on the inlining rules of js_of_ocaml: the weak test in the
   release profile (test/weak, make test-release) guards it. *)
let make start : 'a t = fun ctx ~ok ~error -> started start ctx ~ok ~error

(* A branch starts under its own token, through the trampoline. *)
let branch ctx tok (t : 'a t) ~ok ~error =
  run_if ctx tok (fun () -> t {ctx with cancel= tok} ~ok ~error)

(* A result goes to the continuation through the trampoline: a deep nest of
   combinators does not overflow the stack under js_of_ocaml, which has no tail
   calls. *)
let deliver ctx k = run_if ctx ctx.cancel k

(* A branch that ends first releases its token, cancels the other branch, and
   delivers. *)
let settle ctx mine other k =
  Cancel.release mine ; Cancel.cancel other ; deliver ctx k

(* Each branch has a child token. A branch that ends releases its token, thus a
   loop of and+ adds no token to the task. The first error cancels the other
   branch: after that, the other branch does not start, and gives no result. *)
let both (a : 'a t) (b : 'b t) : ('a * 'b) t =
 fun ctx ~ok ~error ->
  let ta = Cancel.child ctx.cancel and tb = Cancel.child ctx.cancel in
  let ra = ref None and rb = ref None in
  let check () =
    match (!ra, !rb) with
    | Some x, Some y ->
        deliver ctx (fun () -> ok (x, y))
    | _ ->
        ()
  in
  let fail mine other e = settle ctx mine other (fun () -> error e) in
  branch ctx ta a
    ~ok:(fun x ->
      Cancel.release ta ;
      ra := Some x ;
      check () )
    ~error:(fail ta tb) ;
  branch ctx tb b
    ~ok:(fun y ->
      Cancel.release tb ;
      rb := Some y ;
      check () )
    ~error:(fail tb ta)

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) t f = map f t

  let ( and+ ) = both
end

let sleep span : unit t =
 fun ctx ~ok ~error ->
  make
    (fun tok ~ok ~error:_ ->
      let clear = ctx.host.set_timer ~ms:(Span.to_ms span) (fun () -> ok ()) in
      Cancel.on_end tok clear )
    ctx ~ok ~error

(* The first ok cancels the other branches: after that, they do not start and
   give no result. Thus one ok only arrives, and the count of errors never gets
   to the number of branches after an ok. *)
let first (ts : 'a t list) : 'a t =
 fun ctx ~ok ~error ->
  match ts with
  | [] ->
      error (Invalid_argument "Kilim.Async.first []")
  | _ ->
      let n = List.length ts in
      let failures = ref 0 in
      let toks = List.map (fun _ -> Cancel.child ctx.cancel) ts in
      let cancel_others mine =
        List.iter (fun t -> if t != mine then Cancel.cancel t) toks
      in
      List.iter2
        (fun t tok ->
          branch ctx tok t
            ~ok:(fun x ->
              Cancel.release tok ;
              cancel_others tok ;
              deliver ctx (fun () -> ok x) )
            ~error:(fun e ->
              Cancel.release tok ;
              incr failures ;
              if !failures = n then deliver ctx (fun () -> error e) ) )
        ts toks

(* The first end of [t] or of the timer wins and cancels the other. Then the
   other gives nothing: a sleep under a cancelled token does not start, and each
   task gives one result only and drops a result after a cancel. *)
let timeout span (t : 'a t) : ('a, [`Timeout]) result t =
 fun ctx ~ok ~error ->
  let tt = Cancel.child ctx.cancel and ts = Cancel.child ctx.cancel in
  branch ctx tt t
    ~ok:(fun x -> settle ctx tt ts (fun () -> ok (Ok x)))
    ~error:(fun e -> settle ctx tt ts (fun () -> error e)) ;
  (* An error of the timer: a host whose set_timer raises. *)
  branch ctx ts (sleep span)
    ~ok:(fun () -> settle ctx ts tt (fun () -> ok (Error `Timeout)))
    ~error:(fun e -> settle ctx ts tt (fun () -> error e))
