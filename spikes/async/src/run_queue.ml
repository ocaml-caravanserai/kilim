(* The run queue of Async: continuations that wait for the drain. *)

type t =
  { q: (unit -> unit) Queue.t
  ; mutable depth: int
  ; mutable draining: bool
  ; mutable in_turn: bool
  ; schedule: unit -> unit (* asks the host for a turn *) }

let max_depth = 256

let create ~schedule =
  {q= Queue.create (); depth= 0; draining= false; in_turn= false; schedule}

let is_empty r = Queue.is_empty r.q

(* [over ()] reads the clock. The drain asks it before the first continuation
   and then once every 256 continuations. If it gives true, the drain stops, and
   the rest stays in the queue. *)
let drain ?(over = fun () -> false) r =
  if not r.draining then begin
    r.draining <- true ;
    let n = ref 0 and stop = ref false in
    if (not (Queue.is_empty r.q)) && over () then stop := true ;
    ( try
        while (not !stop) && not (Queue.is_empty r.q) do
          (Queue.pop r.q) () ;
          incr n ;
          if !n land 255 = 0 && over () then stop := true
        done
      with e ->
        r.draining <- false ;
        raise e ) ;
    r.draining <- false
  end

(* A push outside a turn asks the host for a turn. *)
let push r f =
  let was_empty = Queue.is_empty r.q in
  Queue.push f r.q ;
  if was_empty && (not r.draining) && not r.in_turn then r.schedule ()

(* The trampoline: run [f] at once while the depth of synchronous steps is below
   the limit. Above the limit, put it on the queue. *)
let step r f =
  if r.depth < max_depth then begin
    r.depth <- r.depth + 1 ;
    match f () with
    | () ->
        r.depth <- r.depth - 1
    | exception e ->
        r.depth <- r.depth - 1 ;
        raise e
  end
  else push r f
