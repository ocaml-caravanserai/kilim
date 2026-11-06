type t =
  { q: (unit -> unit) Queue.t
  ; mutable depth: int
  ; mutable draining: bool
  ; mutable in_turn: bool
  ; schedule: unit -> unit }

let max_depth = 256

let create ~schedule =
  {q= Queue.create (); depth= 0; draining= false; in_turn= false; schedule}

let length r = Queue.length r.q

let is_empty r = Queue.is_empty r.q

let with_turn r f =
  let outer = r.in_turn in
  r.in_turn <- true ;
  match f () with
  | v ->
      r.in_turn <- outer ;
      v
  | exception e ->
      r.in_turn <- outer ;
      raise e

let push r f =
  let was_empty = Queue.is_empty r.q in
  Queue.push f r.q ;
  if was_empty && (not r.draining) && not r.in_turn then r.schedule ()

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

let drain ?(over = fun () -> false) r =
  if not r.draining then begin
    r.draining <- true ;
    let rec loop n =
      if not (Queue.is_empty r.q) then
        if n land 255 = 0 && over () then ()
        else begin
          (Queue.pop r.q) () ;
          loop ((n + 1) [@mutate off "n - 1 also reaches each multiple of 256"])
        end
    in
    match loop 0 with
    | () ->
        r.draining <- false
    | exception e ->
        r.draining <- false ;
        raise e
  end
