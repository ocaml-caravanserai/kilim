(* Cmd.latest (design section 08): a stale result is dropped, a task that a
   newer key cancels before its start never starts, and the scopes. *)

open Windtrap
open Kilim
open Support

type msg = Typed of string | Got of string | Failed of string | Scoped of int

type state =
  { mutable starts: string list  (** The newest first. *)
  ; oks: (string * (string -> unit)) Queue.t
  ; errors: (string * (exn -> unit)) Queue.t
  ; senders: (msg -> unit) Queue.t
  ; mutable got: string list }

let search ?(scoped = fun _ -> Cmd.none) () =
  let s =
    { starts= []
    ; oks= Queue.create ()
    ; errors= Queue.create ()
    ; senders= Queue.create ()
    ; got= [] }
  in
  let task q =
    Async.make (fun _ ~ok ~error ->
        s.starts <- q :: s.starts ;
        Queue.push (q, ok) s.oks ;
        Queue.push (q, error) s.errors )
  in
  let app =
    App.make
      ~init:(fun () ->
        ("", Cmd.batch (List.init 4 (fun _ -> Cmd.task (waiting s.senders)))) )
      ~update:(fun m -> function
        | Typed q ->
            (q, Cmd.latest ~key:"fetch" (Async.map (fun r -> Got r) (task q)))
        | Got r ->
            s.got <- r :: s.got ;
            (m, Cmd.none)
        | Failed _ ->
            (m, Cmd.none)
        | Scoped n ->
            (m, scoped n) )
      ~view:(fun m -> El.div [txt m; button (Scoped 0) "s"])
      ()
  in
  (app, s)

let send s m = (Queue.pop s.senders) m

let ok s q = (List.assoc q (List.of_seq (Queue.to_seq s.oks))) q

(* The result of "a" comes in the same turn as "ab", after it. *)
let stale () =
  let app, s = search () in
  let p = P.start app in
  let h = P.host p in
  H.raw h (fun () -> send s (Typed "a")) ;
  P.run p ;
  equal (list string) ["a"] s.starts ;
  equal int 1 (ins p).latest_keys ;
  H.raw h (fun () -> send s (Typed "ab") ; ok s "a") ;
  P.run p ;
  equal (list string) ["ab"; "a"] s.starts ;
  equal int 1 (stats p).dropped ;
  equal (list string) [] s.got ;
  equal int 1 (ins p).latest_keys ;
  ok s "ab" ;
  P.run p ;
  equal (list string) ["ab"] s.got ;
  equal int 0 (ins p).latest_keys ;
  equal int ~msg:"Typed a, Typed ab, Got ab" 3 (stats p).messages ;
  no_errors p

(* Two key presses in one turn: the first task never starts. *)
let never_starts () =
  let app, s = search () in
  let p = P.start app in
  let h = P.host p in
  H.raw h (fun () -> send s (Typed "a") ; send s (Typed "ab")) ;
  P.run p ;
  equal (list string) ["ab"] s.starts ;
  equal int 1 (ins p).latest_keys ;
  equal int 0 (stats p).dropped ;
  no_errors p

(* A key under a scope is a path. *)
let scopes () =
  let count scoped =
    let app, s = search ~scoped () in
    let p = P.start app in
    ignore s ;
    click p ;
    no_errors p ;
    equal int 0 (ins p).latest_keys ;
    (* A cancelled request does not start: its result never waits in the
       queue. *)
    equal int 0 (stats p).dropped ;
    (stats p).messages - 1
  in
  let fetch k n = Cmd.latest ~key:k (Async.return (Got (string_of_int n))) in
  equal int 2
    (count (fun _ ->
         Cmd.batch
           [ Cmd.map ~scope:"left" Fun.id (fetch "fetch" 1)
           ; Cmd.map ~scope:"right" Fun.id (fetch "fetch" 2) ] ) ) ;
  equal int 1
    (count (fun _ ->
         Cmd.batch
           [ Cmd.map ~scope:"left" Fun.id (fetch "fetch" 1)
           ; Cmd.map ~scope:"left" Fun.id (fetch "fetch" 2) ] ) ) ;
  equal int 2
    (count (fun _ ->
         Cmd.batch [fetch "a/b" 1; Cmd.map ~scope:"a" Fun.id (fetch "b" 2)] ) ) ;
  equal int 1 (count (fun _ -> Cmd.batch [fetch "k" 1; fetch "k" 2])) ;
  equal int 2 (count (fun _ -> Cmd.batch [fetch "k" 1; fetch "j" 2])) ;
  equal int 2
    (count (fun _ ->
         Cmd.batch
           [Cmd.task (Async.return (Got "x")); Cmd.task (Async.return (Got "y"))] )
    )

(* An error of a stale task that waits in the queue is dropped, with no
   error. *)
let stale_error () =
  let app, s = search () in
  let p = P.start app in
  let h = P.host p in
  let error q = List.assoc q (List.of_seq (Queue.to_seq s.errors)) in
  H.raw h (fun () -> send s (Typed "a")) ;
  P.run p ;
  H.raw h (fun () -> send s (Typed "ab") ; error "a" Exit) ;
  P.run p ;
  equal int 1 (stats p).dropped ;
  is_true (ins p).alive ;
  no_errors p ;
  (* The error of the newest task goes to the root. *)
  error "ab" Exit ;
  P.run p ;
  is_false (ins p).alive ;
  equal int 1 (List.length (errors p)) ;
  equal int 0 (ins p).latest_keys

let tests =
  [ group "latest"
      [ test "a stale result in the queue is dropped" stale
      ; test "a task that a newer key cancels never starts" never_starts
      ; test "a key under a scope is a path" scopes
      ; test "a stale error is dropped" stale_error ] ]
