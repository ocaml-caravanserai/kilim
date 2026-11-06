(* A program that uses each module of kilim. test/link examines the units that
   it links (design section 13). *)
open Kilim
open Async.Syntax

let () =
  let tok = Private.Cancel.root () in
  Cancel.on_end tok ignore ;
  let q = Private.Run_queue.create ~schedule:ignore in
  let host =
    { Private.Host.now_ms= (fun () -> 0.)
    ; set_timer= (fun ~ms:_ _ () -> ())
    ; request_frame= ignore
    ; queue_microtask= ignore
    ; post_task= ignore }
  in
  let t =
    Async.timeout (Span.s 1)
      (Async.first
         [ (let+ x = Async.return 1
            and+ y = Async.make (fun _ ~ok ~error:_ -> ok 2) in
            x + y ) ] )
  in
  Private.Async.start ~host ~run:q tok t ~ok:ignore ~error:ignore ;
  Private.Run_queue.drain q ;
  Private.Cancel.cancel tok
