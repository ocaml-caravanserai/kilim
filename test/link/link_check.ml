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

(* The view, the keyed diff and the DOM operations, on a DOM of unit nodes. *)
let () =
  let dom : unit Private.Dom.t =
    { create_element= (fun _ _ -> ())
    ; create_text= ignore
    ; set_text= (fun () _ -> ())
    ; set_attribute= (fun () _ _ -> ())
    ; remove_attribute= (fun () _ -> ())
    ; set_property= (fun () _ -> ())
    ; get_value= (fun () -> "")
    ; get_checked= (fun () -> false)
    ; insert_before= (fun () () _ -> ())
    ; remove= (fun () () -> ())
    ; replace= (fun () () () -> ())
    ; clear= ignore
    ; set_inner_html= (fun () _ -> ())
    ; attach= (fun () _ -> ())
    ; listen= (fun () _ ~passive:_ -> ())
    ; unlisten= (fun () _ -> ()) }
  in
  let view keys =
    El.div
      ~at:[At.id "a"; At.href "/x"; At.v "srcset" "a.png 1x"; At.value "v"]
      ~ev:[Ev.click 0; Ev.input String.length; Ev.scroll 1]
      [ txt "x"
      ; View.keyed El.ul (List.map (fun k -> (Key.int k, El.li [txt "r"])) keys)
      ; View.unsafe_html ~el:El.div (Unsafe_html.of_string "<b>x</b>")
      ; El.svg [El.foreign_object [El.p []]] ]
  in
  let t = Private.Diff.mount dom ~parent:() ~dispatch:ignore (view [1; 2; 3]) in
  Private.Diff.patch t (view [3; 1; 4]) ;
  ignore (Private.Diff.children (Private.Diff.root t))
