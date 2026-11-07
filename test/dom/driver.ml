(* The page side of run.mjs: the drive channel, the log and the result. *)

let console = Jv.get Jv.global "console"

let log s = ignore (Jv.call console "log" [|Jv.of_string s|])

let failures = ref 0

let check name ok =
  log ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

(* The last line of the page: run.mjs waits for it. *)
let finish () = log (if !failures = 0 then "DONE ok" else "DONE FAIL")

(* The drives that wait for kilimDriven, by id. *)
let next = ref 0

let pending : (int * (unit -> unit)) list ref = ref []

let () =
  Jv.set Jv.global "kilimDriven"
    (Jv.callback ~arity:1 (fun id ->
         let id = Jv.to_int id in
         match List.assoc_opt id !pending with
         | Some ok ->
             pending := List.remove_assoc id !pending ;
             ok ()
         | None ->
             () ) )

let json = Jv.get Jv.global "JSON"

let request fields =
  incr next ;
  let id = !next in
  let fut, set = Fut.create () in
  pending := (id, set) :: !pending ;
  let o = Jv.obj (Array.append [|("id", Jv.of_int id)|] fields) in
  ignore (Jv.call Jv.global "kilimDrive" [|Jv.call json "stringify" [|o|]|]) ;
  fut

(* The server sends the body of the page. *)
let release () = request [|("release", Jv.true')|]

(* DevTools input commands, for example [Input.dispatchMouseEvent]. *)
let drive steps =
  request
    [| ( "steps"
       , Jv.of_list
           (fun (m, params) -> Jv.of_jv_array [|Jv.of_string m; params|])
           steps ) |]

let frame () =
  let fut, set = Fut.create () in
  ignore
    (Jv.call Jv.global "requestAnimationFrame"
       [|Jv.callback ~arity:1 (fun _ -> set ())|] ) ;
  fut

(* One task of the host: the same MessageChannel as the tasks of kilim. *)
let task () =
  let fut, set = Fut.create () in
  (Kilim_browser.Private.host ()).post_task set ;
  fut

let sleep ms =
  let fut, set = Fut.create () in
  ignore
    (Jv.call Jv.global "setTimeout"
       [|Jv.callback ~arity:1 (fun _ -> set ()); Jv.of_int ms|] ) ;
  fut

(* The document is not "loading". *)
let loaded () =
  let fut, set = Fut.create () in
  let document = Jv.get Jv.global "document" in
  if not (Jv.to_string (Jv.get document "readyState") = "loading") then set ()
  else
    ignore
      (Jv.call document "addEventListener"
         [| Jv.of_string "DOMContentLoaded"
          ; Jv.callback ~arity:1 (fun _ -> set ())
          ; Jv.obj [|("once", Jv.true')|] |] ) ;
  fut
