(* The tests of kilim.storage with Storage_mock (design sections 13 and 14). *)

open Windtrap
open Kilim
module S = Kilim_storage
module H = Kilim_testing.Host
module P = Kilim_testing.Page
module By = Kilim_testing.By
module M = Kilim_testing.Storage_mock

let show_get = function
  | Ok (Some v) ->
      "some " ^ v
  | Ok None ->
      "none"
  | Error e ->
      S.error_to_string e

let show_unit = function Ok () -> "ok" | Error e -> S.error_to_string e

type msg = Got of string

(* An app whose init task reads [key] of Local. *)
let app ?(key = "nick") () =
  App.make
    ~init:(fun () ->
      ([], Cmd.task (Async.map (fun r -> Got (show_get r)) (S.get Local key))) )
    ~update:(fun l (Got s) -> (s :: l, Cmd.none))
    ~view:(fun l ->
      El.p ~at:[At.data "testid" "log"] [txt (String.concat "," (List.rev l))] )
    ()

let log p = Kilim_testing.Fake_dom.text (P.find p (By.test_id "log"))

let with_page ?host ?key f =
  let p = P.start ?host (app ?key ()) in
  Fun.protect ~finally:(fun () -> P.stop p) (fun () -> f p)

(* A task in a new app on the host of [p]: its result in the first turn. *)
let result host t =
  let r = ref "" in
  let q =
    P.start ~host
      (App.make
         ~init:(fun () -> ((), Cmd.task (Async.map (fun s -> r := s) t)))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () -> txt "")
         () )
  in
  P.stop q ; !r

let init_value () =
  let host = H.create () in
  M.preset host Local [("nick", "ann"); ("other", "x")] ;
  M.preset host Session [("nick", "sam")] ;
  with_page ~host (fun p ->
      equal string ~msg:"the init task gives the value" "some ann" (log p) ;
      equal int ~msg:"one render" 1 (P.stats p).renders ;
      equal
        (list (pair string string))
        ~msg:"contents, sorted"
        [("nick", "ann"); ("other", "x")]
        (M.contents p Local) ;
      equal (list (pair string string)) [("nick", "sam")] (M.contents p Session) ) ;
  with_page ~key:"none" (fun p -> equal string "none" (log p))

let round_trip () =
  with_page (fun p ->
      let host = P.host p in
      let get a k = result host (Async.map show_get (S.get a k)) in
      let set a k v = result host (Async.map show_unit (S.set a k v)) in
      let remove a k = result host (Async.map show_unit (S.remove a k)) in
      equal string "ok" (set Local "a" "1") ;
      equal string "ok" (set Local "a" "2") ;
      equal string "ok" (set Session "a" "s") ;
      equal string "some 2" (get Local "a") ;
      equal string "some s" (get Session "a") ;
      equal (list (pair string string)) [("a", "2")] (M.contents p Local) ;
      equal string "ok" (remove Local "a") ;
      equal string "ok" (remove Local "a") ;
      equal string "none" (get Local "a") ;
      equal string ~msg:"the other area stays" "some s" (get Session "a") ;
      equal string "ok" (remove Session "a") ;
      equal (list (pair string string)) [] (M.contents p Session) )

let blocked () =
  with_page (fun p ->
      let host = P.host p in
      M.block p ;
      equal string "unavailable"
        (result host (Async.map show_get (S.get Local "a"))) ;
      equal string "unavailable"
        (result host (Async.map show_unit (S.set Session "a" "1"))) ;
      equal string "unavailable"
        (result host (Async.map show_unit (S.remove Local "a"))) ;
      equal
        (list (pair string string))
        ~msg:"no write" [] (M.contents p Session) )

let errors () =
  equal string "quota exceeded" (S.error_to_string Quota_exceeded) ;
  equal string "other TypeError" (S.error_to_string (Other "TypeError"))

let no_driver () =
  S.Private.set_driver None ;
  let host = H.create () in
  let got = ref None in
  let q = Private.Run_queue.create ~schedule:ignore in
  Private.Async.start ~host:(H.host host) ~run:q (Private.Cancel.root ())
    (S.get Local "a")
    ~ok:(fun _ -> got := Some "ok")
    ~error:(function Failure s -> got := Some s | _ -> got := Some "other") ;
  Private.Run_queue.drain q ;
  equal (option string)
    (Some
       "kilim.storage: no driver: call Kilim_storage_browser.install () in \
        main.ml, or use Storage_mock in tests" )
    !got

(* The last host wins, and contents works after the stop. *)
let hosts () =
  let p = P.start (app ()) in
  ignore (result (P.host p) (Async.map show_unit (S.set Local "k" "v"))) ;
  P.stop p ;
  equal
    (list (pair string string))
    ~msg:"after the stop"
    [("k", "v")]
    (M.contents p Local) ;
  let q = P.start (app ()) in
  Fun.protect
    ~finally:(fun () -> P.stop q)
    (fun () ->
      equal string ~msg:"a new host: an empty storage" "none" (log q) ;
      raises
        (Failure "Kilim_testing.Storage_mock: the page is not the last page")
        (fun () -> ignore (M.contents p Local) ) ;
      ignore (result (P.host q) (Async.map show_unit (S.set Local "nick" "q"))) ;
      let r = P.start ~host:(P.host q) (app ()) in
      equal string ~msg:"the same host keeps its storage" "some q" (log r) ;
      P.stop r ;
      M.preset (P.host q) Local [("nick", "z")] ;
      equal
        (list (pair string string))
        ~msg:"preset after the mount"
        [("nick", "z")]
        (M.contents q Local) )

let tests =
  [ group "storage"
      [ test "an init task gives the value before the first render" init_value
      ; test "get, set and remove in the two areas" round_trip
      ; test "a blocked storage" blocked
      ; test "error_to_string" errors
      ; test "no driver" no_driver
      ; test "the last host, the reads after the stop" hosts ] ]

let () = exit (Windtrap.run "storage" tests)
