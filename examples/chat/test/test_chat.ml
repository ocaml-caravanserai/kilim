(* The native test of the chat example: Websocket_mock, Http_mock and
   Storage_mock on the virtual clock. *)

open Windtrap
open Kilim
open Kilim_testing

let messages p =
  List.map Fake_dom.text (Fake_dom.children (Page.find p (By.label "Messages")))

let status p = Fake_dom.text (Page.find p (By.test_id "status"))

let nick p = Fake_dom.value (Page.find p (By.label "Nickname"))

let with_page ?host f =
  let p = Page.start ?host Chat.app in
  Fun.protect ~finally:(fun () -> Page.stop p) (fun () -> f p)

(* The messages of a socket render at the next frame. *)
let frame p = Page.advance p (Span.ms 16)

(* The open of the socket, and the reply of /api/history. *)
let open_room p history =
  Http_mock.on_get p ~once:true "/api/history" (`Json history) ;
  Websocket_mock.accept (Websocket_mock.server p "/chat") ;
  frame p

let send p text =
  Page.input p (By.label "Message") text ;
  Page.submit p (By.label "Send a message") ;
  Page.run p

let send_receive () =
  with_page (fun p ->
      let room = Websocket_mock.server p "/chat" in
      equal string "Connecting" (status p) ;
      open_room p {|["ann: hi"]|} ;
      equal string "Connected" (status p) ;
      equal (list string) ~msg:"the history" ["ann: hi"] (messages p) ;
      send p "hello" ;
      equal (list string) ~msg:"no nickname" ["anonymous: hello"]
        (Websocket_mock.received room) ;
      Page.input p (By.label "Nickname") "bob" ;
      send p "yo" ;
      send p "" ;
      equal (list string) ~msg:"the nickname, and no empty message"
        ["anonymous: hello"; "bob: yo"]
        (Websocket_mock.received room) ;
      Websocket_mock.push room "ann: welcome" ;
      frame p ;
      equal (list string) ["ann: hi"; "ann: welcome"] (messages p) ;
      for i = 1 to 25 do
        Websocket_mock.push room (string_of_int i)
      done ;
      frame p ;
      equal int ~msg:"the last 20" 20 (List.length (messages p)) ;
      equal string "25" (List.nth (messages p) 19) ;
      Http_mock.expect_done p )

(* The waits after the closes: 1 s, then 2 s with no open between them, then 1 s
   again after an open. [at p t ms] runs the host until [ms] after the time [t].
   The timer starts in the update of the close, at the time of the drop. The
   message of the timer renders at the next frame: then the new key starts. *)
let at p t ms =
  Page.advance p
    (Span.ms (int_of_float (t +. Float.of_int ms -. Host.now (Page.host p))))

let reconnect () =
  with_page (fun p ->
      let room = Websocket_mock.server p "/chat" in
      let drop () =
        Websocket_mock.drop room {code= 1001; reason= ""; clean= true} ;
        Host.now (Page.host p)
      in
      open_room p "[]" ;
      let t = drop () in
      frame p ;
      equal string "Disconnected: a new try in 1 s" (status p) ;
      at p t 999 ;
      equal int ~msg:"999 ms" 0 (Websocket_mock.connections room) ;
      at p t 1016 ;
      equal int ~msg:"1 s and a frame" 1 (Websocket_mock.connections room) ;
      equal string "Connecting" (status p) ;
      (* The server refuses: the second wait is 2 s. *)
      let t = drop () in
      frame p ;
      equal string "Disconnected: a new try in 2 s" (status p) ;
      at p t 1999 ;
      equal int ~msg:"1,999 ms" 0 (Websocket_mock.connections room) ;
      at p t 2016 ;
      equal int ~msg:"2 s and a frame" 1 (Websocket_mock.connections room) ;
      open_room p {|["ann: back"]|} ;
      equal (list string) ~msg:"the history again" ["ann: back"] (messages p) ;
      ignore (drop ()) ;
      frame p ;
      equal string ~msg:"an open sets the wait back"
        "Disconnected: a new try in 1 s" (status p) ;
      equal (list int) ~msg:"the backoff" [1; 2; 4; 8; 16; 30; 30]
        (List.map Chat.backoff [0; 1; 2; 3; 4; 5; 60]) )

let nickname () =
  let host = Host.create () in
  Storage_mock.preset host Local [("nick", "ann")] ;
  with_page ~host (fun p ->
      equal string ~msg:"the restore" "ann" (nick p) ;
      Page.input p (By.label "Nickname") "anna" ;
      equal
        (list (pair string string))
        [("nick", "anna")]
        (Storage_mock.contents p Local) ;
      Storage_mock.block p ;
      Page.input p (By.label "Nickname") "x" ;
      equal string "The nickname is not saved."
        (Fake_dom.text (Page.find p (By.text "The nickname is not saved."))) ;
      equal
        (list (pair string string))
        ~msg:"no write"
        [("nick", "anna")]
        (Storage_mock.contents p Local) ) ;
  with_page (fun p -> equal string ~msg:"no nickname" "" (nick p))

(* A live message before the reply of the history stays after the history. The
   first live message is also the last message of the history: no copy. *)
let early () =
  with_page (fun p ->
      let room = Websocket_mock.server p "/chat" in
      Http_mock.on_get p ~delay:(Span.ms 100) "/api/history"
        (`Json {|["a","b"]|}) ;
      Websocket_mock.accept room ;
      frame p ;
      Websocket_mock.push room "b" ;
      Websocket_mock.push room "c" ;
      frame p ;
      equal (list string) ~msg:"before the reply" ["b"; "c"] (messages p) ;
      Page.advance p (Span.ms 200) ;
      equal (list string) ~msg:"after the reply" ["a"; "b"; "c"] (messages p) ;
      Websocket_mock.push room "d" ;
      frame p ;
      equal (list string) ["a"; "b"; "c"; "d"] (messages p) )

(* The draft stays until the send works. *)
let unsent () =
  with_page (fun p ->
      let room = Websocket_mock.server p "/chat" in
      let draft () = Fake_dom.value (Page.find p (By.label "Message")) in
      Page.input p (By.label "Message") "hi" ;
      Page.submit p (By.label "Send a message") ;
      Page.run p ;
      equal string ~msg:"not open: the draft stays" "hi" (draft ()) ;
      equal string "The message is not sent."
        (Fake_dom.text (Page.find p (By.text "The message is not sent."))) ;
      open_room p "[]" ;
      Page.submit p (By.label "Send a message") ;
      Page.run p ;
      equal string ~msg:"sent: an empty draft" "" (draft ()) ;
      equal (list string) ["anonymous: hi"] (Websocket_mock.received room) ;
      is_false ~msg:"no alert"
        ( match Page.find p (By.text "The message is not sent.") with
        | _ ->
            true
        | exception Page.Query _ ->
            false ) )

let tests =
  [ group "chat"
      [ test "send and receive" send_receive
      ; test "a close, then a new try at exact times" reconnect
      ; test "the nickname in localStorage" nickname
      ; test "a live message before the history" early
      ; test "the draft stays until the send works" unsent ] ]

let () = exit (Windtrap.run "chat" tests)
