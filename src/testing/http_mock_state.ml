(* The state of Http_mock (design section 14). Page.make starts the mock of its
   host before the first turn, thus a request of an init task goes to the mock,
   and keeps the state in its record. Page.stop stops it. One mock is live: the
   mock of the last host that started a page, as the error handler of
   Host.create. The state of a mock that is not live stays readable. *)

module H = Kilim_http

type reply =
  [ `Json of string
  | `Text of string
  | `Status of int * string
  | `Network of string
  | `Never
  | `Response of H.response ]

type route = {meth: H.meth; url: string; reply: reply; delay: int; once: bool}

type t =
  { host: Host.t
  ; mutable pages: int  (** The live pages of the host. *)
  ; mutable routes: route list  (** The oldest first. *)
  ; mutable requests: H.request list  (** The newest first. *)
  ; mutable unmatched: H.request list  (** The newest first. *)
  ; mutable pending: int
  ; mutable aborted: int
  ; transport: H.transport }

let live : t option ref = ref None

let page_url = "http://localhost/"

(* A scheme: a letter, then letters, digits, "+", "-" or ".", then ":". *)
let has_scheme u =
  let rec go i =
    i < String.length u
    &&
    match u.[i] with
    | ':' ->
        i > 0
    | 'a' .. 'z' | 'A' .. 'Z' ->
        go (i + 1)
    | '0' .. '9' | '+' | '-' | '.' ->
        i > 0 && go (i + 1)
    | _ ->
        false
  in
  go 0

(* The URL against the page URL http://localhost/. It does not remove dot
   segments. *)
let resolve u =
  if has_scheme u then u
  else if String.starts_with ~prefix:"//" u then "http:" ^ u
  else if String.starts_with ~prefix:"/" u then "http://localhost" ^ u
  else page_url ^ u

(* The result of a reply. [`Never] gives none. *)
let response url : reply -> (H.response, string) result option = function
  | `Json body ->
      Some
        (Ok
           { status= 200
           ; url
           ; headers= [("content-type", "application/json")]
           ; body } )
  | `Text body ->
      Some
        (Ok {status= 200; url; headers= [("content-type", "text/plain")]; body})
  | `Status (status, body) ->
      Some (Ok {status; url; headers= []; body})
  | `Network m ->
      Some (Error m)
  | `Response r ->
      Some (Ok r)
  | `Never ->
      None

(* The oldest route of the method and the URL. A route with ~once goes. *)
let take m meth url =
  let fits r = r.meth = meth && String.equal (resolve r.url) url in
  match List.find_opt fits m.routes with
  | Some r ->
      if r.once then m.routes <- List.filter (fun x -> x != r) m.routes ;
      Some r
  | None ->
      None

let send m (req : H.request) =
  Kilim.Async.make (fun tok ~ok ~error:_ ->
      m.requests <- req :: m.requests ;
      m.pending <- m.pending + 1 ;
      let in_flight = ref true in
      let finish () =
        in_flight := false ;
        m.pending <- m.pending - 1
      in
      (* The step ends normally only after a reply: thus the end action of a
         request in flight is a cancel. *)
      Kilim.Cancel.on_end tok (fun () ->
          if !in_flight then begin
            finish () ;
            m.aborted <- m.aborted + 1
          end ) ;
      let reply_after ms r =
        Host.raw m.host ~after:ms (fun () ->
            if !in_flight then begin
              finish () ; ok r
            end )
      in
      let url = resolve req.url in
      match take m req.meth url with
      | None ->
          m.unmatched <- req :: m.unmatched ;
          reply_after 0
            (Error
               ( "Http_mock: no reply for " ^ H.string_of_meth req.meth ^ " "
               ^ req.url ) )
      | Some r ->
          Option.iter (reply_after r.delay) (response url r.reply) )

let start host =
  let m =
    match !live with
    | Some m when m.host == host ->
        m
    | Some _ | None ->
        let rec m =
          { host
          ; pages= 0
          ; routes= []
          ; requests= []
          ; unmatched= []
          ; pending= 0
          ; aborted= 0
          ; transport= {H.send= (fun r -> send m r)} }
        in
        live := Some m ;
        m
  in
  m.pages <- m.pages + 1 ;
  H.Private.set_transport m.transport ;
  m

let stop m =
  match !live with
  | Some l when l == m ->
      m.pages <- m.pages - 1 ;
      if m.pages = 0 then begin
        live := None ;
        (* Not the transport of another library, for example kilim.http.browser
           in a test of the browser. *)
        match H.Private.transport () with
        | Some t when t == m.transport ->
            H.Private.clear ()
        | Some _ | None ->
            ()
      end
  | Some _ | None ->
      ()

(* A reply goes only to the live mock. *)
let check_live m =
  match !live with
  | Some l when l == m ->
      ()
  | Some _ | None ->
      invalid_arg
        "Kilim_testing.Http_mock: the page is stopped, or a page of another \
         host started after it"
