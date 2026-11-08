module Url = Kilim.Url
module Dlist = Kilim.Private.Dlist

type history =
  { location: unit -> string
  ; push: string -> unit
  ; replace: string -> unit
  ; back: unit -> unit
  ; listen: (unit -> unit) -> unit -> unit }

type 'r t = {base: string; parse: Url.location -> 'r; print: 'r -> Url.location}

(* The one driver of the program, as the error handler of the core. *)
let driver : history option ref = ref None

let history () =
  match !driver with
  | Some h ->
      h
  | None ->
      failwith
        "kilim.router: no history: call Kilim_router_browser.install () in \
         main.ml, or use History_mock in tests"

(* The check of each live subscription of [changes]: a push and a replace call
   them. *)
let checks : (unit -> unit) Dlist.t = Dlist.create ()

let base_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' | '/' ->
      true
  | _ ->
      false

(* [b] is not [""] or ["/"]. *)
let valid_base b =
  b.[0] = '/'
  && String.for_all base_char b
  && b.[String.length b - 1] <> '/'
  && not (String.starts_with ~prefix:"//" b)

let make ?(base = "") ~parse ~print () =
  let base = if String.equal base "/" then "" else base in
  if not (String.equal base "" || valid_base base) then
    invalid_arg ("Kilim_router.make: the base " ^ base) ;
  {base; parse; print}

let href r route = r.base ^ Url.to_string (r.print route)

(* The part of [url] below the base, or all of [url] if it is not below the
   base. *)
let below base url =
  let n = String.length base in
  if
    String.length url >= n
    && String.equal (String.sub url 0 n) base
    && (String.length url = n || String.contains "/?#" url.[n])
  then String.sub url n (String.length url - n)
  else url

let route r url = r.parse (Url.parse (below r.base url))

(* The changes of the URL of each push and replace that changed the URL. A
   subscription starts at a render, thus after the init tasks and after a push
   in the same turn. *)
let count = ref 0

(* The mark: the count and the URL of the last route that the program got, from
   [current] or from a message of [changes]. *)
let seen_count = ref 0

let seen_url : string option ref = ref None

let mark url =
  seen_count := !count ;
  seen_url := Some url

let current r =
  Kilim.Async.make (fun _ ~ok ~error:_ ->
      let url = (history ()).location () in
      mark url ;
      ok (route r url) )

(* A push or a replace with [f], then the check of each live subscription. *)
let change f url =
  Kilim.Async.make (fun _ ~ok ~error:_ ->
      let h = history () in
      let before = h.location () in
      f h url ;
      if not (String.equal (h.location ()) before) then incr count ;
      Dlist.iter (fun check -> check ()) checks ;
      ok () )

let push r x = change (fun h -> h.push) (href r x)

let replace r x = change (fun h -> h.replace) (href r x)

let back () =
  Kilim.Async.make (fun _ ~ok ~error:_ -> (history ()).back () ; ok ())

let kind : string Kilim.Private.Sub.kind = Kilim.Private.Sub.kind ()

(* The task of a subscription never ends by itself: the cancel of its key
   removes the listener and the check. A top-level function, thus the closures
   keep only [deliver] and the driver. *)
let start deliver =
  Kilim.Async.make (fun tok ~ok:_ ~error:_ ->
      let h = history () in
      let url = h.location () in
      let last = ref url in
      let check () =
        let url = h.location () in
        if not (String.equal url !last) then begin
          last := url ;
          mark url ;
          deliver url
        end
      in
      (* The URL changed after the mark: the app does not know the route of the
         URL. *)
      let moved =
        match !seen_url with
        | Some u ->
            not (String.equal u url)
        | None ->
            false
      in
      if !count <> !seen_count || moved then begin
        mark url ; deliver url
      end ;
      let unlisten = h.listen check in
      let node = Dlist.push checks check in
      Kilim.Cancel.on_end tok (fun () -> Dlist.remove node ; unlisten ()) )

let changes r f =
  Kilim.Private.Sub.auto kind ~params:r.base ~start (fun url -> f (route r url))

let link r ?(at = []) ~on x children =
  Kilim.El.a
    ~at:(Kilim.At.href (href r x) :: at)
    ~ev:[Kilim.Ev.link_click (on x)]
    children

module Private = struct
  (* A new driver has a new history. *)
  let set_history h =
    driver := h ;
    count := 0 ;
    seen_count := 0 ;
    seen_url := None

  let history () = !driver

  let listeners () = Dlist.length checks
end
