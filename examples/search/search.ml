(* The search example: the search box of design section 09, with the query in
   the URL. Each key press starts a new Cmd.latest task: a debounce, then a
   request. After each result, the box puts the query in the URL with
   Router.replace. At the start, the box reads the query of the URL: thus a deep
   link (/?q=par) loads its results. A link of the app pushes a query, and the
   back button gives the query before it: the box gets each change of the
   URL. *)

open Kilim
open Async.Syntax
module Http = Kilim_http
module Router = Kilim_router

(* The route is the query of the URL: "/?q=par" is "par", and "/" is "". *)
let router =
  Router.make
    ~parse:(fun (l : Url.location) ->
      Option.value ~default:"" (List.assoc_opt "q" l.query) )
    ~print:(fun q ->
      {Url.path= []; query= (if q = "" then [] else [("q", q)]); fragment= None} )
    ()

module Search = struct
  include Component.Defaults

  type props = {endpoint: string}

  (* [url_query]: the query of the last URL that the box wrote or read.
     [failed]: the last request failed. *)
  type model =
    { query: string
    ; results: string list
    ; loading: bool
    ; failed: bool
    ; url_query: string }

  type msg =
    | Typed of string
    | Got of string * (string list, Http.error) result
    | Chose of string
    | Url of string
    | Replaced

  type out = Picked of string

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init _ =
    ( {query= ""; results= []; loading= false; failed= false; url_query= ""}
    , Cmd.task (Async.map (fun q -> Url q) (Router.current router)) )

  (* The debounce and the request. A new key press cancels the old task: its
     timer stops, or the browser aborts its request. *)
  let fetch p q =
    Cmd.latest ~key:"fetch"
      (let* () = Async.sleep (Span.ms 250) in
       let+ r =
         Http.get_json
           Http.Decode.(list string)
           (Url.with_query p.endpoint [("q", q)])
       in
       Got (q, r) )

  let update ctx p m = function
    | Typed q ->
        ({m with query= q; loading= true}, fetch p q)
    | Got (q, Ok rs) ->
        ( {m with results= rs; loading= false; failed= false; url_query= q}
        , Cmd.task (Async.map (fun () -> Replaced) (Router.replace router q)) )
    | Got (_, Error _) ->
        ({m with results= []; loading= false; failed= true}, Cmd.none)
    | Url q when q = m.url_query ->
        (m, Cmd.none)
    | Url q ->
        ({m with query= q; loading= true; url_query= q}, fetch p q)
    | Replaced ->
        (m, Cmd.none)
    | Chose s ->
        (m, Ctx.emit ctx (Picked s))

  let subs _ _ = Router.changes router (fun q -> Url q)

  let view _ m =
    El.div
      [ El.input
          ~at:[At.value m.query; At.aria_label "City"]
          ~ev:[Ev.input (fun s -> Typed s)]
          ()
      ; El.ul
          ~at:
            [At.aria_label "Cities"; At.v "aria-busy" (string_of_bool m.loading)]
          (List.map (fun r -> El.li ~ev:[Ev.click (Chose r)] [txt r]) m.results)
      ; ( if m.failed then El.p ~at:[At.role "alert"] [txt "The search failed."]
          else txt "" ) ]
end

type msg = Pick of string | Go of string | Pushed

(* A link with a query: a plain click pushes its URL. The search box gets the
   change, and the back button gives the query before it. *)
let app =
  App.make
    ~init:(fun () -> ("", Cmd.none))
    ~update:(fun city -> function
      | Pick c ->
          (c, Cmd.none)
      | Go q ->
          (city, Cmd.task (Async.map (fun () -> Pushed) (Router.push router q)))
      | Pushed ->
          (city, Cmd.none) )
    ~view:(fun city ->
      El.main
        [ El.h1 [txt "Cities"]
        ; El.nav
            [ Router.link router ~on:(fun q -> Go q) "lon" [txt "London"]
            ; Router.link router ~on:(fun q -> Go q) "m" [txt "Cities with M"]
            ]
        ; Component.v
            (module Search)
            ~on:(fun (Search.Picked c) -> Pick c)
            {Search.endpoint= "/api/cities"}
        ; El.p
            ~at:[At.data "testid" "picked"]
            [txt (if city = "" then "No city" else "Picked: " ^ city)] ] )
    ()
