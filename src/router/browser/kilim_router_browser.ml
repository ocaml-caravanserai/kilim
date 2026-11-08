module History = Brr.Window.History

let string_js = Jstr.v "string"

(* The trap rule: a read of a JS string examines its type first. *)
let str v = if Jstr.equal (Jv.typeof v) string_js then Jv.to_string v else ""

(* The globals are read at each call: node has no [location] and no [history]
   when the module starts (test/weak). *)
let location_obj () = Jv.get Jv.global "location"

let history_obj () = History.of_jv (Jv.get Jv.global "history")

let location () =
  let l = location_obj () in
  str (Jv.get l "pathname") ^ str (Jv.get l "search") ^ str (Jv.get l "hash")

let warn what e =
  ignore
    (Jv.call
       (Jv.get Jv.global "console")
       "warn"
       [|Jv.of_string ("kilim.router: the browser refused the " ^ what); e|] )

(* The URL of [url] against the URL of the document. [Brr.Uri.v] throws a
   TypeError for a URL that is not valid, and [pushState] a SecurityError for a
   URL of another origin. *)
let uri url =
  Brr.Uri.v ~base:(Jstr.v (str (Jv.get (location_obj ()) "href"))) (Jstr.v url)

let push url =
  match History.push_state ~uri:(uri url) (history_obj ()) with
  | () ->
      Kilim_web.Scroll.to_ ~left:0. ~top:0. ()
  | exception Jv.Error e ->
      warn ("push of " ^ url) (Jv.repr e)

let replace url =
  match History.replace_state ~uri:(uri url) (history_obj ()) with
  | () ->
      ()
  | exception Jv.Error e ->
      warn ("replace of " ^ url) (Jv.repr e)

let back () = History.back (history_obj ())

(* A top-level function: the JS callback keeps only [f]. *)
let listen f =
  let target = Brr.Ev.target_of_jv Jv.global in
  let l = Brr.Ev.listen History.Ev.popstate (fun _ -> f ()) target in
  fun () -> Brr.Ev.unlisten l

let driver = {Kilim_router.location; push; replace; back; listen}

let install () = Kilim_router.Private.set_history (Some driver)
