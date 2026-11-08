(* The components example. One app uses the components of Phase 2 together: two
   counters with an output and a reset by key, a filter box with a debounce, a
   theme context, and a boundary around a panel that can raise. *)

open Kilim

type theme = Light | Dark

let theme : theme Context.t = Context.create ~default:Light ()

let theme_class = function Light -> "light" | Dark -> "dark"

(* A counter. Each change gives the new count to the parent. The view reads the
   theme. *)
module Counter = struct
  include Component.Defaults

  type props = {label: string; step: int}

  type model = int

  type msg = Incr | Decr

  type out = Changed of int

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init _ = (0, Cmd.none)

  let update ctx p n msg =
    let n = match msg with Incr -> n + p.step | Decr -> n - p.step in
    (n, Ctx.emit ctx (Changed n))

  let view p n =
    Context.read theme (fun t ->
        El.div
          ~at:[At.classes ["counter"; theme_class t]]
          [ El.span [txt p.label]
          ; El.button
              ~at:[At.aria_label ("Decrement " ^ p.label)]
              ~ev:[Ev.click Decr]
              [txt "-"]
          ; El.output
              ~at:[At.aria_label (p.label ^ " count")]
              [txt (string_of_int n)]
          ; El.button
              ~at:[At.aria_label ("Increment " ^ p.label)]
              ~ev:[Ev.click Incr]
              [txt "+"] ] )
end

(* A filter box with a debounce. Each input starts a new sleep with the same
   key, and the new sleep cancels the old one. Thus the parent gets the query
   only after [delay] with no input. *)
module Filter = struct
  include Component.Defaults

  type props = unit

  type model = string

  type msg = Edit of string | Settle of string

  type out = Query of string

  let id : (props, model, msg, out) Component.id = Component.id ()

  let delay = Span.ms 300

  let init () = ("", Cmd.none)

  let update ctx () draft = function
    | Edit s ->
        ( s
        , Cmd.latest ~key:"debounce"
            (Async.map (fun () -> Settle s) (Async.sleep delay)) )
    | Settle s ->
        (draft, Ctx.emit ctx (Query s))

  let view () draft =
    El.input
      ~at:[At.aria_label "Filter"; At.value draft]
      ~ev:[Ev.input (fun s -> Edit s)]
      ()
end

(* A panel with a button that raises in [update]. *)
module Panel = struct
  include Component.Defaults

  type props = unit

  type model = int

  type msg = Click | Break

  type out = |

  exception Broken

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init () = (0, Cmd.none)

  let update _ () n = function
    | Click ->
        (n + 1, Cmd.none)
    | Break ->
        raise Broken

  let view () n =
    El.div
      [ El.button ~ev:[Ev.click Click] [txt ("Clicks: " ^ string_of_int n)]
      ; El.button ~ev:[Ev.click Break] [txt "Break"] ]
end

let fruits =
  ["Apple"; "Apricot"; "Banana"; "Cherry"; "Grape"; "Mango"; "Orange"; "Pear"]

(* [contains s sub] is true if [sub] is a part of [s]. *)
let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

type model =
  {left: int; right: int; resets: int; query: string; dark: bool; tries: int}

type msg =
  | Left of int
  | Right of int
  | Reset
  | Query of string
  | Toggle_theme
  | Retry

let init () =
  ({left= 0; right= 0; resets= 0; query= ""; dark= false; tries= 0}, Cmd.none)

let update m = function
  | Left n ->
      ({m with left= n}, Cmd.none)
  | Right n ->
      ({m with right= n}, Cmd.none)
  | Reset ->
      ({m with left= 0; right= 0; resets= m.resets + 1}, Cmd.none)
  | Query q ->
      ({m with query= q}, Cmd.none)
  | Toggle_theme ->
      ({m with dark= not m.dark}, Cmd.none)
  | Retry ->
      ({m with tries= m.tries + 1}, Cmd.none)

(* A new key gives a new instance: thus Reset sets the two counters to 0. *)
let counters m =
  let key side = side ^ "/" ^ string_of_int m.resets in
  El.section
    [ Component.v
        (module Counter)
        ~key:(key "left")
        ~on:(fun (Counter.Changed n) -> Left n)
        {Counter.label= "Ones"; step= 1}
    ; Component.v
        (module Counter)
        ~key:(key "right")
        ~on:(fun (Counter.Changed n) -> Right n)
        {Counter.label= "Tens"; step= 10}
    ; El.p
        [ txt "Total: "
        ; El.output
            ~at:[At.aria_label "Total"]
            [txt (string_of_int (m.left + m.right))] ]
    ; El.button ~ev:[Ev.click Reset] [txt "Reset"] ]

let search m =
  let q = String.lowercase_ascii m.query in
  let found =
    List.filter (fun f -> contains (String.lowercase_ascii f) q) fruits
  in
  El.section
    [ Component.v (module Filter) ~on:(fun (Filter.Query q) -> Query q) ()
    ; El.ul
        ~at:[At.aria_label "Fruits"]
        (List.map (fun f -> El.li [txt f]) found) ]

(* The boundary catches the error of the panel and shows the fallback. The rest
   of the app works. A new key tries the panel again. *)
let guarded m =
  View.boundary ~key:(string_of_int m.tries)
    ~fallback:(fun _ ->
      El.p
        ~at:[At.role "alert"]
        [txt "The panel failed."; El.button ~ev:[Ev.click Retry] [txt "Retry"]] )
    (Component.v (module Panel) ~on:(function _ -> .) ())

let view m =
  let t = if m.dark then Dark else Light in
  Context.provide theme t
    (El.main
       ~at:[At.class_ (theme_class t)]
       [ El.button
           ~ev:[Ev.click Toggle_theme]
           [txt (if m.dark then "Light theme" else "Dark theme")]
       ; counters m
       ; search m
       ; guarded m ] )

let app = App.make ~init ~update ~view ()
