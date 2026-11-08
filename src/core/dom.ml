type ns = Html | Svg

type prop = Value of string | Checked of bool

type mouse =
  { button: int
  ; alt: bool
  ; ctrl: bool
  ; meta: bool
  ; shift: bool
  ; default_prevented: bool }

type filter = Any | Plain_click

type _ payload =
  | Unit : unit payload
  | String : string payload
  | Key : string payload
  | Bool : bool payload
  | Mouse : mouse payload

type 'msg decode = Decode : 'a payload * ('a -> 'msg) -> 'msg decode

type 'msg event =
  { event: string
  ; bubbles: bool
  ; prevent_default: bool
  ; stop: bool
  ; filter: filter
  ; decode: 'msg decode }

type handler =
  | Handler :
      { payload: 'a payload
      ; prevent_default: bool
      ; stop: bool
      ; filter: filter
      ; fire: 'a -> unit }
      -> handler

type 'msg post = {post: 'a. 'a -> ('a -> 'msg) -> unit} [@@unboxed]

let post_now k = {post= (fun x f -> k (f x))}

type events = Events : 'msg event list * 'msg post -> events

type cell =
  { mutable events: events
  ; mutable value: string option
  ; mutable checked: bool option
  ; mutable composing: bool }

let bind post (h : _ event) =
  match h.decode with
  | Decode (payload, f) ->
      Handler
        { payload
        ; prevent_default= h.prevent_default
        ; stop= h.stop
        ; filter= h.filter
        ; fire= (fun x -> post.post x f) }

let handler cell name =
  match cell.events with
  | Events (evs, post) ->
      List.find_map
        (fun (h : _ event) ->
          if String.equal h.event name then Some (bind post h) else None )
        evs

let handlers cell =
  match cell.events with
  | Events (evs, post) ->
      List.map (fun (h : _ event) -> (h.event, bind post h)) evs

type 'node t =
  { create_element: ns -> string -> 'node
  ; create_text: string -> 'node
  ; set_text: 'node -> string -> unit
  ; set_attribute: 'node -> string -> string -> unit
  ; remove_attribute: 'node -> string -> unit
  ; set_property: 'node -> prop -> unit
  ; get_value: 'node -> string
  ; get_checked: 'node -> bool
  ; focused: 'node -> bool
  ; insert_before: 'node -> 'node -> 'node option -> unit
  ; remove: 'node -> 'node -> unit
  ; replace: 'node -> 'node -> 'node -> unit
  ; clear: 'node -> unit
  ; set_inner_html: 'node -> Unsafe_html.t -> unit
  ; attach: 'node -> cell -> unit
  ; listen: 'node -> string -> passive:bool -> unit
  ; unlisten: 'node -> string -> unit }
