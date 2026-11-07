module Dom = Kilim.Private.Dom
module Event = Kilim.Private.Event
module Runtime = Kilim.Private.Runtime

type t =
  { host: Host.t
  ; doc: Fake_dom.t
  ; root: Fake_dom.node
  ; rt: Fake_dom.node Runtime.t
  ; mutable depth: int  (** The nested calls. *) }

exception Query of string

(* The microtasks run after a top-level call, not inside a turn. *)
let top t f =
  t.depth <- t.depth + 1 ;
  match f () with
  | () ->
      t.depth <- t.depth - 1 ;
      if t.depth = 0 then Host.microtasks t.host
  | exception e ->
      t.depth <- t.depth - 1 ;
      raise e

let start ?host app =
  let host = match host with Some h -> h | None -> Host.create () in
  let doc = Fake_dom.create () in
  let root = Fake_dom.element "div" in
  let rt = Runtime.start (Host.env host) (Fake_dom.ops doc) ~parent:root app in
  Host.microtasks host ;
  {host; doc; root; rt; depth= 0}

let host t = t.host

let doc t = t.doc

let root t = t.root

let html t = Fake_dom.to_html t.root

let rec collect p n acc =
  let acc =
    List.fold_left (fun acc c -> collect p c acc) acc (Fake_dom.children n)
  in
  if p n then n :: acc else acc

let find t = function
  | By.Node n ->
      n
  | By.Test_id s -> (
      let has n =
        Option.equal String.equal (Fake_dom.attribute n "data-testid") (Some s)
      in
      match collect has t.root [] with
      | [n] ->
          n
      | [] ->
          raise (Query ("no element with data-testid " ^ s))
      | _ ->
          raise (Query ("more than one element with data-testid " ^ s)) )

(* The handlers of one event, from the target up, in one turn. The payload comes
   from the target. *)
let dispatch t by name =
  top t (fun () ->
      let target = find t by in
      let kind =
        if Event.discrete name then Runtime.Discrete else Runtime.Macro
      in
      Runtime.event t.rt kind (fun () ->
          Event.walk ~parent:Fake_dom.parent ~cell:Fake_dom.cell target name
            (fun (Dom.Handler h) ->
              match h.payload with
              | Dom.Unit ->
                  h.fire ()
              | Dom.String | Dom.Key | Dom.Bool ->
                  invalid_arg
                    ("Kilim_testing.Page: no payload for the event " ^ name) ) ) )

let click t by = dispatch t by "click"

let focus t by = dispatch t by "focusin"

let set_hidden t b =
  top t (fun () ->
      Host.set_hidden t.host b ;
      Runtime.visibility_changed t.rt )

let advance t span = Host.advance t.host span

let run t = Host.run t.host

let stats t = Runtime.stats t.rt

let inspect t = Runtime.inspect t.rt

let stop t = top t (fun () -> Runtime.stop t.rt)

let expect_no_tasks t =
  let live = Host.live_timers t.host in
  if live > 0 then failwith (string_of_int live ^ " live timers") ;
  let kids = (inspect t).token_children in
  if kids > 0 then failwith (string_of_int kids ^ " children of the root token")
