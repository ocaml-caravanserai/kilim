module Dom = Kilim.Private.Dom
module Diff = Kilim.Private.Diff
module Event = Kilim.Private.Event
module Runtime = Kilim.Private.Runtime

type t =
  { host: Host.t
  ; doc: Fake_dom.t
  ; ops: Fake_dom.node Dom.t
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
  let ops = Fake_dom.ops doc in
  let rt = Runtime.start (Host.env host) ops ~parent:root app in
  Host.microtasks host ;
  {host; doc; ops; root; rt; depth= 0}

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

let rec under root n =
  match Fake_dom.parent n with
  | Some p ->
      p == root || under root p
  | None ->
      false

(* The restore of a discrete event: the target and the other buttons of its
   radio group, from their cells. It does nothing for a target that is not under
   the mount element. *)
let restore t target () =
  if under t.root target then
    List.iter
      (fun n -> Option.iter (Diff.restore t.ops n) (Fake_dom.cell n))
      (target :: Fake_dom.group target)

(* The type of an input element, in lowercase. *)
let input_type n =
  if Fake_dom.ns n = Some Dom.Html && String.equal (Fake_dom.name n) "input"
  then Option.map String.lowercase_ascii (Fake_dom.attribute n "type")
  else None

(* The handlers of one event, from the target up, in one turn. The payload comes
   from the target: its live value, its live checked, or [key]. A discrete event
   gives its restore. The result is [true] if a handler prevents the default. *)
let event t ?(key = "") target name =
  let prevented = ref false in
  let fire (Dom.Handler h) =
    if h.prevent_default then prevented := true ;
    match h.payload with
    | Dom.Unit ->
        h.fire ()
    | Dom.String ->
        h.fire (Fake_dom.value target)
    | Dom.Key ->
        h.fire key
    | Dom.Bool ->
        h.fire (Fake_dom.checked target)
  in
  let walk () =
    Event.walk ~parent:Fake_dom.parent ~cell:Fake_dom.cell target name fire
  in
  if Event.discrete name then
    Runtime.event t.rt Runtime.Discrete ~restore:(restore t target) walk
  else Runtime.event t.rt Runtime.Macro walk ;
  !prevented

let dispatch t by name = top t (fun () -> ignore (event t (find t by) name))

(* As a browser does, a click changes [checked] before the handlers: a checkbox
   toggles, and a radio button becomes checked, which unchecks its group. If a
   handler prevents the default, the browser undoes the change after the
   dispatch: the checkbox toggles back, and the radio button that was checked
   before becomes checked again. *)
let click t by =
  top t (fun () ->
      let target = find t by in
      let was = Fake_dom.checked target in
      let undo =
        match input_type target with
        | Some "checkbox" ->
            Fake_dom.browser_set target (Checked (not was)) ;
            fun () -> Fake_dom.browser_set target (Checked was)
        | Some "radio" ->
            let before =
              List.find_opt Fake_dom.checked (Fake_dom.group target)
            in
            Fake_dom.browser_set target (Checked true) ;
            fun () ->
              Fake_dom.browser_set target (Checked was) ;
              Option.iter
                (fun n -> Fake_dom.browser_set n (Checked true))
                before
        | Some _ | None ->
            ignore
      in
      if event t target "click" then undo () )

(* The browser writes the value, then sends the event. *)
let set_then t by v name =
  top t (fun () ->
      let target = find t by in
      Fake_dom.browser_set target (Value v) ;
      ignore (event t target name) )

let type_ t by s =
  String.iter
    (fun c ->
      top t (fun () ->
          let target = find t by in
          Fake_dom.browser_set target
            (Value (Fake_dom.value target ^ String.make 1 c)) ;
          ignore (event t target "input") ) )
    s

let input t by v = set_then t by v "input"

let change t by v = set_then t by v "change"

let submit t by = dispatch t by "submit"

let keydown t by key =
  top t (fun () -> ignore (event t ~key (find t by) "keydown"))

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
