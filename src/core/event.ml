let discrete = function
  | "click" | "input" | "change" | "submit" | "keydown" ->
      true
  | _ ->
      false

let bubbles = function
  | "mouseenter"
  | "mouseleave"
  | "scroll"
  | "wheel"
  | "touchstart"
  | "touchmove" ->
      false
  | _ ->
      true

let delegated_events =
  [ "click"
  ; "input"
  ; "change"
  ; "submit"
  ; "keydown"
  ; "keyup"
  ; "focusin"
  ; "focusout"
  ; "compositionstart"
  ; "compositionend" ]

let direct_events =
  ["mouseenter"; "mouseleave"; "scroll"; "wheel"; "touchstart"; "touchmove"]

let walk ~parent ~cell target name f =
  let up = bubbles name in
  let rec go node =
    let stop =
      match Option.bind (cell node) (fun c -> Dom.handler c name) with
      | Some (Dom.Handler h as handler) ->
          f handler ; h.stop
      | None ->
          false
    in
    if up && not stop then match parent node with Some p -> go p | None -> ()
  in
  go target

type 'node env =
  { ops: 'node Dom.t
  ; parent: 'node -> 'node option
  ; cell: 'node -> Dom.cell option
  ; connected: 'node -> bool
  ; group: 'node -> 'node list
  ; toggles: 'node -> bool
  ; post_task: (unit -> unit) -> unit }

type 'node event =
  { name: string
  ; target: 'node
  ; composing: bool
  ; read: 'a. 'a Dom.payload -> 'a
  ; prevent: unit -> unit
  ; stop: unit -> unit }

(* The restore of a target: the target and the other buttons of its radio group,
   from their cells. It does nothing for a target that is not connected, for
   example the target of a [change] that the browser sends inside a removal. *)
let restore env target () =
  if env.connected target then
    List.iter
      (fun n -> Option.iter (Diff.restore env.ops n) (env.cell n))
      (target :: env.group target)

(* The composition flag of the target. *)
let composing env target =
  match env.cell target with Some c -> c.Dom.composing | None -> false

let set_composing env target b =
  Option.iter (fun (c : Dom.cell) -> c.composing <- b) (env.cell target)

let dispatch env rt ev =
  let prevented = ref false in
  (* The payload read comes during the walk, thus before the restore. The filter
     Plain_click skips a click with a button that is not 0 or with a modifier
     key. It also skips a click whose default a listener or a handler below
     prevented. A skipped handler does not prevent the default and gives no
     message, thus the browser does its default action (for example, it opens
     the link in a new tab). Its stop applies: a click handler above does not
     run. *)
  let fire (Dom.Handler h) =
    let skip =
      match h.filter with
      | Dom.Any ->
          false
      | Dom.Plain_click ->
          let m = ev.read Dom.Mouse in
          m.button <> 0 || m.alt || m.ctrl || m.meta || m.shift
          || m.default_prevented || !prevented
    in
    if h.prevent_default && not skip then begin
      ev.prevent () ;
      prevented := true
    end ;
    if h.stop then ev.stop () ;
    if not skip then h.fire (ev.read h.payload)
  in
  let walk () = walk ~parent:env.parent ~cell:env.cell ev.target ev.name fire in
  let discrete_turn () =
    Runtime.event rt Runtime.Discrete ~restore:(restore env ev.target) walk
  in
  (* A task with a discrete turn that only restores the target. *)
  let restore_task () =
    env.post_task (fun () ->
        Runtime.event rt Runtime.Discrete ~restore:(restore env ev.target)
          ignore )
  in
  ( match ev.name with
  | ("keydown" | "keyup") when ev.composing ->
      (* The Enter that commits an IME candidate does not submit. *)
      ()
  | "compositionstart" ->
      set_composing env ev.target true
  | "compositionend" ->
      (* Firefox sends the last input after compositionend, and the live value
         is then the commit. A render here writes the old view value over the
         commit. Thus a Macro turn, which does not render, and a task that
         restores the target. In the order of Firefox, the last input renders
         first. *)
      set_composing env ev.target false ;
      Runtime.event rt Runtime.Macro walk ;
      restore_task ()
  | ("focusin" | "focusout") when composing env ev.target ->
      (* Chrome sends no compositionend when it moves or removes the composing
         element, but it sends focusout. A composition starts after a focus,
         thus a focusin also ends a composition that stays. *)
      set_composing env ev.target false ;
      discrete_turn ()
  | name when not (bubbles name) ->
      (* A removed element keeps its cell and its listener. *)
      if env.connected ev.target then Runtime.event rt Runtime.Macro walk
  | name when discrete name ->
      discrete_turn ()
  | _ ->
      Runtime.event rt Runtime.Macro walk ) ;
  (* The browser undoes a prevented toggle after the dispatch, and a listener
     above the mount element can prevent the default too. A microtask runs
     before the undo, and a new task after it. *)
  if String.equal ev.name "click" && env.toggles ev.target then restore_task () ;
  !prevented
