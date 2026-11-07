module Dom = Kilim.Private.Dom
module Diff = Kilim.Private.Diff
module Event = Kilim.Private.Event
module Runtime = Kilim.Private.Runtime

type t =
  { host: Host.t
  ; doc: Fake_dom.t
  ; root: Fake_dom.node
  ; rt: Fake_dom.node Runtime.t
  ; env: Fake_dom.node Event.env
  ; mutable undo: (unit -> unit) list
        (** The undo of a prevented click, after the microtasks. *)
  ; tasks: (unit -> unit) Queue.t  (** The tasks of [Event.dispatch]. *)
  ; mutable depth: int  (** The nested calls. *) }

exception Query of string

(* After a top-level call, not inside a turn: the microtasks, the undo of a
   prevented click, then each task with the microtasks after it. This is the
   order of a browser. *)
let top t f =
  t.depth <- t.depth + 1 ;
  match f () with
  | () ->
      t.depth <- t.depth - 1 ;
      if t.depth = 0 then begin
        Host.microtasks t.host ;
        let undo = List.rev t.undo in
        t.undo <- [] ;
        List.iter (fun f -> f ()) undo ;
        while not (Queue.is_empty t.tasks) do
          (Queue.pop t.tasks) () ;
          Host.microtasks t.host
        done
      end
  | exception e ->
      t.depth <- t.depth - 1 ;
      raise e

let rec under root n =
  match Fake_dom.parent n with
  | Some p ->
      p == root || under root p
  | None ->
      false

(* The type of an input element, in lowercase. *)
let input_type n =
  if Fake_dom.ns n = Some Dom.Html && String.equal (Fake_dom.name n) "input"
  then Option.map String.lowercase_ascii (Fake_dom.attribute n "type")
  else None

let toggles n =
  match input_type n with
  | Some ("checkbox" | "radio") ->
      true
  | Some _ | None ->
      false

let start ?host app =
  let host = match host with Some h -> h | None -> Host.create () in
  let doc = Fake_dom.create () in
  let root = Fake_dom.element "div" in
  let ops = Fake_dom.ops doc in
  let tasks = Queue.create () in
  let env =
    { Event.ops
    ; parent= Fake_dom.parent (* The mount element has no parent. *)
    ; cell= Fake_dom.cell
    ; connected= under root
    ; group= Fake_dom.group
    ; toggles
    ; post_task= (fun f -> Queue.push f tasks) }
  in
  let rt = Runtime.start (Host.env host) ops ~parent:root app in
  Host.microtasks host ;
  {host; doc; root; rt; env; undo= []; tasks; depth= 0}

let host t = t.host

let doc t = t.doc

let root t = t.root

let html t = Fake_dom.to_html t.root

(* The nodes below [n] (not [n]) that satisfy [p], in the order of the document.
   Thus a query never gives the mount element. *)
let collect p n =
  let rec go acc n =
    let acc = if p n then n :: acc else acc in
    List.fold_left go acc (Fake_dom.children n)
  in
  List.rev (List.fold_left go [] (Fake_dom.children n))

let is_element n =
  match Fake_dom.name n with "#text" | "#html" -> false | _ -> true

let space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'

(* Each run of white space becomes one space, and the white space at the start
   and at the end goes. *)
let collapse s =
  String.split_on_char ' ' (String.map (fun c -> if space c then ' ' else c) s)
  |> List.filter (fun w -> not (String.equal w ""))
  |> String.concat " "

let has_text s n = is_element n && String.equal (collapse (Fake_dom.text n)) s

let named names n = is_element n && List.mem (Fake_dom.name n) names

(* The first node in the order of the document. *)
let first p n = match collect p n with x :: _ -> Some x | [] -> None

(* The control of a label: the element of its [for], else the first control
   inside it. *)
let control t l =
  match Fake_dom.attribute l "for" with
  | Some id ->
      first
        (fun n ->
          is_element n
          && Option.equal String.equal (Fake_dom.attribute n "id") (Some id) )
        t.root
  | None ->
      first (named ["input"; "select"; "textarea"; "button"]) l

let one what s = function
  | [n] ->
      n
  | [] ->
      raise (Query ("no element with " ^ what ^ " " ^ s))
  | _ ->
      raise (Query ("more than one element with " ^ what ^ " " ^ s))

let find t = function
  | By.Node n ->
      n
  | By.Test_id s ->
      let has n =
        Option.equal String.equal (Fake_dom.attribute n "data-testid") (Some s)
      in
      one "data-testid" s (collect has t.root)
  | By.Text s ->
      let s = collapse s in
      let deepest n =
        has_text s n && not (List.exists (has_text s) (Fake_dom.children n))
      in
      one "the text" s (collect deepest t.root)
  | By.Label s ->
      let s = collapse s in
      let labels =
        collect (fun n -> named ["label"] n && has_text s n) t.root
      in
      let aria =
        collect
          (fun n ->
            match Fake_dom.attribute n "aria-label" with
            | Some a ->
                String.equal (collapse a) s
            | None ->
                false )
          t.root
      in
      let controls = List.filter_map (control t) labels in
      let all =
        List.fold_left
          (fun acc n -> if List.memq n acc then acc else n :: acc)
          [] (controls @ aria)
      in
      one "the label" s all

(* One event on [target]. The payload comes from the target: its live value, its
   live checked, or [key]. The result is [true] if a handler prevents the
   default. *)
let send t ?(key = "") ?(composing = false) target name =
  let read : type a. a Dom.payload -> a = function
    | Dom.Unit ->
        ()
    | Dom.String ->
        Fake_dom.value target
    | Dom.Key ->
        key
    | Dom.Bool ->
        Fake_dom.checked target
  in
  Event.dispatch t.env t.rt
    {name; target; composing; read; prevent= ignore; stop= ignore}

let dispatch t by name = top t (fun () -> ignore (send t (find t by) name))

(* As a browser does, a click changes [checked] before the handlers: a checkbox
   toggles, and a radio button becomes checked, which unchecks its group. If a
   handler prevents the default, the browser undoes the change after the
   dispatch and its microtasks: the checkbox toggles back. The radio button that
   was checked before becomes checked again, if it is still in the group of the
   target. If not, the target stays checked (Chrome). *)
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
              begin match before with
              | Some n when not (List.memq n (Fake_dom.group target)) ->
                  ()
              | Some n ->
                  (* n is in the group of the target: thus this unchecks the
                     target. *)
                  Fake_dom.browser_set n (Checked true)
              | None ->
                  Fake_dom.browser_set target (Checked was)
              end
        | Some _ | None ->
            ignore
      in
      if send t target "click" then t.undo <- undo :: t.undo )

(* The browser writes the value, then sends the event. *)
let set_then t by v name =
  top t (fun () ->
      let target = find t by in
      Fake_dom.browser_set target (Value v) ;
      ignore (send t target name) )

let type_ t by s =
  String.iter
    (fun c ->
      top t (fun () ->
          let target = find t by in
          Fake_dom.browser_set target
            (Value (Fake_dom.value target ^ String.make 1 c)) ;
          ignore (send t target "input") ) )
    s

let input t by v = set_then t by v "input"

let change t by v = set_then t by v "change"

let submit t by = dispatch t by "submit"

let keydown t ?composing by key =
  top t (fun () -> ignore (send t ~key ?composing (find t by) "keydown"))

let keyup t ?composing by key =
  top t (fun () -> ignore (send t ~key ?composing (find t by) "keyup"))

let focus t by = dispatch t by "focusin"

let blur t by = dispatch t by "focusout"

let event t by name = dispatch t by name

(* The order of Chrome: compositionstart, then for each text an input event with
   isComposing (the last text is the commit), then compositionend. One task for
   each event. *)
let compose t by texts =
  let target = find t by in
  let start = Fake_dom.value target in
  dispatch t (By.node target) "compositionstart" ;
  List.iter
    (fun s ->
      top t (fun () ->
          Fake_dom.browser_set target (Value (start ^ s)) ;
          ignore (send t ~composing:true target "input") ) )
    texts ;
  dispatch t (By.node target) "compositionend"

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
