module Dom = Kilim.Private.Dom
module Names = Kilim.Private.Names
module Smap = Map.Make (String)

type kind = Element of Dom.ns * string | Text | Html

type node =
  { kind: kind
  ; mutable text: string
  ; mutable parent: node option
  ; mutable first: node option
  ; mutable last: node option
  ; mutable prev: node option
  ; mutable next: node option
  ; mutable attrs: (string * string) list
  ; mutable value: string option  (** [None] until a write. *)
  ; mutable checked: bool
  ; mutable selected: bool  (** For an option. *)
  ; mutable cell: Dom.cell option
  ; mutable listeners: (string * bool) list }

type t =
  { mutable total: int
  ; mutable counts: int Smap.t
  ; mutable moves: int
  ; mutable fail_at: int (* 0: no fault *) }

exception Misuse of string

exception Fault

let create () = {total= 0; counts= Smap.empty; moves= 0; fail_at= 0}

let make kind text =
  { kind
  ; text
  ; parent= None
  ; first= None
  ; last= None
  ; prev= None
  ; next= None
  ; attrs= []
  ; value= None
  ; checked= false
  ; selected= false
  ; cell= None
  ; listeners= [] }

let element name = make (Element (Html, name)) ""

let misuse op what = raise (Misuse (op ^ ": " ^ what))

(* The fault, then the count. Call it after the checks of the operation. *)
let step t op =
  let n = t.total + 1 in
  if n = t.fail_at then begin
    t.fail_at <- 0 ;
    raise Fault
  end ;
  t.total <- n ;
  t.counts <-
    Smap.update op (function None -> Some 1 | Some c -> Some (c + 1)) t.counts

let total t = t.total

let counts t = Smap.bindings t.counts

let moves t = t.moves

let reset t =
  t.total <- 0 ;
  t.counts <- Smap.empty ;
  t.moves <- 0

let fail_at t k = t.fail_at <- t.total + k

let is_element n = match n.kind with Element _ -> true | Text | Html -> false

let need_element op n = if not (is_element n) then misuse op "not an element"

let is_child parent n =
  match n.parent with Some p -> p == parent | None -> false

let unlink n =
  ( match n.parent with
  | None ->
      ()
  | Some p -> (
      ( match n.prev with
      | Some q ->
          q.next <- n.next
      | None ->
          p.first <- n.next ) ;
      match n.next with Some q -> q.prev <- n.prev | None -> p.last <- n.prev )
  ) ;
  n.parent <- None ;
  n.prev <- None ;
  n.next <- None

(* Link the node that has no parent before [r], or at the end. *)
let link parent n r =
  n.parent <- Some parent ;
  match r with
  | None ->
      n.prev <- parent.last ;
      ( match parent.last with
      | Some l ->
          l.next <- Some n
      | None ->
          parent.first <- Some n ) ;
      parent.last <- Some n
  | Some r ->
      n.next <- Some r ;
      n.prev <- r.prev ;
      ( match r.prev with
      | Some q ->
          q.next <- Some n
      | None ->
          parent.first <- Some n ) ;
      r.prev <- Some n

let rec is_ancestor a n =
  a == n || match n.parent with Some p -> is_ancestor a p | None -> false

let rec children_of acc = function
  | Some c ->
      children_of (c :: acc) c.next
  | None ->
      List.rev acc

let children n = children_of [] n.first

let is_named name n =
  match n.kind with Element (Html, k) -> String.equal k name | _ -> false

let attribute n k = List.assoc_opt k n.attrs

(* [is_input ty n] is [true] for an input of the type [ty]. As in a browser, the
   case of the type is not important. [ty] is in lowercase. *)
let is_input ty n =
  is_named "input" n
  &&
  match attribute n "type" with
  | Some t ->
      String.equal (String.lowercase_ascii t) ty
  | None ->
      false

let rec text n =
  match n.kind with
  | Text | Html ->
      n.text
  | Element _ ->
      String.concat ""
        (List.map text (List.filter (fun c -> c.kind <> Html) (children n)))

let rec iter_tree f n =
  f n ;
  let rec go = function Some c -> iter_tree f c ; go c.next | None -> () in
  go n.first

(* The model of a select: its options are its option children and the option
   children of its optgroup children. Each option keeps its selectedness when it
   moves. The value of a select is the value of its first selected option, or
   [""]. *)

let options select =
  List.concat_map
    (fun c ->
      if is_named "optgroup" c then List.filter (is_named "option") (children c)
      else if is_named "option" c then [c]
      else [] )
    (children select)

(* A drop-down select: no [multiple] attribute, and no [size] above 1. *)
let drop_down select =
  Option.is_none (attribute select "multiple")
  &&
  match Option.bind (attribute select "size") int_of_string_opt with
  | Some k ->
      k <= 1
  | None ->
      true

(* [select.value = v] selects the first option with the value [v], and no other
   option. *)
let rec set_select select v =
  ignore
    (List.fold_left
       (fun found o ->
         o.selected <- (not found) && String.equal (live_value o) v ;
         found || o.selected )
       false (options select) )

(* After a change of the options or of the size of a drop-down select: if no
   option is selected, the first option becomes selected. *)
and reset_select select =
  match options select with
  | first :: _ as l
    when drop_down select && not (List.exists (fun o -> o.selected) l) ->
      first.selected <- true
  | _ ->
      ()

(* The model of a range input: the browser keeps its value between [min] and
   [max] (by default 0 and 100). A value that is not a number becomes the
   middle. The model ignores [step]. *)

and number n k default =
  match Option.bind (attribute n k) float_of_string_opt with
  | Some x ->
      x
  | None ->
      default

and show x =
  if Float.is_integer x then string_of_int (int_of_float x)
  else string_of_float x

and is_range n = is_input "range" n

and clamp n v =
  if is_range n then
    let lo = number n "min" 0. in
    let hi = Float.max lo (number n "max" 100.) in
    match float_of_string_opt v with
    | Some x when x < lo ->
        show lo
    | Some x when x > hi ->
        show hi
    | Some _ ->
        v
    | None ->
        show ((lo +. hi) /. 2.)
  else v

(* The value with no write: the [value] attribute, or the text of an option or
   of a textarea. *)
and default_value n =
  match attribute n "value" with
  | Some v ->
      clamp n v
  | None when is_named "option" n || is_named "textarea" n ->
      text n
  | None ->
      clamp n ""

(* The live [value] property. *)
and live_value n =
  if is_named "select" n then
    match List.find_opt (fun o -> o.selected) (options n) with
    | Some o ->
        live_value o
    | None ->
        ""
  else match n.value with Some v -> v | None -> default_value n

let sanitize n = n.value <- Option.map (clamp n) n.value

(* The select whose options [n] holds: [n] itself, or the select of an
   optgroup. *)
let owner n =
  if is_named "select" n then Some n
  else if is_named "optgroup" n then
    match n.parent with Some p when is_named "select" p -> Some p | _ -> None
  else None

let options_changed parent = Option.iter reset_select (owner parent)

(* The model of a radio group: the radio buttons with the same name, in the same
   form, or in the same tree with no form. When a checked radio button joins a
   group, or a radio button becomes checked, the other buttons of its group
   become unchecked. *)

let is_radio n = is_input "radio" n

let rec tree_root n = match n.parent with Some p -> tree_root p | None -> n

let rec form n =
  match n.parent with
  | Some p ->
      if is_named "form" p then Some p else form p
  | None ->
      None

let settle n =
  if is_radio n && n.checked then
    match attribute n "name" with
    | None | Some "" ->
        ()
    | Some name ->
        let f = form n in
        iter_tree
          (fun m ->
            if
              m != n && is_radio m
              && Option.equal String.equal (attribute m "name") (Some name)
              && Option.equal ( == ) (form m) f
            then m.checked <- false )
          (tree_root n)

let create_element t ns name =
  step t "create_element" ;
  make (Element (ns, name)) ""

let create_text t s = step t "create_text" ; make Text s

let set_text t n s =
  if n.kind <> Text then misuse "set_text" "not a text node" ;
  step t "set_text" ;
  n.text <- s

(* After a change of the attribute [k]. *)
let attribute_changed n k =
  sanitize n ;
  settle n ;
  if is_named "select" n && (String.equal k "size" || String.equal k "multiple")
  then reset_select n

let set_attribute t n k v =
  need_element "set_attribute" n ;
  step t "set_attribute" ;
  n.attrs <- (k, v) :: List.remove_assoc k n.attrs ;
  attribute_changed n k

let remove_attribute t n k =
  if not (List.mem_assoc k n.attrs) then misuse "remove_attribute" k ;
  step t "remove_attribute" ;
  n.attrs <- List.remove_assoc k n.attrs ;
  attribute_changed n k

let write n (p : Dom.prop) =
  match p with
  | Value v ->
      if is_named "select" n then set_select n v
      else n.value <- Some (clamp n v)
  | Checked b ->
      n.checked <- b ;
      settle n

let set_property t n p =
  need_element "set_property" n ;
  step t "set_property" ;
  write n p

let browser_set n p =
  need_element "browser_set" n ;
  write n p

let get_value t n =
  need_element "get_value" n ; step t "get_value" ; live_value n

let get_checked t n =
  need_element "get_checked" n ;
  step t "get_checked" ;
  n.checked

(* A node that joins a tree: each checked radio button in it joins a group. *)
let joined n = iter_tree settle n

let insert_before t parent n r =
  let op = "insert_before" in
  need_element op parent ;
  if is_ancestor n parent then misuse op "the node is the parent or above it" ;
  let move = is_child parent n in
  if (not move) && Option.is_some n.parent then
    misuse op "the node has another parent" ;
  ( match r with
  | Some r when r == n ->
      misuse op "the reference is the node"
  | Some r when not (is_child parent r) ->
      misuse op "the reference is not a child"
  | _ ->
      () ) ;
  step t op ;
  if move then t.moves <- t.moves + 1 ;
  unlink n ;
  link parent n r ;
  joined n ;
  options_changed parent

let remove t parent n =
  if not (is_child parent n) then misuse "remove" "not a child" ;
  step t "remove" ;
  unlink n ;
  options_changed parent

let replace t parent n old =
  let op = "replace" in
  if not (is_child parent old) then misuse op "the old node is not a child" ;
  if Option.is_some n.parent then misuse op "the new node has a parent" ;
  step t op ;
  let r = old.next in
  unlink old ; link parent n r ; joined n ; options_changed parent

let rec clear_children n =
  match n.first with Some c -> unlink c ; clear_children n | None -> ()

let clear t n =
  need_element "clear" n ; step t "clear" ; clear_children n ; options_changed n

let set_inner_html t n h =
  need_element "set_inner_html" n ;
  step t "set_inner_html" ;
  clear_children n ;
  link n (make Html (Kilim.Unsafe_html.to_string h)) None ;
  options_changed n

let attach t n c =
  need_element "attach" n ;
  if Option.is_some n.cell then misuse "attach" "a second cell" ;
  step t "attach" ;
  n.cell <- Some c

let listen t n ev ~passive =
  need_element "listen" n ;
  if List.mem_assoc ev n.listeners then misuse "listen" ev ;
  step t "listen" ;
  n.listeners <- (ev, passive) :: n.listeners

let unlisten t n ev =
  if not (List.mem_assoc ev n.listeners) then misuse "unlisten" ev ;
  step t "unlisten" ;
  n.listeners <- List.remove_assoc ev n.listeners

let ops t : node Dom.t =
  { create_element= create_element t
  ; create_text= create_text t
  ; set_text= set_text t
  ; set_attribute= set_attribute t
  ; remove_attribute= remove_attribute t
  ; set_property= set_property t
  ; get_value= get_value t
  ; get_checked= get_checked t
  ; insert_before= insert_before t
  ; remove= remove t
  ; replace= replace t
  ; clear= clear t
  ; set_inner_html= set_inner_html t
  ; attach= attach t
  ; listen= listen t
  ; unlisten= unlisten t }

let parent n = n.parent

let name n =
  match n.kind with
  | Element (_, name) ->
      name
  | Text ->
      "#text"
  | Html ->
      "#html"

let ns n = match n.kind with Element (ns, _) -> Some ns | Text | Html -> None

let value = live_value

let checked n = n.checked

let cell n = n.cell

let listeners n = List.sort compare n.listeners

let same : type a b. a Dom.payload -> b Dom.payload -> (a, b) Type.eq option =
 fun a b ->
  match (a, b) with
  | Unit, Unit ->
      Some Equal
  | String, String ->
      Some Equal
  | Key, Key ->
      Some Equal
  | Bool, Bool ->
      Some Equal
  | _ ->
      None

let fire : type a. node -> string -> a Dom.payload -> a -> unit =
 fun n ev payload x ->
  match Option.bind n.cell (fun c -> Dom.handler c ev) with
  | None ->
      misuse "fire" ("no handler for " ^ ev)
  | Some (Dom.Handler h) -> (
    match same h.payload payload with
    | Some Equal ->
        h.fire x
    | None ->
        misuse "fire" ("another payload for " ^ ev) )

let escape ~quote s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' ->
          Buffer.add_string b "&amp;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | '"' when quote ->
          Buffer.add_string b "&quot;"
      | c ->
          Buffer.add_char b c )
    s ;
  Buffer.contents b

(* The HTML of [n]. [tag] gives the name of an element, [state] the text after
   its attributes, and [text] the text of a text node. *)
let rec render b ~tag ~state ~text n =
  match n.kind with
  | Text ->
      Buffer.add_string b (text n.text)
  | Html ->
      Buffer.add_string b n.text
  | Element (ns, name) ->
      let tag_name = tag ns name in
      Buffer.add_string b ("<" ^ tag_name) ;
      List.iter
        (fun (k, v) ->
          Buffer.add_string b (" " ^ k ^ "=\"" ^ escape ~quote:true v ^ "\"") )
        (List.sort compare n.attrs) ;
      Buffer.add_string b (state n) ;
      Buffer.add_char b '>' ;
      List.iter (render b ~tag ~state ~text) (children n) ;
      if not (ns = Html && Names.is_void name) then
        Buffer.add_string b ("</" ^ tag_name ^ ">")

let to_html n =
  let b = Buffer.create 64 in
  render b
    ~tag:(fun _ name -> name)
    ~state:(fun _ -> "")
    ~text:(escape ~quote:false) n ;
  Buffer.contents b

let payload_name : type a. a Dom.payload -> string = function
  | Unit ->
      ""
  | String ->
      "/value"
  | Key ->
      "/key"
  | Bool ->
      "/checked"

(* The value of a select, and a value that is not the value with no write. *)
let state n =
  let v = live_value n in
  let props =
    ( if is_named "select" n || not (String.equal v (default_value n)) then
        " .value=\"" ^ escape ~quote:true v ^ "\""
      else "" )
    ^ if n.checked then " .checked" else ""
  in
  let listeners =
    List.map
      (fun (ev, passive) -> " @" ^ ev ^ if passive then "~" else "")
      (listeners n)
  in
  let handlers =
    match n.cell with
    | None ->
        []
    | Some c ->
        List.map
          (fun (ev, Dom.Handler h) ->
            " !" ^ ev ^ payload_name h.payload
            ^ (if h.prevent_default then "+p" else "")
            ^ if h.stop then "+s" else "" )
          (Dom.handlers c)
  in
  props ^ String.concat "" listeners
  ^ String.concat "" (List.sort compare handlers)

let dump n =
  let b = Buffer.create 64 in
  render b
    ~tag:(fun ns name -> match ns with Dom.Svg -> "svg:" ^ name | Html -> name)
    ~state
    ~text:(fun s -> "\"" ^ escape ~quote:true s ^ "\"")
    n ;
  Buffer.contents b
