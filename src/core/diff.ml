type ('node, 'msg) node = ('node, 'msg) Tree.node =
  {mutable dom: 'node; mutable shape: ('node, 'msg) shape}

and ('node, 'msg) shape = ('node, 'msg) Tree.shape =
  | Text of string
  | Element of ('node, 'msg) element
  | Comp :
      { key: string option
      ; on: ('o -> 'msg) ref
      ; inst: ('node, 'c) Tree.inst
      ; data: ('p, 'm, 'c, 'o) Tree.data
      ; child: ('node, 'c) node }
      -> ('node, 'msg) shape
  | Boundary of ('node, 'msg) Tree.boundary

and ('node, 'msg) element = ('node, 'msg) Tree.element =
  { ns: Dom.ns
  ; name: string
  ; cell: Dom.cell
  ; mutable attrs: (string * string) list
  ; mutable listened: (string * bool) list
  ; mutable children: ('node, 'msg) children }

and ('node, 'msg) children = ('node, 'msg) Tree.children =
  | Plain of ('node, 'msg) node array
  | Keyed of Key.t array * ('node, 'msg) node array
  | Inner of Unsafe_html.t

type ('node, 'msg) ctx = ('node, 'msg) Tree.ctx =
  { ops: 'node Dom.t
  ; post: 'msg Dom.post
  ; ns: Dom.ns
  ; controlled: ('node * Dom.cell) list ref
  ; owner: ('node, 'msg) Tree.inst
  ; env: 'node Tree.env
  ; boundary: 'node Tree.any_boundary option }

type ('node, 'msg) t = ('node, 'msg) Tree.mount =
  {ctx: ('node, 'msg) ctx; parent: 'node; root: ('node, 'msg) node}

(* The functions of the component nodes (Reconcile). Component.v installs them:
   thus an app with no component does not link Reconcile. *)
type hooks =
  { build:
      'node 'msg.
      ('node, 'msg) ctx -> 'node -> 'msg View.comp -> ('node, 'msg) node
  ; patch:
      'node 'msg.
      ('node, 'msg) ctx -> 'node -> ('node, 'msg) node -> 'msg View.comp -> unit
  ; render: 'node 'msg. ('node, 'msg) Tree.inst -> unit
  ; walk: 'node 'msg. 'node Tree.env -> ('node, 'msg) node -> unit
  ; walk_children:
      'node 'msg. 'node Tree.env -> ('node, 'msg) Tree.children -> unit
  ; flush:
      'node.
      'node Tree.env -> ('node Tree.any_boundary option -> exn -> unit) -> bool
  }

let no_component () = invalid_arg "kilim: a component node with no Component.v"

let hooks =
  ref
    { build= (fun _ _ _ -> no_component ())
    ; patch= (fun _ _ _ _ -> no_component ())
    ; render= (fun _ -> no_component ())
    ; walk= (fun _ _ -> ())
    ; walk_children= (fun _ _ -> ())
    ; flush= (fun _ _ -> false) }

(* The functions of the boundary nodes (Boundary). View.boundary installs
   them. *)
type bounds =
  { build_boundary:
      'node 'msg.
      ('node, 'msg) ctx -> 'node -> 'msg View.boundary -> ('node, 'msg) node
  ; patch_boundary:
      'node 'msg.
         ('node, 'msg) ctx
      -> 'node
      -> ('node, 'msg) node
      -> 'msg View.boundary
      -> unit
  ; show:
      'node 'msg. 'node Tree.env -> ('node, 'msg) Tree.boundary -> exn -> unit
  ; fail: 'node. 'node Tree.env -> 'node Tree.any_boundary -> exn -> unit }

let no_boundary () = invalid_arg "kilim: a boundary node with no View.boundary"

let bounds =
  ref
    { build_boundary= (fun _ _ _ -> no_boundary ())
    ; patch_boundary= (fun _ _ _ _ -> no_boundary ())
    ; show= (fun _ _ _ -> ())
    ; fail= (fun _ _ _ -> ()) }

(* The DOM node of a retained node. A component node and a boundary node are
   transparent: the DOM node of a component node is the DOM node of the root of
   the view of its instance, and the DOM node of a boundary node is the DOM node
   of its child. A text and an element keep their DOM node in [dom]. *)
let rec dom : 'node 'msg. ('node, 'msg) node -> 'node =
 fun r ->
  match r.shape with
  | Comp c ->
      dom c.child
  | Boundary b ->
      dom b.child
  | Text _ | Element _ ->
      r.dom

(* With no child instance and no boundary, the view of the owner has no
   component node and no boundary node: no walk. Thus an app with no component
   keeps the cost of Phase 1. *)
let has_children (ctx : _ ctx) =
  ( Dlist.length ctx.owner.children > 0
  || match ctx.owner.owned with Some l -> Dlist.length l > 0 | None -> false )
  [@mutate off "a pure shortcut: the walk finds no component or boundary node"]

let gone ctx r = if has_children ctx then !hooks.walk ctx.env r

let gone_children ctx c =
  if has_children ctx then !hooks.walk_children ctx.env c

(* One empty array for all: under js_of_ocaml, each [[||]] is a new array. *)
let empty = [||]

(* The plan of a keyed list. The diff makes it, and does all key checks, before
   it writes the DOM of the element. *)
type 'msg plan =
  { nk: Key.t array
  ; nv: 'msg View.t array
  ; p: int  (** the length of the common start *)
  ; s: int  (** the length of the common end *)
  ; src: int array
        (** for each new node of the middle: its old index, or -1 for a new key
        *)
  ; matched: bool array  (** for each old node of the middle *)
  ; reads: int
        (** the number of slots that the plan read in the table, or -1 if the
            plan uses a map. For the tests. *) }

let duplicate k =
  invalid_arg ("kilim: View.keyed: a duplicate key: " ^ Key.to_string k)

module Kmap = Map.Make (Key)

(* The searches of a plan read more slots of the table than the budget. *)
exception Long

(* The budget of a plan: the plan can read 8 slots of the table for each key
   that it adds to the table or finds in it. With usual keys, the plan reads
   about 2 slots for each key. *)
let budget keys = 8 * keys

(* [slot left nk slots key i] is the slot of [key] in the open-addressing table
   [slots], from slot [i]: the slot that holds [key], or the empty slot where
   [key] goes. A slot holds an index of [nk], or -1 if it is empty. The size is
   a power of 2, and at most half of the slots are full. [left] is the number of
   slots that the plan can read before it uses a map. *)
let rec slot left nk slots key i =
  if !left <= 0 then raise Long ;
  decr left ;
  let j = slots.(i) in
  if j < 0 || Key.equal nk.(j) key then i
  else slot left nk slots key ((i + 1) land (Array.length slots - 1))

let start slots key = Key.hash key land (Array.length slots - 1)

(* The smallest power of 2 that is [n] or more. *)
let rec pow2 n s = if s >= n then s else pow2 n (s + s)

(* The index of the new middle [nk.(p) .. nk.(p + k - 1)]: the new index of a
   key, or -1. A table, or a map if the searches in the table are too long: the
   keys can collide, by chance or by intent. *)
type index = Table of int ref * int array | Tree of int Kmap.t

let table left nk p k =
  let slots = Array.make (pow2 (k + k) 1) (-1) in
  for j = p to p + k - 1 do
    let i = slot left nk slots nk.(j) (start slots nk.(j)) in
    if slots.(i) >= 0 then duplicate nk.(j) ;
    slots.(i) <- j
  done ;
  Table (left, slots)

let tree nk p k =
  let m = ref Kmap.empty in
  for j = p to p + k - 1 do
    if Kmap.mem nk.(j) !m then duplicate nk.(j) ;
    m := Kmap.add nk.(j) j !m
  done ;
  Tree !m

let find nk index key =
  match index with
  | Table (left, slots) ->
      slots.(slot left nk slots key (start slots key))
  | Tree m -> (
    match Kmap.find_opt key m with Some j -> j | None -> -1 )

(* The old list has no duplicate key. Thus a duplicate of the new list is two
   keys of the new middle, or one key of the new middle and one key of the
   common start or end. An index of the new middle finds them, and it gives the
   new index of each old key of the middle. *)
let fill nk ok p s index =
  let m = Array.length ok and n = Array.length nk in
  let k = n - p - s and km = m - p - s in
  let src = Array.make k (-1) in
  let matched = Array.make km false in
  let outside j = if find nk index nk.(j) >= 0 then duplicate nk.(j) in
  for j = 0 to p - 1 do
    outside j
  done ;
  for j = p + k to n - 1 do
    outside j
  done ;
  for i = p to p + km - 1 do
    let j = find nk index ok.(i) in
    if j >= 0 then begin
      src.(j - p) <- i ;
      matched.(i - p) <- true
    end
  done ;
  (src, matched)

let make (ok : Key.t array) nk (nv : 'msg View.t array) : 'msg plan =
  let m = Array.length ok and n = Array.length nk in
  let p = ref 0 in
  while !p < m && !p < n && Key.equal ok.(!p) nk.(!p) do
    incr p
  done ;
  let p = !p in
  let s = ref 0 in
  let limit = min m n - p in
  while !s < limit && Key.equal ok.(m - 1 - !s) nk.(n - 1 - !s) do
    incr s
  done ;
  let s = !s in
  (* The lengths of the new and the old middle. With no new middle, no key is
     new and no key moves: the index is not necessary. The plan adds the [k]
     keys of the new middle to the table, then finds the [p + s] keys of the
     common start and end and the [km] keys of the old middle. *)
  let k = n - p - s and km = m - p - s in
  if k > 0 then begin
    let all = budget (n + km) in
    let left = ref all in
    match fill nk ok p s (table left nk p k) with
    | src, matched ->
        {nk; nv; p; s; src; matched; reads= all - !left}
    | exception Long ->
        let src, matched = fill nk ok p s (tree nk p k) in
        {nk; nv; p; s; src; matched; reads= -1}
  end
  else {nk; nv; p; s; src= empty; matched= Array.make km false; reads= 0}

let plan ok (l : (Key.t * 'msg View.t) list) =
  make ok (Array.of_list (List.map fst l)) (Array.of_list (List.map snd l))

let probes nk ok =
  let pl = make ok nk empty in
  if pl.reads < 0 then None else Some pl.reads

(* For each entry of [a] that is not negative: [true] if the entry is in one
   longest strictly increasing subsequence of these entries. O(k log k). *)
let lis a =
  let k = Array.length a in
  let keep = Array.make k false in
  let pred = Array.make k (-1) in
  (* tails.(l) is the index of the last entry of an increasing run of length l +
     1. *)
  let tails =
    Array.make k (0 [@mutate off "each entry is written before it is read"])
  in
  let len = ref 0 in
  for i = 0 to k - 1 do
    let x = a.(i) in
    if x >= 0 then begin
      let lo = ref 0 and hi = ref !len in
      while !lo < !hi do
        let mid = (!lo + !hi) / 2 in
        if (a.(tails.(mid)) < x) [@mutate off "the entries are distinct"] then
          lo := mid + 1
        else hi := mid
      done ;
      if !lo > 0 then pred.(i) <- tails.(!lo - 1) ;
      tails.(!lo) <- i ;
      if !lo = !len then incr len
    end
  done ;
  let i = ref (if !len > 0 then tails.(!len - 1) else -1) in
  while !i >= 0 do
    keep.(!i) <- true ;
    i := pred.(!i)
  done ;
  keep

type 'msg prepared =
  | Pplain of 'msg View.t list
  | Pkeyed of 'msg plan
  | Pinner of Unsafe_html.t

let prepare ok (c : 'msg View.children) =
  match c with
  | View.Plain l ->
      Pplain l
  | View.Keyed l ->
      Pkeyed (plan ok l)
  | View.Inner h ->
      Pinner h

let ns_of (ctx : _ ctx) name : Dom.ns =
  if String.equal name "svg" then Svg else ctx.ns

let child_ctx (ctx : _ ctx) name (ns : Dom.ns) =
  let ns : Dom.ns = if String.equal name "foreignObject" then Html else ns in
  if ns = ctx.ns then ctx else {ctx with ns}

(* The events with a listener on the element, and passive. *)
let listened (evs : _ View.handler list) =
  List.filter_map
    (fun (h : _ View.handler) ->
      if h.bubbles then None else Some (h.event, not h.prevent_default) )
    evs

(* [same_listened l evs] is [listened evs = l], with no allocation. *)
let rec same_listened l (evs : _ View.handler list) =
  match evs with
  | [] -> (
    match l with [] -> true | _ :: _ -> false )
  | h :: evs -> (
      let bubbles = h.bubbles in
      if
        bubbles
        [@mutate off "a false value gives false: the caller compares again"]
      then same_listened l evs
      else
        match l with
        | [] ->
            false
        | (ev, passive) :: l ->
            String.equal h.event ev
            && Bool.equal (not h.prevent_default) passive
            && same_listened l evs )

(* The cell keeps the view values. An element with a view value goes to the list
   of the controlled elements. *)
let control ctx d (cell : Dom.cell) (ve : _ View.element) =
  cell.value <- ve.value ;
  cell.checked <- ve.checked ;
  match (ve.value, ve.checked) with
  | None, None ->
      ()
  | Some _, _ | _, Some _ ->
      ctx.controlled := (d, cell) :: !(ctx.controlled)

(* The composition flag stops a [value] write only while the element has the
   focus. A composition cannot go on in an element with no focus. A move or a
   removal takes the focus away, and Firefox and WebKit then send no
   compositionend and no focusout. Thus a write site that finds the flag on an
   element with no focus clears the flag. The focus is read only when the flag
   is set. *)
let composing (ops : _ Dom.t) d (cell : Dom.cell) =
  if cell.composing && not (ops.focused d) then cell.composing <- false ;
  cell.composing

(* The browser can change [value] and [checked] with no write of the diff: the
   user types, a radio button unchecks the other buttons of its group, a select
   selects an option. Thus the diff compares the view value with the live
   property, and writes only a different value. A write of [value] stops an IME
   composition, thus no [value] write while the cell is composing. *)
let set_props (ops : _ Dom.t) (d, (cell : Dom.cell)) =
  ( match cell.value with
  | Some x
    when (not (composing ops d cell)) && not (String.equal x (ops.get_value d))
    ->
      ops.set_property d (Value x)
  | Some _ | None ->
      () ) ;
  match cell.checked with
  | Some x when not (Bool.equal x (ops.get_checked d)) ->
      ops.set_property d (Checked x)
  | Some _ | None ->
      ()

let restore ops d cell = set_props ops (d, cell)

(* [run ctx f] runs [f ()], then writes the properties of the controlled
   elements, in the order of their end tags in the document: a child before its
   parent. A mount and a patch thus write in the same order, and give the same
   result for two checked radio buttons in one group. All the attributes and the
   children of the patch come first: a later change of a name or of a type can
   uncheck a radio button. The list starts empty: an exception can stop the last
   run before its writes. *)
let run ctx f =
  ctx.controlled := [] ;
  let x = f () in
  List.iter (set_props ctx.ops) (List.rev !(ctx.controlled)) ;
  x

(* [has k l] is [true] if the list [l] of pairs has the name [k]. *)
let rec has k = function
  | [] ->
      false
  | (k', _) :: l ->
      String.equal k k' || has k l

(* [same k v l] is [true] if the first pair of [l] with the name [k] is [(k,
   v)]. *)
let rec same k v = function
  | [] ->
      false
  | (k', v') :: l ->
      if String.equal k k' then String.equal v v' else same k v l

let rec remove_attrs (ops : _ Dom.t) d nw = function
  | [] ->
      ()
  | (k, _) :: old ->
      if not (has k nw) then ops.remove_attribute d k ;
      remove_attrs ops d nw old

let rec set_attrs (ops : _ Dom.t) d old = function
  | [] ->
      ()
  | (k, v) :: nw ->
      if not (same k v old) then ops.set_attribute d k v ;
      set_attrs ops d old nw

(* The removals come first: in the HTML namespace, two names that differ only in
   case are one attribute. *)
let patch_attrs ops d old nw =
  remove_attrs ops d nw old ; set_attrs ops d old nw

let patch_listeners (ops : _ Dom.t) d old nw =
  List.iter
    (fun (ev, _) -> if not (List.mem_assoc ev nw) then ops.unlisten d ev)
    old ;
  List.iter
    (fun (ev, passive) ->
      match List.assoc_opt ev old with
      | Some p when Bool.equal p passive ->
          ()
      | Some _ ->
          ops.unlisten d ev ; ops.listen d ev ~passive
      | None ->
          ops.listen d ev ~passive )
    nw

(* Remove all children, if there are some. The DOM operation comes first, then
   the walk: a DOM fault leaves each retained node in the DOM. *)
let clear ctx d e none =
  let old = e.children in
  ( match old with
  | Plain [||] | Keyed (_, [||]) ->
      ()
  | Plain _ | Keyed _ | Inner _ ->
      ctx.ops.Dom.clear d ) ;
  e.children <- none ;
  gone_children ctx old

(* Remove the plain children of [a] from index [n]. [n] is 0, or less than the
   length of [a]. *)
let shrink ctx d e a n =
  if n = 0 then clear ctx d e (Plain empty)
  else begin
    let m = Array.length a in
    let i = ref n in
    ( try
        while !i < m do
          ctx.ops.Dom.remove d (dom a.(!i)) ;
          gone ctx a.(!i) ;
          incr i
        done
      with exn ->
        e.children <-
          Plain (Array.append (Array.sub a 0 n) (Array.sub a !i (m - !i))) ;
        raise exn ) ;
    e.children <- Plain (Array.sub a 0 n)
  end

let rec build :
    'node 'msg. ('node, 'msg) ctx -> 'node -> 'msg View.t -> ('node, 'msg) node
    =
 fun ctx parent v ->
  match v with
  | View.Text s ->
      {dom= ctx.ops.create_text s; shape= Text s}
  | View.Element ve ->
      let kids = prepare empty ve.children in
      let ns = ns_of ctx ve.name in
      let d = ctx.ops.create_element ns ve.name in
      (* The cell keeps the handlers of the view: the handlers are made only
         when an event occurs. *)
      let cell =
        { Dom.events= Events (ve.events, ctx.post)
        ; value= None
        ; checked= None
        ; composing= false }
      in
      ctx.ops.attach d cell ;
      List.iter (fun (k, v) -> ctx.ops.set_attribute d k v) ve.attrs ;
      let listened = listened ve.events in
      List.iter (fun (ev, passive) -> ctx.ops.listen d ev ~passive) listened ;
      let cctx = child_ctx ctx ve.name ns in
      let children =
        match kids with
        | Pplain l ->
            Plain (append cctx d (Array.of_list l) ~on_error:ignore)
        | Pkeyed pl ->
            Keyed (pl.nk, append cctx d pl.nv ~on_error:ignore)
        | Pinner h ->
            ctx.ops.set_inner_html d h ; Inner h
      in
      control ctx d cell ve ;
      { dom= d
      ; shape=
          Element {ns; name= ve.name; cell; attrs= ve.attrs; listened; children}
      }
  | View.Comp c ->
      !hooks.build ctx parent c
  | View.Boundary b ->
      !bounds.build_boundary ctx parent b

(* Build each view and append it to [d]. If a DOM operation raises, [on_error]
   gets the nodes that are in [d], before the exception goes on. *)
and append :
    'node 'msg.
       ('node, 'msg) ctx
    -> 'node
    -> 'msg View.t array
    -> on_error:(('node, 'msg) node array -> unit)
    -> ('node, 'msg) node array =
 fun ctx d nv ~on_error ->
  let n = Array.length nv in
  if n = 0 then empty
  else begin
    let first = build ctx d nv.(0) in
    ctx.ops.insert_before d (dom first) None ;
    let a = Array.make n first in
    let i = ref 1 in
    ( try
        while !i < n do
          let c = build ctx d nv.(!i) in
          ctx.ops.insert_before d (dom c) None ;
          a.(!i) <- c ;
          incr i
        done
      with exn ->
        on_error (Array.sub a 0 !i) ;
        raise exn ) ;
    a
  end

and patch_node :
    'node 'msg.
    ('node, 'msg) ctx -> 'node -> ('node, 'msg) node -> 'msg View.t -> unit =
 fun ctx parent r v ->
  match (r.shape, v) with
  | Text s, View.Text s' ->
      if not (String.equal s s') then begin
        ctx.ops.set_text r.dom s' ;
        r.shape <- Text s'
      end
  | Element e, View.Element ve
    when String.equal e.name ve.name && e.ns = ns_of ctx ve.name ->
      patch_element ctx r.dom e ve
  | Comp _, View.Comp c ->
      !hooks.patch ctx parent r c
  | Boundary _, View.Boundary b ->
      !bounds.patch_boundary ctx parent r b
  | _ ->
      replace ctx parent r v

(* Build [v] in the place of [r]. The DOM operation comes first, then the walk
   of the old subtree. *)
and replace :
    'node 'msg.
    ('node, 'msg) ctx -> 'node -> ('node, 'msg) node -> 'msg View.t -> unit =
 fun ctx parent r v ->
  let fresh = build ctx parent v in
  ctx.ops.replace parent (dom fresh) (dom r) ;
  let old = {dom= r.dom; shape= r.shape} in
  r.dom <- fresh.dom ;
  r.shape <- fresh.shape ;
  gone ctx old

and patch_element :
    'node 'msg.
       ('node, 'msg) ctx
    -> 'node
    -> ('node, 'msg) element
    -> 'msg View.element
    -> unit =
 fun ctx d e ve ->
  let ok =
    match e.children with Keyed (ok, _) -> ok | Plain _ | Inner _ -> empty
  in
  let kids = prepare ok ve.children in
  ( match (e.attrs, ve.attrs) with
  | (([], []) [@mutate off "patch_attrs gives the same result"]) ->
      ()
  | old, nw ->
      patch_attrs ctx.ops d old nw ;
      e.attrs <- nw ) ;
  ( match (ve.events, e.cell.events) with
  | (([], Events ([], _)) [@mutate off "the second case gives the same result"])
    ->
      ()
  | evs, _ ->
      e.cell.events <- Events (evs, ctx.post) ;
      if not (same_listened e.listened evs) then begin
        let listened = listened evs in
        patch_listeners ctx.ops d e.listened listened ;
        e.listened <- listened
      end ) ;
  patch_children ctx d e kids ;
  control ctx d e.cell ve

and patch_children :
    'node 'msg.
    ('node, 'msg) ctx -> 'node -> ('node, 'msg) element -> 'msg prepared -> unit
    =
 fun ctx d e kids ->
  let cctx = child_ctx ctx e.name e.ns in
  match (e.children, kids) with
  | Plain a, Pplain l ->
      patch_plain cctx d e a 0 l
  | Keyed (ok, on), Pkeyed pl ->
      patch_keyed cctx d e ok on pl
  | Inner h, Pinner h' ->
      if not (String.equal (Unsafe_html.to_string h) (Unsafe_html.to_string h'))
      then begin
        ctx.ops.set_inner_html d h' ;
        e.children <- Inner h'
      end
  | ((Plain _ | Keyed _) as old), Pinner h ->
      ctx.ops.set_inner_html d h ;
      e.children <- Inner h ;
      gone_children cctx old
  | (Keyed _ | Inner _), Pplain l ->
      clear cctx d e (Plain empty) ;
      e.children <-
        Plain
          (append cctx d (Array.of_list l) ~on_error:(fun a ->
               e.children <- Plain a ) )
  | (Plain _ | Inner _), Pkeyed pl ->
      clear cctx d e (Keyed (empty, empty)) ;
      e.children <- Keyed (pl.nk, append_keyed cctx d e pl)

and append_keyed :
    'node 'msg.
       ('node, 'msg) ctx
    -> 'node
    -> ('node, 'msg) element
    -> 'msg plan
    -> ('node, 'msg) node array =
 fun ctx d e pl ->
  append ctx d pl.nv ~on_error:(fun a ->
      e.children <- Keyed (Array.sub pl.nk 0 (Array.length a), a) )

(* Patch the plain children of [a] from index [i] with the views [l], then
   remove or append the nodes of the difference. With no change of the length,
   the patch makes no array and keeps [e.children]. *)
and patch_plain :
    'node 'msg.
       ('node, 'msg) ctx
    -> 'node
    -> ('node, 'msg) element
    -> ('node, 'msg) node array
    -> int
    -> 'msg View.t list
    -> unit =
 fun ctx d e a i l ->
  match l with
  | v :: rest when i < Array.length a ->
      patch_node ctx d a.(i) v ;
      patch_plain ctx d e a (i + 1) rest
  | [] ->
      if
        ( i < Array.length a )
        [@mutate off "for i = m, no node goes, and the array is a copy"]
      then shrink ctx d e a i
  | l ->
      let extra =
        append ctx d (Array.of_list l) ~on_error:(fun b ->
            e.children <- Plain (Array.append a b) )
      in
      e.children <- Plain (Array.append a extra)

and patch_keyed :
    'node 'msg.
       ('node, 'msg) ctx
    -> 'node
    -> ('node, 'msg) element
    -> Key.t array
    -> ('node, 'msg) node array
    -> 'msg plan
    -> unit =
 fun ctx d e ok on pl ->
  let m = Array.length on and n = Array.length pl.nk in
  if n = 0 then clear ctx d e (Keyed (empty, empty))
  else if m = 0 then e.children <- Keyed (pl.nk, append_keyed ctx d e pl)
  else begin
    let {nk; nv; p; s; src; matched; reads= _} = pl in
    for i = 0 to p - 1 do
      patch_node ctx d on.(i) nv.(i)
    done ;
    let keep = lis src in
    let nn = Array.make n on.(0) in
    Array.blit on 0 nn 0 p ;
    Array.blit on (m - s) nn (n - s) s ;
    let k = Array.length src and km = Array.length matched in
    let placed = Array.make k false in
    let removed = Array.make km false in
    (* The retained list after an exception: each node that is in [d]. *)
    let fixup () =
      let ks = ref [] and ns = ref [] in
      let add k r =
        ks := k :: !ks ;
        ns := r :: !ns
      in
      for i = m - 1 downto m - s do
        add ok.(i) on.(i)
      done ;
      for i = p + km - 1 downto p do
        if not (matched.(i - p) || removed.(i - p)) then add ok.(i) on.(i)
      done ;
      for j = p + k - 1 downto p do
        let i = src.(j - p) in
        if i >= 0 then add nk.(j) on.(i)
        else if placed.(j - p) then add nk.(j) nn.(j)
      done ;
      for i = p - 1 downto 0 do
        add ok.(i) on.(i)
      done ;
      Keyed (Array.of_list !ks, Array.of_list !ns)
    in
    match
      (* First the patches and the new nodes of the middle and of the common
         end, in the order of the document: see [run]. *)
      for j = p to p + k - 1 do
        let i = src.(j - p) in
        if i < 0 then nn.(j) <- build ctx d nv.(j)
        else begin
          nn.(j) <- on.(i) ;
          patch_node ctx d on.(i) nv.(j)
        end
      done ;
      for i = s downto 1 do
        patch_node ctx d on.(m - i) nv.(n - i)
      done ;
      (* Then the inserts and the moves, from the end: the node after [j] is
         always in its new place. *)
      let place j =
        let before = if j + 1 < n then Some (dom nn.(j + 1)) else None in
        ctx.ops.insert_before d (dom nn.(j)) before
      in
      for j = p + k - 1 downto p do
        if src.(j - p) < 0 then begin
          place j ;
          placed.(j - p) <- true
        end
        else if not keep.(j - p) then place j
      done ;
      for i = p to p + km - 1 do
        if not matched.(i - p) then begin
          ctx.ops.remove d (dom on.(i)) ;
          removed.(i - p) <- true ;
          gone ctx on.(i)
        end
      done
    with
    | () ->
        e.children <- Keyed (nk, nn)
    | exception exn ->
        e.children <- fixup () ;
        raise exn
  end

let start ctx ~parent v =
  let root =
    run ctx (fun () ->
        let root = build ctx parent v in
        ctx.ops.Dom.insert_before parent (dom root) None ;
        root )
  in
  {ctx; parent; root}

(* The env of a diff with no runtime: the messages of a child go nowhere, and
   its commands start no task. *)
let env () : _ Tree.env =
  { post= (fun _ -> {Dom.post= (fun _ _ -> ())})
  ; command= (fun _ _ -> ())
  ; viewed= ignore
  ; caught= ignore
  ; serial= 1
  ; work= Instance.Work.empty
  ; passes= false
  ; stale= []
  ; touched= [] }

let mount ops ~parent ~post v =
  let owner = Instance.root (Cancel.root ()) in
  start
    {ops; post; ns= Html; controlled= ref []; owner; env= env (); boundary= None}
    ~parent v

let patch t v = run t.ctx (fun () -> patch_node t.ctx t.parent t.root v)

let render i = !hooks.render i

let root t = t.root

type 'node child = Child : ('node, 'msg) node -> 'node child

(* The retained children of the element of a node. A component node is
   transparent. *)
let rec children : 'node 'msg. ('node, 'msg) node -> 'node child list option =
 fun r ->
  match r.shape with
  | Text _ ->
      None
  | Comp c ->
      children c.child
  | Boundary b ->
      children b.child
  | Element e -> (
    match e.children with
    | Plain a | Keyed (_, a) ->
        Some (Array.to_list (Array.map (fun c -> Child c) a))
    | Inner _ ->
        None )
