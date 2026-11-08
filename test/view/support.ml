(* Helpers for the diff tests: a page on the fake DOM, the checks of the
   retained tree, and a printer of views. *)

open Windtrap
open Kilim
module F = Kilim_testing.Fake_dom
module V = Private.View
module Diff = Private.Diff

type page =
  {doc: F.t; root: F.node; tree: (F.node, string) Diff.t; msgs: string list ref}

let mount_with doc v =
  let root = F.element "div" in
  let msgs = ref [] in
  let tree =
    Diff.mount (F.ops doc) ~parent:root
      ~post:(Private.Dom.post_now (fun m -> msgs := m :: !msgs))
      v
  in
  {doc; root; tree; msgs}

let mount v = mount_with (F.create ()) v

(* Patch with the counts at 0 before. *)
let patch p v = F.reset p.doc ; Diff.patch p.tree v

let dump p = F.dump p.root

let fresh v = dump (mount v)

(* The DOM node of the view. *)
let node p = Diff.dom (Diff.root p.tree)

let kids p = F.children (node p)

let child_dom (Diff.Child c) = Diff.dom c

let retained_kids p =
  match Diff.children (Diff.root p.tree) with
  | Some l ->
      List.map child_dom l
  | None ->
      []

let counts p = F.counts p.doc

let same_nodes a b = List.length a = List.length b && List.for_all2 ( == ) a b

(* Each retained node is a DOM child of its retained parent, and each DOM child
   of a retained element is retained. With [~order], the order is also the
   same. *)
let rec check_node :
    'msg. order:bool -> F.node -> (F.node, 'msg) Diff.node -> unit =
 fun ~order parent r ->
  let d = Diff.dom r in
  is_true ~msg:"the retained node is in its parent"
    (match F.parent d with Some q -> q == parent | None -> false) ;
  match Diff.children r with
  | None ->
      ()
  | Some l ->
      let doms = List.map child_dom l in
      let dom_kids = F.children d in
      equal int ~msg:"the number of children" (List.length dom_kids)
        (List.length doms) ;
      List.iter
        (fun c ->
          is_true ~msg:"each DOM child is retained"
            (List.exists (( == ) c) doms) )
        dom_kids ;
      if order then is_true ~msg:"the order" (same_nodes dom_kids doms) ;
      List.iter (fun (Diff.Child c) -> check_node ~order d c) l

let check_tree ?(order = true) p = check_node ~order p.root (Diff.root p.tree)

(* A component whose view is its props: a view of the props. Its messages are
   strings, as in the views of the tests. [equal_props] is [( == )]. *)
module Wrap = struct
  type props = string View.t

  type model = unit

  type msg = string

  type out = string

  let id : (props, model, msg, out) Private.Component.id =
    Private.Component.id ()

  let init _ = ((), Cmd.none)

  let update _ _ () _ = ((), Cmd.none)

  let view p () = p

  let subs _ () = Sub.none

  let equal_props = ( == )
end

let wrap ?key v = Private.Component.v (module Wrap) ?key ~on:Fun.id v

(* A printer of views for the counterexamples. *)
let rec show (v : string View.t) =
  match v with
  | V.Text s ->
      "\"" ^ s ^ "\""
  | V.Comp (V.C {spec; key; props; _}) -> (
      let key = match key with Some k -> " key=" ^ k | None -> "" in
      match Type.Id.provably_equal spec.id.tid Wrap.id.tid with
      | Some Type.Equal ->
          "<Wrap" ^ key ^ ">" ^ show props ^ "</Wrap>"
      | None ->
          "<comp" ^ key ^ ">" )
  | V.Element e ->
      let attrs = List.map (fun (k, v) -> " " ^ k ^ "=" ^ v) e.attrs in
      let value = match e.value with Some v -> " .value=" ^ v | None -> "" in
      let checked =
        match e.checked with
        | Some b ->
            " .checked=" ^ string_of_bool b
        | None ->
            ""
      in
      let evs =
        List.map
          (fun (h : _ V.handler) ->
            " !" ^ h.event ^ if h.prevent_default then "+p" else "" )
          e.events
      in
      let kids =
        match e.children with
        | V.Plain l ->
            String.concat " " (List.map show l)
        | V.Keyed l ->
            String.concat " "
              (List.map
                 (fun (k, v) -> Private.Key.to_string k ^ ":" ^ show v)
                 l )
        | V.Inner h ->
            "html:" ^ Unsafe_html.to_string h
      in
      "<" ^ e.name ^ String.concat "" attrs ^ value ^ checked
      ^ String.concat "" evs ^ ">" ^ kids ^ "</" ^ e.name ^ ">"

let pp_view ppf v = Format.pp_print_string ppf (show v)

(* A record of the DOM operations, in order. *)
let logged (ops : 'n Private.Dom.t) (log : string list ref) : 'n Private.Dom.t =
  let add s = log := s :: !log in
  { ops with
    create_element=
      (fun ns n ->
        add ("create " ^ n) ;
        ops.create_element ns n )
  ; set_attribute=
      (fun d k v ->
        add ("attr " ^ k) ;
        ops.set_attribute d k v )
  ; remove_attribute=
      (fun d k ->
        add ("remove " ^ k) ;
        ops.remove_attribute d k )
  ; set_inner_html=
      (fun d h ->
        add ("html " ^ Unsafe_html.to_string h) ;
        ops.set_inner_html d h )
  ; set_property= (fun d p -> add "prop" ; ops.set_property d p)
  ; get_value= (fun d -> add "get value" ; ops.get_value d)
  ; get_checked= (fun d -> add "get checked" ; ops.get_checked d)
  ; focused= (fun d -> add "focused" ; ops.focused d)
  ; insert_before= (fun d n r -> add "insert" ; ops.insert_before d n r)
  ; listen=
      (fun d e ~passive ->
        add ("listen " ^ e) ;
        ops.listen d e ~passive )
  ; attach= (fun d c -> add "attach" ; ops.attach d c) }

let rows ?(key = Key.int) ?(label = string_of_int) keys =
  List.map (fun k -> (key k, El.li [txt (label k)])) keys

let ul ?key keys = View.keyed El.ul (rows ?key keys)

(* [colliding n bits] is [n] ints from 0 whose keys have the same low [bits]
   bits of the hash: [Private.Key.hash (Key.int 0)] is 0. In a table of [2^bits]
   slots or fewer, these keys start at slot 0. *)
let colliding n bits =
  let mask = (1 lsl bits) - 1 in
  let rec go i acc c =
    if c = n then Array.of_list (List.rev acc)
    else if Private.Key.hash (Key.int i) land mask = 0 then
      go (i + 1) (i :: acc) (c + 1)
    else go (i + 1) acc c
  in
  go 0 [] 0

let texts p = List.map F.text (kids p)
