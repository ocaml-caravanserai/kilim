(* A small model of the component runtime of section 09 of docs/design.html. No
   Obj.magic: Type.Id gives back the types of a stored instance. *)

module Component_id = struct
  type ('p, 'm, 'msg, 'o) t = ('p * 'm * 'msg * 'o) Type.Id.t

  let make () : ('p, 'm, 'msg, 'o) t = Type.Id.make ()
end

module Cmd = struct
  type 'msg t = None | Emit of (unit -> unit)

  let none = None
end

module Ctx = struct
  type ('msg, 'out) t = {emit: 'out -> unit}

  let emit ctx out = Cmd.Emit (fun () -> ctx.emit out)
end

(* A view: elements with buttons that give a message, and component nodes. *)
type 'msg view =
  | Text of string
  | Button of string * 'msg
  | El of string * 'msg view list
  | Comp of 'msg comp

and 'msg comp =
  | Comp_node :
      ('p, 'm, 'cmsg, 'o) spec * string option * ('o -> 'msg) * 'p
      -> 'msg comp

and ('p, 'm, 'msg, 'o) spec =
  { id: ('p, 'm, 'msg, 'o) Component_id.t
  ; name: string
  ; init: 'p -> 'm * 'msg Cmd.t
  ; update: ('msg, 'o) Ctx.t -> 'p -> 'm -> 'msg -> 'm * 'msg Cmd.t
  ; view: 'p -> 'm -> 'msg view }

module type COMPONENT = sig
  type props

  type model

  type msg

  type out

  val id : (props, model, msg, out) Component_id.t

  val name : string

  val init : props -> model * msg Cmd.t

  val update : (msg, out) Ctx.t -> props -> model -> msg -> model * msg Cmd.t

  val view : props -> model -> msg view
end

(* OCaml 5.5: a module-dependent function. *)
let v :
    (module C : COMPONENT)
    -> ?key:string -> on:(C.out -> 'msg) -> C.props -> 'msg view =
 fun (module C) ?key ~on props ->
  Comp
    (Comp_node
       ( {id= C.id; name= C.name; init= C.init; update= C.update; view= C.view}
       , key
       , on
       , props ) )

(* The instances. *)
type ('p, 'm, 'msg, 'o) inst =
  { spec: ('p, 'm, 'msg, 'o) spec
  ; key: string option
  ; mutable props: 'p
  ; mutable model: 'm
  ; mutable last: 'msg view
  ; mutable kids: packed list
  ; mutable on_out: 'o -> unit (* replaced on each render of the parent *)
  ; mutable alive: bool }

and packed = Inst : ('p, 'm, 'msg, 'o) inst -> packed

let views = ref 0

let inits = ref 0

let queue : (unit -> unit) Queue.t = Queue.create ()

let rec unmount (Inst i) =
  i.alive <- false ;
  List.iter unmount i.kids ;
  i.kids <- []

(* Render the view of an instance, and reconcile its component children by
   place: the position among component nodes and the key. *)
let rec render : type p m msg o. (p, m, msg, o) inst -> unit =
 fun i ->
  incr views ;
  let view = i.spec.view i.props i.model in
  i.last <- view ;
  let rec comps acc = function
    | Comp c ->
        c :: acc
    | El (_, xs) ->
        List.fold_left comps acc xs
    | Text _ | Button _ ->
        acc
  in
  let nodes = List.rev (comps [] view) in
  let old = Array.of_list i.kids in
  let kids =
    List.mapi
      (fun pos (Comp_node (spec, key, on, props)) ->
        let reuse =
          if pos < Array.length old then
            let (Inst o) = old.(pos) in
            match Type.Id.provably_equal spec.id o.spec.id with
            | Some Type.Equal when o.key = key ->
                (* The same component and key: the types are now known. *)
                o.on_out <-
                  (fun out -> Queue.push (fun () -> dispatch i (on out)) queue) ;
                if not (Eq_spike.Eq.shallow_equal o.props props) then begin
                  o.props <- props ;
                  render o
                end ;
                Some (Inst o)
            | _ ->
                None
          else None
        in
        match reuse with
        | Some k ->
            k
        | None ->
            if pos < Array.length old then unmount old.(pos) ;
            Inst
              (mount spec key props (fun out ->
                   Queue.push (fun () -> dispatch i (on out)) queue ) ) )
      nodes
  in
  (* Unmount the old children that have no place now. *)
  Array.iteri (fun pos k -> if pos >= List.length nodes then unmount k) old ;
  i.kids <- kids

and mount : type p m msg o.
       (p, m, msg, o) spec
    -> string option
    -> p
    -> (o -> unit)
    -> (p, m, msg, o) inst =
 fun spec key props on_out ->
  incr inits ;
  let model, _ = spec.init props in
  let i =
    {spec; key; props; model; last= Text ""; kids= []; on_out; alive= true}
  in
  render i ; i

and dispatch : type p m msg o. (p, m, msg, o) inst -> msg -> unit =
 fun i msg ->
  if i.alive then begin
    let ctx = {Ctx.emit= (fun out -> i.on_out out)} in
    let model, cmd = i.spec.update ctx i.props i.model msg in
    (match cmd with Cmd.Emit f -> f () | Cmd.None -> ()) ;
    (* Model change by physical identity. *)
    if model != i.model then begin
      i.model <- model ;
      render i
    end
  end

let settle () =
  while not (Queue.is_empty queue) do
    (Queue.pop queue) ()
  done

(* A click on a button of the view of an instance. *)
let click : type p m msg o. (p, m, msg, o) inst -> string -> unit =
 fun i label ->
  let rec find = function
    | Button (l, m) when l = label ->
        Some m
    | El (_, xs) ->
        List.find_map find xs
    | _ ->
        None
  in
  match find i.last with
  | Some m ->
      dispatch i m ; settle ()
  | None ->
      failwith ("no button " ^ label)

(* The components of the example on the page. *)
module Counter = struct
  type props = {step: int; label: string}

  type model = int

  type msg = Incr | Same

  type out = Changed of int

  let id : (props, model, msg, out) Component_id.t = Component_id.make ()

  let name = "Counter"

  let init _ = (0, Cmd.none)

  let update ctx p n = function
    | Incr ->
        let n = n + p.step in
        (n, Ctx.emit ctx (Changed n))
    | Same ->
        (n, Cmd.none)

  let view p n =
    El
      ( "div"
      , [ Text p.label
        ; Button ("+", Incr)
        ; Button ("same", Same)
        ; Text (string_of_int n) ] )
end

module App = struct
  type props = unit

  type model = {total: int; resets: int; label: string}

  type msg = Count of int | Reset | Relabel | Rerender

  type out = |

  let id : (props, model, msg, out) Component_id.t = Component_id.make ()

  let name = "App"

  let init () = ({total= 0; resets= 0; label= "Left"}, Cmd.none)

  let update _ _ m = function
    | Count n ->
        ({m with total= n}, Cmd.none)
    | Reset ->
        ({m with total= 0; resets= m.resets + 1}, Cmd.none)
    | Relabel ->
        ({m with label= "Links"}, Cmd.none)
    | Rerender ->
        ({m with total= m.total}, Cmd.none)

  let view () m =
    El
      ( "main"
      , [ v
            (module Counter)
            ~key:("left/" ^ string_of_int m.resets)
            ~on:(fun (Counter.Changed n) -> Count n)
            {Counter.step= 1; label= m.label}
        ; v
            (module Counter)
            ~on:(fun (Counter.Changed n) -> Count n)
            {Counter.step= 10; label= "Right"}
        ; Button ("reset", Reset)
        ; Button ("relabel", Relabel)
        ; Button ("rerender", Rerender) ] )
end

let failures = ref 0

let check name ok =
  print_endline ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

let kid : type p m msg o.
       (p, m, msg, o) inst
    -> int
    -> (Counter.props, Counter.model, Counter.msg, Counter.out) inst =
 fun i n ->
  let (Inst k) = List.nth i.kids n in
  match Type.Id.provably_equal k.spec.id Counter.id with
  | Some Type.Equal ->
      k
  | None ->
      failwith ("not a Counter: " ^ k.spec.name)

let () =
  let app =
    mount
      { id= App.id
      ; name= App.name
      ; init= App.init
      ; update= App.update
      ; view= App.view } None () (fun _ -> () )
  in
  check "mount: one App and two Counters"
    (!inits = 3 && List.length app.kids = 2) ;
  let left = kid app 0 in
  click left "+" ;
  check "an output goes to the parent through ~on"
    (app.model.total = 1 && left.model = 1) ;
  let views_before = !views in
  click app "rerender" ;
  check "equal props: the children do not render" (!views = views_before + 1) ;
  let left_views = !views in
  click app "relabel" ;
  check "new props: the child renders"
    ((kid app 0).props.label = "Links" && !views = left_views + 2) ;
  let v0 = !views in
  click (kid app 0) "same" ;
  check "update gives the same model: no render" (!views = v0) ;
  let right = kid app 1 in
  click right "+" ;
  let inits_before = !inits in
  click app "reset" ;
  let left' = kid app 0 in
  check "a key change resets the child"
    ( left' != left && left'.model = 0
    && !inits = inits_before + 1
    && not left.alive ) ;
  check "the other child keeps its model"
    (kid app 1 == right && right.model = 10) ;
  if !failures > 0 then exit 1
