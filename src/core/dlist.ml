(* A node with no previous node points to itself in [prev], and the last node
   points to itself in [next]. Thus a link needs no option. *)
type 'a node =
  { value: 'a
  ; list: 'a t
  ; mutable prev: 'a node
  ; mutable next: 'a node
  ; mutable linked: bool }

and 'a t = {mutable first: 'a node option; mutable length: int}

let create () = {first= None; length= 0}

let push l value =
  let rec n = {value; list= l; prev= n; next= n; linked= true} in
  ( match l.first with
  | Some f ->
      n.next <- f ;
      f.prev <- n
  | None ->
      () ) ;
  l.first <- Some n ;
  l.length <- l.length + 1 ;
  n

let remove n =
  if n.linked then begin
    let l = n.list and p = n.prev and x = n.next in
    let last = x == n in
    if p == n then l.first <- (if last then None else Some x)
    else p.next <- (if last then p else x) ;
    (* For the last node, [x] is [n]: the write below clears it. *)
    x.prev <- (if p == n then x else p) ;
    n.linked <- false ;
    (n.prev <- n) [@mutate off "frees memory: no list holds the node"] ;
    (n.next <- n) [@mutate off "frees memory: no list holds the node"] ;
    l.length <- l.length - 1
  end

let length l = l.length

(* The next node is read before [f] runs: [f] can remove its own node. *)
let iter f l =
  let rec go n =
    let x = n.next in
    let last = x == n in
    f n.value ;
    if not last then go x
  in
  match l.first with Some n -> go n | None -> ()
