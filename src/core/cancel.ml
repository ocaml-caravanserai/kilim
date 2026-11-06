type t =
  { mutable cancelled: bool
  ; mutable ended: bool
  ; mutable actions: (unit -> unit) list (* the newest first *)
  ; mutable first: t option (* the newest child *)
  ; mutable prev: t option
  ; mutable next: t option
  ; mutable parent: t option }

let make parent cancelled =
  { cancelled
  ; ended= false
  ; actions= []
  ; first= None
  ; prev= None
  ; next= None
  ; parent }

let root () = make None false

let cancelled t = t.cancelled

let run_actions t =
  let actions = t.actions in
  t.actions <- [] ;
  List.iter (fun f -> try f () with e -> Report.error e) actions

(* O(1). A token that has no parent is not in a list. *)
let unlink c =
  ( match c.parent with
  | None ->
      ()
  | Some p -> (
      ( match c.prev with
      | Some q ->
          q.next <- c.next
      | None ->
          p.first <- c.next ) ;
      match c.next with Some q -> q.prev <- c.prev | None -> () ) ) ;
  c.parent <- None ;
  (c.prev <- None) [@mutate off "frees memory: no list holds the token"] ;
  (c.next <- None) [@mutate off "frees memory: no list holds the token"]

(* A child of a cancelled token is cancelled at once and is in no list. *)
let child p =
  if p.cancelled then make None true
  else begin
    let c = make (Some p) false in
    c.next <- p.first ;
    (match p.first with Some f -> f.prev <- Some c | None -> ()) ;
    p.first <- Some c ;
    c
  end

let children t =
  let rec count n = function None -> n | Some c -> count (n + 1) c.next in
  count 0 t.first

(* The token detaches from its parent, is marked as ended, and runs its end
   actions. A second release, or a release after a cancel, finds no parent and
   no action. *)
let release c =
  c.ended <- true ;
  unlink c ;
  run_actions c

type frame = Enter of t | Exit of t

(* An explicit stack: a deep tree of tokens does not overflow the stack. The
   children go first, the newest first, then the actions of the token. A cancel
   of an ended token sets its cancelled flag. A linked child is never cancelled,
   and a cancelled token has no child and no action, thus a cancel of a
   cancelled token changes nothing. *)
let cancel t =
  unlink t ;
  let rec loop = function
    | [] ->
        ()
    | Enter x :: rest ->
        x.cancelled <- true ;
        (* The children leave the list: a later release of a child then finds no
           parent. *)
        let rec collect acc = function
          | None ->
              acc
          | Some c ->
              let next = c.next in
              c.parent <- None ;
              collect (Enter c :: acc) next
        in
        let kids = List.rev (collect [] x.first) in
        (x.first <-
          None )
        [@mutate off "frees memory: a cancelled token gets no child"] ;
        loop (kids @ (Exit x :: rest))
    | Exit x :: rest ->
        run_actions x ; loop rest
  in
  loop [Enter t]

let on_end t f =
  if t.cancelled || t.ended then try f () with e -> Report.error e
  else t.actions <- f :: t.actions
