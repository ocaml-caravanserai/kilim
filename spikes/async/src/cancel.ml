type t =
  { mutable cancelled: bool
  ; mutable ended: bool
  ; mutable actions: (unit -> unit) list (* newest first *)
  ; mutable first: t option (* newest child *)
  ; mutable prev: t option
  ; mutable next: t option
  ; mutable parent: t option }

let on_error = ref (fun (_ : exn) -> ())

let set_error_handler f = on_error := f

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
  List.iter (fun f -> try f () with e -> !on_error e) actions

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
  c.prev <- None ;
  c.next <- None

let child p =
  if p.cancelled then make None true
  else begin
    let c = make (Some p) false in
    c.next <- p.first ;
    (match p.first with Some f -> f.prev <- Some c | None -> ()) ;
    p.first <- Some c ;
    c
  end

let release c =
  if (not c.cancelled) && not c.ended then begin
    c.ended <- true ;
    unlink c ;
    run_actions c
  end

type frame = Enter of t | Exit of t

(* An explicit stack: a deep tree of tokens does not overflow the stack. The
   children go first, the newest first, then the actions of the token. *)
let cancel t =
  if not t.cancelled then begin
    unlink t ;
    let stack = ref [Enter t] in
    while !stack <> [] do
      match !stack with
      | [] ->
          ()
      | Enter x :: rest ->
          stack := rest ;
          if not x.cancelled then begin
            x.cancelled <- true ;
            let rec collect acc = function
              | None ->
                  acc
              | Some c ->
                  collect (Enter c :: acc) c.next
            in
            let kids = List.rev (collect [] x.first) in
            x.first <- None ;
            stack := kids @ (Exit x :: !stack)
          end
      | Exit x :: rest ->
          stack := rest ;
          run_actions x
    done
  end

let on_end t f =
  if t.cancelled || t.ended then try f () with e -> !on_error e
  else t.actions <- f :: t.actions
