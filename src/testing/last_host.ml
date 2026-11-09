(* "The last host wins" for the state of a mock (design section 14). The slot
   keeps the state of one host: the last host that started a page, as the error
   handler of Host.create. A state of an older host stays readable by its owner,
   but the slot does not give it. *)

type 'a t = {host: 'a -> Host.t; mutable last: 'a option}

let make host = {host; last= None}

let set t s = t.last <- Some s

let clear t = t.last <- None

let current t = t.last

(* The state of [h], if [h] is the last host. *)
let find t h =
  match t.last with
  | Some s when t.host s == h ->
      Some s
  | Some _ | None ->
      None

(* The state of [h]. If [h] is not the last host, [fresh ()] makes the state of
   [h], and [h] becomes the last host. *)
let find_or t h fresh =
  match find t h with
  | Some s ->
      s
  | None ->
      let s = fresh () in
      set t s ; s

(* The state of [h]. If [h] is not the last host, a failure occurs. *)
let get t h ~mock =
  match find t h with
  | Some s ->
      s
  | None ->
      failwith ("Kilim_testing." ^ mock ^ ": the page is not the last page")
