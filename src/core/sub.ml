type 'p kind = 'p Type.Id.t

let kind () = Type.Id.make ()

type 'msg t =
  | None
  | Batch of 'msg t list
  | Run of string * (('msg -> unit) -> unit Async.t)
  | Auto :
      { kind: 'p kind
      ; params: string
      ; start: ('p -> unit) -> unit Async.t
      ; on: 'p -> 'msg }
      -> 'msg t

let auto kind ~params ~start on = Auto {kind; params; start; on}

(* A fixed delay: the next timer starts after the delivery of the last one. *)
let rec loop span deliver =
  Async.bind (Async.sleep span) (fun () -> deliver () ; loop span deliver)

let every_kind : unit kind = kind ()

let every span msg =
  if Span.to_ms span = 0 then invalid_arg "Kilim.Sub.every: a zero span"
  else
    auto every_kind
      ~params:(string_of_int (Span.to_ms span))
      ~start:(loop span)
      (fun () -> msg)

module Key = struct
  type auto = {uid: int; params: string; nth: int}

  type t = Run of string | Auto of auto

  let compare a b =
    match (a, b) with
    | Run x, Run y ->
        String.compare x y
    | Run _, Auto _ ->
        -1
    | Auto _, Run _ ->
        1
    | Auto a, Auto b ->
        let c = Int.compare a.uid b.uid in
        if c <> 0 then c
        else
          let c = String.compare a.params b.params in
          if c <> 0 then c else Int.compare a.nth b.nth
end

type 'msg leaf =
  | Run_leaf of string * (('msg -> unit) -> unit Async.t)
  | Auto_leaf :
      Key.auto * 'p kind * (('p -> unit) -> unit Async.t) * ('p -> 'msg)
      -> 'msg leaf

module Keys = Set.Make (String)

(* The count of the leaves of each auto kind and params. *)
module Counts = Map.Make (struct
  type t = int * string

  let compare (a, p) (b, q) =
    let c = Int.compare a b in
    if c <> 0 then c else String.compare p q
end)

let leaves (s : 'msg t) : 'msg leaf list =
  let rec loop acc runs counts = function
    | [] ->
        List.rev acc
    | None :: stack ->
        loop acc runs counts stack
    | Batch l :: stack ->
        loop acc runs counts (l @ stack)
    | Run (k, f) :: stack ->
        if Keys.mem k runs then
          invalid_arg ("Kilim.Sub.run: the duplicate key " ^ k)
        else loop (Run_leaf (k, f) :: acc) (Keys.add k runs) counts stack
    | Auto {kind; params; start; on} :: stack ->
        let uid = Type.Id.uid kind in
        let nth =
          Option.value ~default:0 (Counts.find_opt (uid, params) counts)
        in
        let key = {Key.uid; params; nth} in
        loop
          (Auto_leaf (key, kind, start, on) :: acc)
          runs
          (Counts.add (uid, params) (nth + 1) counts)
          stack
  in
  loop [] Keys.empty Counts.empty [s]
