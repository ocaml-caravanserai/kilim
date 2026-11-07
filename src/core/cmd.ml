type 'msg t =
  | None
  | Batch of 'msg t list
  | Task of 'msg Async.t
  | Latest of string * 'msg Async.t
  | Map : string option * ('a -> 'msg) * 'a t -> 'msg t

type 'msg request =
  | Request :
      {key: string list option; task: 'a Async.t; to_msg: 'a -> 'msg}
      -> 'msg request

(* The functions of the nested maps, inner first. [apply] is a loop, thus a deep
   nest of maps does not overflow the stack under js_of_ocaml. *)
type ('a, 'msg) maps =
  | Id : ('msg, 'msg) maps
  | Then : ('a -> 'b) * ('b, 'msg) maps -> ('a, 'msg) maps

let rec apply : type a msg. (a, msg) maps -> a -> msg =
 fun m x -> match m with Id -> x | Then (f, rest) -> apply rest (f x)

(* A command that waits on the stack, with the scopes above it (the innermost
   first) and the maps above it. *)
type 'msg frame = Frame : 'a t * string list * ('a, 'msg) maps -> 'msg frame

let requests (c : 'msg t) : 'msg request list =
  let rec loop acc = function
    | [] ->
        List.rev acc
    | Frame (c, scopes, maps) :: stack -> (
      match c with
      | None ->
          loop acc stack
      | Batch l ->
          let stack =
            List.fold_left
              (fun stack c -> Frame (c, scopes, maps) :: stack)
              stack (List.rev l)
          in
          loop acc stack
      | Task task ->
          loop
            (Request {key= Option.None; task; to_msg= apply maps} :: acc)
            stack
      | Latest (k, task) ->
          let key = Some (List.rev (k :: scopes)) in
          loop (Request {key; task; to_msg= apply maps} :: acc) stack
      | Map (scope, f, c) ->
          let scopes =
            match scope with Some s -> s :: scopes | Option.None -> scopes
          in
          loop acc (Frame (c, scopes, Then (f, maps)) :: stack) )
  in
  loop [] [Frame (c, [], Id)]
