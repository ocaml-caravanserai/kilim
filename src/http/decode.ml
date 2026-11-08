type error = {path: string list; message: string}

type 'a t = Json.t -> ('a, error) result

let decode d v = d v

let decode_string d s =
  match Json.parse s with
  | Ok v ->
      d v
  | Error message ->
      Error {path= []; message}

let error_to_string e =
  match e.path with
  | [] ->
      e.message
  | p ->
      String.concat "." p ^ ": " ^ e.message

let failure message = Error {path= []; message}

let kind : Json.t -> string = function
  | Null ->
      "null"
  | Bool _ ->
      "a boolean"
  | Number _ ->
      "a number"
  | String _ ->
      "a string"
  | Array _ ->
      "an array"
  | Object _ ->
      "an object"

let expected what v = failure ("expected " ^ what ^ ", got " ^ kind v)

(* The error of [r] is inside [segment]. *)
let inside segment = function
  | Ok _ as r ->
      r
  | Error e ->
      Error {e with path= segment :: e.path}

let string : string t = function String s -> Ok s | v -> expected "a string" v

let bool : bool t = function Bool b -> Ok b | v -> expected "a boolean" v

let float : float t = function Number f -> Ok f | v -> expected "a number" v

(* min_int is -2^(n-1), exact as a float. The range is [-2^(n-1), 2^(n-1)). *)
let low = Float.of_int min_int

let int : int t = function
  | Number f when Float.is_integer f && f >= low && f < -.low ->
      Ok (int_of_float f)
  | Number _ ->
      failure "expected an integer in the range of int"
  | v ->
      expected "an integer" v

let null x : 'a t = function Null -> Ok x | v -> expected "null" v

let nullable d : 'a option t = function
  | Null ->
      Ok None
  | v ->
      Result.map Option.some (d v)

let list d : 'a list t = function
  | Array l ->
      let rec go i acc = function
        | [] ->
            Ok (List.rev acc)
        | v :: rest -> (
          match d v with
          | Ok x ->
              go (i + 1) (x :: acc) rest
          | Error _ as r ->
              inside (string_of_int i) r )
      in
      go 0 [] l
  | v ->
      expected "an array" v

(* The last member [name]. *)
let member name l =
  List.fold_left
    (fun acc (k, v) -> if String.equal k name then Some v else acc)
    None l

let field_with name d ~missing : 'a t = function
  | Object l -> (
    match member name l with Some v -> inside name (d v) | None -> missing () )
  | v ->
      expected "an object" v

let field name d =
  field_with name d ~missing:(fun () -> failure ("no member \"" ^ name ^ "\""))

let field_opt name d =
  field_with name
    (fun v -> Result.map Option.some (d v))
    ~missing:(fun () -> Ok None)

let index i d : 'a t = function
  | Array l -> (
    match if i < 0 then None else List.nth_opt l i with
    | Some x ->
        inside (string_of_int i) (d x)
    | None ->
        failure
          ( "no index " ^ string_of_int i ^ " in an array of "
          ^ string_of_int (List.length l) ) )
  | v ->
      expected "an array" v

let value : Json.t t = fun v -> Ok v

let succeed x : 'a t = fun _ -> Ok x

let fail message : 'a t = fun _ -> failure message

let map f d : 'b t = fun v -> Result.map f (d v)

let bind d f : 'b t =
 fun v -> match d v with Ok x -> f x v | Error e -> Error e

let one_of ds : 'a t =
 fun v ->
  let rec go last = function
    | [] ->
        last
    | d :: rest -> (
      match d v with Ok _ as r -> r | Error _ as r -> go r rest )
  in
  go (failure "one_of []") ds

module Syntax = struct
  let ( let+ ) d f = map f d

  let ( and+ ) a b : _ t =
   fun v ->
    match a v with
    | Error e ->
        Error e
    | Ok x -> (
      match b v with Ok y -> Ok (x, y) | Error e -> Error e )
end
