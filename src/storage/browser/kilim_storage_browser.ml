(* The driver of kilim.storage on the Web Storage API (design section 13). Each
   call reads the storage object again: the getter of localStorage throws a
   SecurityError when the page cannot use the storage. Each call catches
   Jv.Error, and a storage object that is null or undefined gives Unavailable.
   The calls use Jv, not Brr_io.Storage.set_item and remove_item: set_item does
   not catch the error of the getter, and remove_item catches no error. *)

module S = Kilim_storage

let string_js = Jstr.v "string"

let area = function
  | S.Local ->
      Brr_io.Storage.(to_jv (local Brr.G.window))
  | S.Session ->
      Brr_io.Storage.(to_jv (session Brr.G.window))

let error e =
  match Jstr.to_string (Jv.Error.name e) with
  | "SecurityError" ->
      S.Unavailable
  | "QuotaExceededError" | "NS_ERROR_DOM_QUOTA_REACHED" ->
      S.Quota_exceeded
  | name ->
      S.Other name

(* [f] on the storage object of [a]. *)
let call a f =
  match area a with
  | s when Jv.is_none s ->
      Error S.Unavailable
  | s -> (
    match f s with x -> Ok x | exception Jv.Error e -> Error (error e) )
  | exception Jv.Error e ->
      Error (error e)

(* The trap rule: a read of a JS string examines its type first. *)
let get a key =
  call a (fun s ->
      let v = Jv.call s "getItem" [|Jv.of_string key|] in
      if Jstr.equal (Jv.typeof v) string_js then Some (Jv.to_string v) else None )

let set a key value =
  call a (fun s ->
      ignore (Jv.call s "setItem" [|Jv.of_string key; Jv.of_string value|]) )

let remove a key =
  call a (fun s -> ignore (Jv.call s "removeItem" [|Jv.of_string key|]))

let driver = {S.get; set; remove}

let install () = S.Private.set_driver (Some driver)
