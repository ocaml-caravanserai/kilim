(** kilim.storage: localStorage and sessionStorage as tasks (design section 13).

    Each operation is a task with a synchronous result: the task gives its
    result in the turn that starts it. Thus an init task of the App gives the
    stored value before the first render. The app maps the result to a message
    with [Cmd.task].

    In the browser, [main.ml] calls [Kilim_storage_browser.install ()] (the
    library kilim.storage.browser) before [Kilim_browser.run]. In the tests of
    kilim.testing, each page has its [Storage_mock]. With no driver, a task
    fails with [Failure]: the error goes to the error scope of the instance.

    A write of the page gives no event to the page: the browser sends the
    [storage] event only to the other documents of the origin. Version 1 has no
    subscription to these events. *)

type area =
  | Local  (** localStorage: it stays after the browser closes. *)
  | Session
      (** sessionStorage: one tab. It stays after a reload, until the tab
          closes. *)

type error =
  | Unavailable
      (** The page cannot use the storage, for example the user blocks the
          storage of the site (a SecurityError). *)
  | Quota_exceeded  (** A write is larger than the free space. *)
  | Other of string  (** Another error, with the name of the JS error. *)

val error_to_string : error -> string
(** ["unavailable"], ["quota exceeded"] or ["other NAME"]. *)

val get : area -> string -> (string option, error) result Kilim.Async.t
(** [get area key] gives the value of [key], or [None]. *)

val set : area -> string -> string -> (unit, error) result Kilim.Async.t
(** [set area key value] writes [value] at [key]. *)

val remove : area -> string -> (unit, error) result Kilim.Async.t
(** [remove area key] removes [key]. A key that does not exist gives [Ok ()]. *)

(** A driver: the analog of the host of the core. Plain OCaml functions, with no
    type of Brr. Each function gives its result at once. *)
type driver =
  { get: area -> string -> (string option, error) result
  ; set: area -> string -> string -> (unit, error) result
  ; remove: area -> string -> (unit, error) result }

(** The internals for the drivers and the tests. They can change at any time. *)
module Private : sig
  val set_driver : driver option -> unit
  (** [set_driver (Some d)] makes [d] the driver of the program, and
      [set_driver None] removes the driver. *)
end
