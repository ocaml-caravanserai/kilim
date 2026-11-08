(** The history of kilim.router on the virtual host (design section 14).

    Each page sets this mock as the history driver of kilim.router when it
    starts, with the URL ["/"], or the URL of {!set_url} for its host. The mock
    keeps a list of entries and the index of the entry of the URL. The driver of
    the last page that started is the driver of the program: thus tests that use
    the router must not run in parallel domains. A [popstate] event comes as a
    raw macrotask of the host (see {!Host.raw}): run the host, for example with
    {!Page.advance}, to get it. *)

val set_url : Host.t -> string -> unit
(** [set_url host url] makes [url] the URL of the pages of [host], for example
    ["/users/42?tab=a"], with one entry. Call it before the mount. After the
    mount, it changes the URL of the live page with no [popstate] event: the
    other entries go, and the loads, the scrolls and the listeners stay.

    The mock does not resolve a ["."] or [".."] segment, as a browser does. *)

val url : 'out Page.t -> string
(** The URL of the page: the entry at {!index}. *)

val entries : 'out Page.t -> string list
(** The entries of the history, the oldest first. A push removes the entries
    after {!index}, then adds its URL. A replace changes the entry at {!index}.
*)

val index : 'out Page.t -> int
(** The index of the entry of the URL in {!entries}, from 0. *)

val back : 'out Page.t -> unit
(** The back button of the browser. In a raw macrotask, if {!index} is more than
    0, the index decreases by 1, then each [popstate] listener runs. If not,
    nothing occurs. [Kilim_router.back] does the same. *)

val follow :
     'out Page.t
  -> ?button:int
  -> ?alt:bool
  -> ?ctrl:bool
  -> ?meta:bool
  -> ?shift:bool
  -> By.t
  -> unit
(** [follow page q] is a click on a link, with the button and the modifier keys
    of {!Page.click}. If no handler prevented the default, the browser loads the
    URL of the nearest [a] element with a [href], from the target up: {!loads}
    gets the [href]. The page and the history do not change. *)

val loads : 'out Page.t -> string list
(** The URLs of the loads of {!follow}, the oldest first. *)

val listeners : 'out Page.t -> int
(** The live [popstate] listeners, for the leak tests. *)

val scrolls : 'out Page.t -> int
(** The scrolls to the top: one for each push. *)
