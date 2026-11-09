(** The driver of kilim.storage on the virtual host (design section 14).

    Each page sets this mock as the driver of kilim.storage when it starts. The
    storage of a host starts empty, or with the values of {!preset}. The driver
    of the last host that started a page is the driver of the program: thus
    tests that use the storage must not run in parallel domains. After
    {!Page.stop}, {!contents} still works while no page of another host started.
*)

val preset : Host.t -> Kilim_storage.area -> (string * string) list -> unit
(** [preset host area items] puts [items] in [area] of the storage of [host].
    Call it before the mount: then an init task gets the values. It removes the
    other keys of [area]. *)

val contents : 'out Page.t -> Kilim_storage.area -> (string * string) list
(** The keys and values of [area], sorted by key.

    Raises [Failure] if a page of another host started after the page. *)

val block : 'out Page.t -> unit
(** Each later call of the driver gives [Error Unavailable], as a browser that
    blocks the storage of the site. *)
