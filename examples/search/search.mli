(** The search example: a search box with a debounce, a cancel and the query in
    the URL, with kilim.http and kilim.router. *)

val router : string Kilim_router.t
(** The route is the query of the URL: ["/?q=par"] is ["par"]. *)

(** The search box of design section 09. Each key press starts a request 250 ms
    later, and cancels the request before it. After each result, the box puts
    the query in the URL. At the start and at each change of the URL (for
    example the back button), the box gets the query of the URL. A click on a
    city gives the output [Picked city]. If a request fails, the box shows "The
    search failed." until the next result. *)
module Search : sig
  type props = {endpoint: string}

  type out = Picked of string

  include Kilim.COMPONENT with type props := props and type out := out
end

val app : Kilim.App.t
