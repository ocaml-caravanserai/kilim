(** The components example: two counters, a filter box with a debounce, a theme
    context and a boundary, in one app. *)

type theme = Light | Dark

val theme : theme Kilim.Context.t
(** The theme of the app. The counters read it. *)

(** A counter. Each change gives the new count. *)
module Counter : sig
  type props = {label: string; step: int}

  type out = Changed of int

  include Kilim.COMPONENT with type props := props and type out := out
end

(** A filter box. It gives the query 300 ms after the last input. *)
module Filter : sig
  type props = unit

  type out = Query of string

  include Kilim.COMPONENT with type props := props and type out := out
end

val app : Kilim.App.t
