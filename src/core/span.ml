type t = int

let max_ms = 0x3FFFFFFF

let zero = 0

let ms n =
  if n < 0 then invalid_arg "Kilim.Span.ms: a negative span"
  else if n > max_ms then invalid_arg "Kilim.Span.ms: out of range"
  else n

let s n =
  if n < 0 then invalid_arg "Kilim.Span.s: a negative span"
  else if n > max_ms / 1000 then invalid_arg "Kilim.Span.s: out of range"
  else n * 1000

let to_ms t = t

let compare = Int.compare

let equal = Int.equal
