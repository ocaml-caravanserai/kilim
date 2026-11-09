let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

let invalid_arg sub = function
  | Invalid_argument m ->
      contains m sub
  | _ ->
      false

let failure sub = function Failure m -> contains m sub | _ -> false
