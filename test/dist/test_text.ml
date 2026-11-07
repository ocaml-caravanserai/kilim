(* The loader text: its budget and the bytes that an inline script refuses. *)

open Windtrap

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let text = Kilim_dist.Loader_text.text

let tests =
  [ group "loader text"
      [ test "at most 2,560 bytes" (fun () ->
            (* The budget of docs/budgets.md: the page has the text two times in
               the inline form. *)
            is_true
              ~msg:(string_of_int (String.length text))
              (String.length text <= 2560) )
      ; test "only ASCII" (fun () ->
            is_true (String.for_all (fun c -> Char.code c < 128) text) )
      ; test "no comment and no end of a script" (fun () ->
            List.iter
              (fun bad -> is_false ~msg:bad (contains text bad))
              ["//"; "/*"; "</"; "<!--"] )
      ; test "a CSP source expression" (fun () ->
            let c = Kilim_dist.Loader_text.csp in
            is_true ~msg:c
              ( String.length c = 53
              && String.starts_with ~prefix:"'sha256-" c
              && String.ends_with ~suffix:"='" c ) ) ] ]
