(* DOM operations with literal method names and literal arrays: the compilers
   must change them into direct JS calls. *)
let set_attribute el name value =
  ignore (Jv.call el "setAttribute" [|Jv.of_string name; Jv.of_string value|])

let append_child parent child = ignore (Jv.call parent "appendChild" [|child|])

let set_text el s = Jv.set el "textContent" (Jv.of_string s)

(* A fake element with the same methods, made in JS. *)
let fake_element : unit -> Jv.t =
  let make =
    Jv.get Jv.global "Function"
    |> fun f ->
    Jv.new' f
      [| Jv.of_string
           "return { attrs: {}, kids: [], textContent: '', setAttribute(n, v) \
            { this.attrs[n] = v }, appendChild(c) { this.kids.push(c) } }" |]
  in
  fun () -> Jv.apply make [||]

let now () = Brr.Performance.now_ms Brr.G.performance

let () =
  let root = fake_element () in
  let n = 1_000_000 in
  let el = fake_element () in
  let t0 = now () in
  for i = 1 to n do
    set_attribute el "data-x" (if i land 1 = 0 then "a" else "b")
  done ;
  let t1 = now () in
  for _ = 1 to n do
    set_text el "row"
  done ;
  let t2 = now () in
  for _ = 1 to 100_000 do
    append_child root el
  done ;
  let t3 = now () in
  let ns a b k = (b -. a) *. 1e6 /. float k in
  print_endline ("setAttribute: " ^ string_of_float (ns t0 t1 n) ^ " ns/call") ;
  print_endline ("textContent:  " ^ string_of_float (ns t1 t2 n) ^ " ns/call") ;
  print_endline
    ("appendChild:  " ^ string_of_float (ns t2 t3 100_000) ^ " ns/call") ;
  let attrs = Jv.get el "attrs" in
  let ok =
    Jv.to_string (Jv.get attrs "data-x") = "a"
    && Jv.to_int (Jv.get (Jv.get root "kids") "length") = 100_000
  in
  print_endline
    ( if ok then "ok   the DOM operations wrote the values"
      else "FAIL the DOM operations" ) ;
  if not ok then exit 1
