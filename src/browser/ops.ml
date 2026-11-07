(* The real Dom.t of one app (design section 11). Each operation is one JS call,
   get or set with a literal name. The record holds the direct listener
   functions of the app: [unlisten] removes the same function. *)

module Dom = Kilim.Private.Dom

let svg_ns = lazy (Jv.of_string "http://www.w3.org/2000/svg")

let make (st : Listen.state) : Jv.t Dom.t =
  let document = Lazy.force Js.document in
  let fns =
    List.map
      (fun name -> (name, Listen.direct st name))
      Kilim.Private.Event.direct_events
  in
  let fn e = List.assoc e fns in
  { create_element=
      (fun ns name ->
        match ns with
        | Dom.Html ->
            Jv.call document "createElement" [|Jv.of_string name|]
        | Dom.Svg ->
            Jv.call document "createElementNS"
              [|Lazy.force svg_ns; Jv.of_string name|] )
  ; create_text= (fun s -> Jv.call document "createTextNode" [|Jv.of_string s|])
  ; set_text= (fun n s -> Jv.set n "nodeValue" (Jv.of_string s))
  ; set_attribute=
      (fun n a v ->
        ignore (Jv.call n "setAttribute" [|Jv.of_string a; Jv.of_string v|]) )
  ; remove_attribute=
      (fun n a -> ignore (Jv.call n "removeAttribute" [|Jv.of_string a|]))
  ; set_property=
      (fun n p ->
        match p with
        | Dom.Value v ->
            Jv.set n "value" (Jv.of_string v)
        | Dom.Checked b ->
            Jv.set n "checked" (Jv.of_bool b) )
  ; get_value= (fun n -> Listen.value_string (Jv.get n "value"))
  ; get_checked= (fun n -> Listen.truthy (Jv.get n "checked"))
  ; insert_before=
      (fun p n r ->
        ignore
          (Jv.call p "insertBefore"
             [|n; (match r with Some r -> r | None -> Jv.null)|] ) )
  ; remove= (fun p n -> ignore (Jv.call p "removeChild" [|n|]))
  ; replace= (fun p n o -> ignore (Jv.call p "replaceChild" [|n; o|]))
  ; clear= (fun n -> Jv.set n "textContent" (Jv.of_string ""))
  ; set_inner_html=
      (fun n h ->
        Jv.set n "innerHTML"
          (Jv.of_string (Kilim.Private.Unsafe_html.to_string h)) )
  ; attach= (fun n c -> Jv.set n "__kilim" (Jv.repr c))
  ; listen=
      (fun n e ~passive ->
        ignore
          (Jv.call n "addEventListener"
             [|Jv.of_string e; fn e; Jv.obj [|("passive", Jv.of_bool passive)|]|] ) )
  ; unlisten=
      (fun n e ->
        ignore (Jv.call n "removeEventListener" [|Jv.of_string e; fn e|]) ) }
