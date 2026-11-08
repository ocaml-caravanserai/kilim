module S = History_state

let set_url = S.set_url

let state page = S.of_host (Page.host page)

let url page = S.entry (state page)

let entries page = (state page).entries

let index page = (state page).index

let back page = S.back (state page)

(* The nearest a element with a href, from [n] up. *)
let rec link n =
  match (Fake_dom.name n, Fake_dom.attribute n "href") with
  | "a", Some href ->
      Some href
  | _ ->
      Option.bind (Fake_dom.parent n) link

let follow page ?button ?alt ?ctrl ?meta ?shift q =
  let s = state page in
  let target = Page.find page q in
  Page.click page ?button ?alt ?ctrl ?meta ?shift (By.node target) ;
  if not (Page.prevented page) then
    match link target with
    | Some href ->
        s.loads <- href :: s.loads
    | None ->
        ()

let loads page = List.rev (state page).loads

let scrolls page = (state page).scrolls

let listeners page = Kilim.Private.Dlist.length (state page).listeners
