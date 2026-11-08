module M = Http_mock_state

type reply = M.reply

let on page ?(delay = Kilim.Span.zero) ?(once = false) meth url r =
  let m = Page.http_mock page in
  M.check_live m ;
  let route = {M.meth; url; reply= r; delay= Kilim.Span.to_ms delay; once} in
  m.routes <- m.routes @ [route]

let on_get page ?delay ?once url r = on page ?delay ?once Kilim_http.Get url r

let on_post page ?delay ?once url r = on page ?delay ?once Kilim_http.Post url r

let requests page = List.rev (Page.http_mock page).requests

let pending page = (Page.http_mock page).pending

let aborted page = (Page.http_mock page).aborted

let unmatched page = List.rev (Page.http_mock page).unmatched

let expect_done page =
  let m = Page.http_mock page in
  if m.pending > 0 then
    failwith (string_of_int m.pending ^ " requests in flight") ;
  match List.rev m.unmatched with
  | [] ->
      ()
  | r :: _ ->
      failwith
        ( "no reply for "
        ^ Kilim_http.string_of_meth r.meth
        ^ " " ^ r.url ^ " and "
        ^ string_of_int (List.length m.unmatched - 1)
        ^ " other requests" )
