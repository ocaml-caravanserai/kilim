(* The markers of the template. *)

open Windtrap
open Support
module T = Kilim_dist.Template

let fill ?name page = T.fill ~name ~head:"H" ~start:"S" page

let error_of = function Ok p -> fail ("no error: " ^ p) | Error e -> e

let tests =
  [ group "template"
      [ test "the fill changes no other byte" (fun () ->
            (* Bytes that a parser could change: CRLF, UTF-8, a marker with no
               comment, an empty comment. *)
            let a = "\r\n\xc3\xa9 kilim:head <!----> "
            and b = " <p>\t</p>\r\n" in
            equal (result string string)
              (Ok (a ^ "H" ^ b ^ "S" ^ a))
              (fill (a ^ "<!--kilim:head-->" ^ b ^ "<!--kilim:start-->" ^ a)) ;
            (* The start marker can come first. *)
            equal (result string string) (Ok "S-H")
              (fill "<!--kilim:start-->-<!--kilim:head-->") )
      ; test "a missing marker" (fun () ->
            let e = error_of (fill "<!--kilim:head-->") in
            is_true ~msg:e (contains e "<!--kilim:start--> occurs 0 times") ;
            let e = error_of (fill "<!--kilim:start-->") in
            is_true ~msg:e (contains e "<!--kilim:head--> occurs 0 times") )
      ; test "a marker two times" (fun () ->
            let e =
              error_of
                (fill "<!--kilim:head--><!--kilim:start--><!--kilim:head-->")
            in
            is_true ~msg:e (contains e "<!--kilim:head--> occurs 2 times") ;
            let e =
              error_of
                (fill "<!--kilim:head--><!--kilim:start--><!--kilim:start-->")
            in
            is_true ~msg:e (contains e "<!--kilim:start--> occurs 2 times") )
      ; test "a name changes only its markers" (fun () ->
            let page =
              "<!--kilim:head--><!--kilim:head:x--><!--kilim:start:x--><!--kilim:start-->"
            in
            equal (result string string)
              (Ok "<!--kilim:head-->HS<!--kilim:start-->") (fill ~name:"x" page) ;
            equal (result string string)
              (Ok "H<!--kilim:head:x--><!--kilim:start:x-->S") (fill page) ;
            equal (pair string string)
              ("<!--kilim:head:app-2-->", "<!--kilim:start:app-2-->")
              (T.markers (Some "app-2")) ) ] ]
