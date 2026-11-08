(* The URL corpus of Kilim.Url. test/core/test_url.ml checks each case, and U1
   of test/dom/test_web.ml compares the cases with the functions of the
   browsers. *)

module Url = Kilim.Url

(* The texts of encode_component: encodeURIComponent gives the same result. *)
let texts =
  [ ""
  ; "abc"
  ; "a b+&=/?#%"
  ; "\xc3\xa9\xe2\x82\xac\xf0\x9f\x98\x80"
  ; "-_.!~*'()"
  ; "\x00\x1f\x7f"
  ; String.init 128 Char.chr ]

(* decode_component: each input and its result. A bad %XX stays. *)
let decodes =
  [ ("", "")
  ; ("abc", "abc")
  ; ("%41", "A")
  ; ("x%41", "xA")
  ; ("%41x", "Ax")
  ; ("%4a%4A", "JJ")
  ; ("%fa%FA%af", "\xfa\xfa\xaf")
  ; ("%09%0f", "\t\x0f")
  ; ("%", "%")
  ; ("%4", "%4")
  ; ("a%4", "a%4")
  ; ("%%41", "%A")
  ; ("%G1", "%G1")
  ; ("%1G", "%1G")
  ; ("%/1", "%/1")
  ; ("%:1", "%:1")
  ; ("%@1", "%@1")
  ; ("%`1", "%`1")
  ; ("%g1", "%g1")
  ; ("%1g", "%1g")
  ; ("a+b", "a+b")
  ; ("%C3%A9", "é")
  ; ("%FF", "\xff") ]

(* The inputs that decodeURIComponent accepts, with the result. *)
let escapes =
  [ ("%41", "A")
  ; ("%c3%a9", "\xc3\xa9")
  ; ("%C3%A9%e2%82%AC", "\xc3\xa9\xe2\x82\xac")
  ; ("a%2Fb", "a/b")
  ; ("%25%2B+", "%++")
  ; ("x", "x") ]

(* query: each input and its pairs, as URLSearchParams gives them. *)
let queries =
  [ ("", [])
  ; ("?", [])
  ; ("??a=1", [("?a", "1")])
  ; ("a=1", [("a", "1")])
  ; ("?a=1&b=2", [("a", "1"); ("b", "2")])
  ; ("a=1&a=2", [("a", "1"); ("a", "2")])
  ; ("a", [("a", "")])
  ; ("a=", [("a", "")])
  ; ("=1", [("", "1")])
  ; ("=", [("", "")])
  ; ("&&a=1&&", [("a", "1")])
  ; ("a=1=2", [("a", "1=2")])
  ; ("a+b=c+d", [("a b", "c d")])
  ; ("a%2Bb=%2B", [("a+b", "+")])
  ; ("q=%C3%A9", [("q", "é")])
  ; ("q=%FF", [("q", "\xef\xbf\xbd")])
  ; ("q=%C3", [("q", "\xef\xbf\xbd")])
  ; ("q=%E2%82", [("q", "\xef\xbf\xbd")])
  ; ("q=%F0%9F%98%80x", [("q", "😀x")])
  ; ("q=%C3%28", [("q", "\xef\xbf\xbd(")])
  ; ("q=%", [("q", "%")])
  ; ("q=%zz&r", [("q", "%zz"); ("r", "")])
  ; ("a#b=1", [("a#b", "1")]) ]

(* with_query: the URL, the pairs and the result. *)
let with_queries =
  [ ("/s", [], "/s")
  ; ("/s?a=1", [], "/s?a=1")
  ; ("/s", [("q", "a b")], "/s?q=a%20b")
  ; ("/s", [("q", "1"); ("r", "&=")], "/s?q=1&r=%26%3D")
  ; ("/s?a=1", [("q", "2")], "/s?a=1&q=2")
  ; ("/s?", [("q", "2")], "/s?q=2")
  ; ("/s?a=1&", [("q", "2")], "/s?a=1&q=2")
  ; ("/s#f", [("q", "2")], "/s?q=2#f")
  ; ("/s?a#f?x", [("q", "2")], "/s?a&q=2#f?x")
  ; ("", [("é", "+")], "?%C3%A9=%2B")
  ; ("/s", [("", "")], "/s?=") ]

(* parse: each input and its path, query and fragment. *)
let parses =
  [ ("", ([], [], None))
  ; ("/", ([], [], None))
  ; ("/a", (["a"], [], None))
  ; ("a/b", (["a"; "b"], [], None))
  ; ("/a/", (["a"; ""], [], None))
  ; ("//a", ([""; "a"], [], None))
  ; ("/users/42", (["users"; "42"], [], None))
  ; ("/a%2Fb/c%20d", (["a/b"; "c d"], [], None))
  ; ("/a+b", (["a+b"], [], None))
  ; ("/s?q=a+b&r", (["s"], [("q", "a b"); ("r", "")], None))
  ; ("/s??q", (["s"], [("?q", "")], None))
  ; ("/s?", (["s"], [], None))
  ; ("/s#", (["s"], [], Some ""))
  ; ("/s#top%20x", (["s"], [], Some "top x"))
  ; ("/s#a?b#c", (["s"], [], Some "a?b#c"))
  ; ("/s?q=1#f", (["s"], [("q", "1")], Some "f"))
  ; ("?q=1", ([], [("q", "1")], None))
  ; ("#f", ([], [], Some "f"))
  ; ("/.//a", ([""; "a"], [], None))
  ; ("/./", ([""], [], None))
  ; ("/./a", (["."; "a"], [], None))
  ; ("/.", (["."], [], None)) ]

(* to_string: each location and its URL. *)
let prints =
  [ ({Url.path= []; query= []; fragment= None}, "/")
  ; ({Url.path= [""]; query= []; fragment= None}, "/")
  ; ({Url.path= ["a"; ""]; query= []; fragment= None}, "/a/")
  ; ({Url.path= ["a/b"; "c d"]; query= []; fragment= None}, "/a%2Fb/c%20d")
  ; ( {Url.path= ["s"]; query= [("q", "a b"); ("r", "")]; fragment= None}
    , "/s?q=a%20b&r=" )
  ; ({Url.path= ["s"]; query= []; fragment= Some ""}, "/s#")
  ; ({Url.path= []; query= [("q", "1")]; fragment= Some "a#b"}, "/?q=1#a%23b")
  ; ( {Url.path= [""; "evil.example"; "x"]; query= []; fragment= None}
    , "/.//evil.example/x" )
  ; ({Url.path= [""; ""]; query= []; fragment= None}, "/.//")
  ; ({Url.path= ["."; "x"]; query= []; fragment= None}, "/./x")
  ; ({Url.path= ["a"; ""; "b"]; query= []; fragment= None}, "/a//b") ]
