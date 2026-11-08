external pure_js_expr : string -> 'a = "caml_pure_js_expr"

(* A JS expression: OCaml code cannot write import(). The compilers put the text
   in the output (no eval), and the dead code elimination removes it from a
   program that does not use it. The catch gives a rejected promise for a
   synchronous throw, for example an invalid URL. *)
let import_js : Jv.t =
  pure_js_expr
    "(function(s){try{var \
     d=globalThis.document;if(d&&(s.startsWith('./')||s.startsWith('../')||s.startsWith('/')))s=new \
     URL(s,d.baseURI).href;return import(s)}catch(e){return \
     Promise.reject(e)}})"

let import spec =
  Fut.of_promise ~ok:(fun v -> v) (Jv.apply import_js [|Jv.of_jstr spec|])
