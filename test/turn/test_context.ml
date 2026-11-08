(* Context (design section 10): the providers, the reads, the registrations of
   the readers, the environment of an instance, and the interaction with the
   boundaries and with the keyed moves. *)

open Windtrap
open Kilim
open Support
module C = Component

exception Boom

let theme : string Context.t = Context.create ~default:"light" ()

let other : int Context.t = Context.create ~default:0 ()

let test_id id = At.data "testid" id

(* A reader: a click adds 1 to its model. With [raises], its view raises when
   its model is not 0. *)
module Reader = struct
  type props = {id: string; raises: bool}

  type model = int

  type msg = Incr

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init p =
    say ("init " ^ p.id) ;
    (0, Cmd.none)

  let update _ _ m Incr = (m + 1, Cmd.none)

  let view p m =
    say ("view " ^ p.id) ;
    Context.read theme (fun t ->
        if p.raises && m > 0 then raise Boom ;
        El.button
          ~at:[test_id p.id]
          ~ev:[Ev.click Incr]
          [txt (p.id ^ "=" ^ t ^ "/" ^ string_of_int m)] )

  let subs _ _ = Sub.none

  let equal_props a b = String.equal a.id b.id && Bool.equal a.raises b.raises
end

let reader ?(raises = false) id =
  C.v (module Reader) ~on:never {Reader.id; raises}

(* A component that does not read. Its props are a label: a new label renders
   it. *)
module Plain = Leaf (struct
  type props = string * string

  let init _ = Cmd.none

  let view (id, label) =
    say ("view " ^ id) ;
    El.span ~at:[test_id id] [txt label]
end)

let plain ?(label = "") id = C.v (module Plain) ~on:never (id, label)

(* A component whose [equal_props] always gives [true]: the render of the parent
   skips it. Its view is the view of its first props. *)
module Wall = struct
  include C.Defaults

  type props = unit View.t

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = ((), Cmd.none)

  let update _ _ () () = ((), Cmd.none)

  let view v () = say "view wall" ; v

  let equal_props _ _ = true
end

(* A component that reads only when its model is odd: a click flips it. *)
module Cond = struct
  include C.Defaults

  type props = unit

  type model = bool

  type msg = Flip

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = (true, Cmd.none)

  let update _ () on Flip = (not on, Cmd.none)

  let view () on =
    say "view cond" ;
    El.button
      ~at:[test_id "cond"]
      ~ev:[Ev.click Flip]
      [(if on then Context.read theme txt else txt "off")]
end

(* A component whose [update] raises. *)
module Bomb = struct
  include C.Defaults

  type props = unit

  type model = unit

  type msg = Go

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = ((), Cmd.none)

  let update _ () () Go = raise Boom

  let view () () = El.button ~at:[test_id "bomb"] ~ev:[Ev.click Go] [txt "bomb"]
end

let bomb () = C.v (module Bomb) ~on:never ()

(* A boundary whose fallback reads the theme. *)
let fb ?(id = "fb") child =
  View.boundary
    ~fallback:(fun _ ->
      say ("fallback " ^ id) ;
      Context.read theme (fun t ->
          El.span ~at:[test_id id] [txt (id ^ " " ^ t)] ) )
    child

(* The registrations: on the instances, and in the reader lists. *)
let regs ?(msg = "registrations") p n =
  equal int ~msg n (tree p).readers ;
  equal int ~msg:(msg ^ " in the reader lists") n (tree p).reader_links

let span id t = El.span ~at:[test_id id] [txt t]

(* [f ()] with the functions of the diff for the contexts not installed: the
   first call of Context.provide or Context.read installs them. *)
let uninstalled f =
  let saved = !Private.Diff.contexts in
  let no () = failwith "not installed" in
  Private.Diff.contexts :=
    { build_provider= (fun _ _ _ -> no ())
    ; patch_provider= (fun _ _ _ _ -> no ())
    ; read= (fun _ _ -> no ()) } ;
  Fun.protect ~finally:(fun () -> Private.Diff.contexts := saved) f

let installs () =
  uninstalled (fun () ->
      let p = page (fun _ -> Context.read theme (fun t -> span "s" t)) in
      equal string ~msg:"a read alone" "light" (text p "s") ;
      no_errors p ) ;
  uninstalled (fun () ->
      let p =
        page (fun m -> Context.provide theme "x" (span "s" (string_of_int m)))
      in
      bump p ;
      equal string ~msg:"a provider alone" "1" (text p "s") ;
      no_errors p )

let default () =
  let p =
    page (fun _ ->
        El.div
          [reader "a"; Context.read other (fun n -> span "n" (string_of_int n))] )
  in
  equal string "a=light/0" (text p "a") ;
  equal string "0" (text p "n") ;
  equal int ~msg:"no provider" 0 (tree p).providers ;
  regs ~msg:"no registration for a default" p 0 ;
  no_errors p

let nearest () =
  let p =
    page (fun _ ->
        Context.provide theme "outer"
          (El.div
             [ Context.provide theme "inner" (reader "a")
             ; Context.provide other 5 (reader "b") ] ) )
  in
  equal string "a=inner/0" (text p "a") ;
  equal string ~msg:"through a provider of another context" "b=outer/0"
    (text p "b") ;
  equal int 3 (tree p).providers ;
  regs p 2 ;
  no_errors p

(* The decision of question 10 of the Phase 2 plan: the render of the root
   removes its registrations first, thus the new value marks no reader. *)
let same_view () =
  let p =
    page (fun m ->
        Context.provide theme (string_of_int m)
          (Context.read theme (fun t -> span "s" t)) )
  in
  equal string "0" (text p "s") ;
  regs p 1 ;
  let v = (stats p).views in
  one_render p (fun () -> bump p) ;
  equal string "1" (text p "s") ;
  equal int ~msg:"one view" (v + 1) (stats p).views ;
  regs p 1 ;
  equal int 0 (tree p).queued ;
  quiet p

(* The same rule for a component: a render by its parent, and a render alone. *)
module Self = struct
  include C.Defaults

  type props = int

  type model = int

  type msg = Incr

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update _ _ m Incr = (m + 1, Cmd.none)

  let view n m =
    say "view self" ;
    Context.provide theme
      (string_of_int (n + m))
      (Context.read theme (fun t ->
           El.button ~at:[test_id "self"] ~ev:[Ev.click Incr] [txt t] ) )

  let equal_props = Int.equal
end

let same_view_child () =
  let p = page (fun m -> C.v (module Self) ~on:never m) in
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"a render by the parent" ["view self"] (take ()) ;
  P.click p (By.test_id "self") ;
  equal (list string) ~msg:"a render alone" ["view self"] (take ()) ;
  equal string "2" (text p "self") ;
  regs p 1 ;
  quiet p

(* A provider in the view of a component that renders alone: the work queue
   renders the reader below it in the same render. *)
module Box = struct
  include C.Defaults

  type props = unit

  type model = int

  type msg = Incr

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = (0, Cmd.none)

  let update _ () m Incr = (m + 1, Cmd.none)

  let view () m =
    Context.provide theme (string_of_int m)
      (El.div
         [ El.button ~at:[test_id "box"] ~ev:[Ev.click Incr] [txt "+"]
         ; C.v (module Wall) ~on:never (reader "a") ] )
end

let alone_marks () =
  let p = page (fun _ -> C.v (module Box) ~on:never ()) in
  ignore (take ()) ;
  one_render p (fun () -> P.click p (By.test_id "box")) ;
  equal (list string) ["view a"] (take ()) ;
  equal string "a=1/0" (text p "a") ;
  regs p 1 ;
  quiet p

(* A new value renders the readers, in the same render, and no other component.
   The parent of the readers skips them: their props do not change. *)
let only_readers () =
  let p =
    page (fun m ->
        Context.provide theme
          (if m > 0 then "dark" else "light")
          (El.div [reader "a"; plain "q"; reader "b"]) )
  in
  ignore (take ()) ;
  let v = (stats p).views in
  one_render p (fun () -> bump p) ;
  equal (list string) ["view a"; "view b"] (take ()) ;
  equal int ~msg:"the root and the two readers" (v + 3) (stats p).views ;
  equal string "a=dark/0" (text p "a") ;
  equal string "b=dark/0" (text p "b") ;
  bump p ;
  equal (list string) ~msg:"the same value" [] (take ()) ;
  regs p 2 ;
  quiet p

(* A reader that a render marks renders in the same render. The parent renders a
   marked child in its diff, in the order of the document, and the work queue
   does not render it again. *)
let marked_in_diff () =
  let p =
    page (fun m ->
        Context.provide theme (string_of_int m)
          (El.div [reader "a"; plain ~label:(string_of_int m) "q"]) )
  in
  ignore (take ()) ;
  one_render p (fun () -> bump p) ;
  equal (list string) ["view a"; "view q"] (take ()) ;
  equal string "a=1/0" (text p "a") ;
  equal int 0 (tree p).queued ;
  quiet p

let below_skip () =
  let p =
    page (fun m ->
        Context.provide theme (string_of_int m)
          (C.v (module Wall) ~on:never (El.div [reader "a"])) )
  in
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"only the reader" ["view a"] (take ()) ;
  equal string "a=1/0" (text p "a") ;
  quiet p

(* A record value, new at each render: Component.shallow_equal compares its
   fields. *)
type look = {name: string; size: int}

let look : look Context.t = Context.create ~default:{name= ""; size= 0} ()

let shallow () =
  let p =
    page (fun m ->
        Context.provide look
          {name= "dark"; size= m / 2}
          (C.v
             (module Wall)
             ~on:never
             (Context.read look (fun l -> span "l" (string_of_int l.size))) ) )
  in
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"an equal record" [] (take ()) ;
  equal string "0" (text p "l") ;
  bump p ;
  equal (list string) ~msg:"a new size" ["view wall"] (take ()) ;
  equal string "1" (text p "l") ;
  quiet p

(* Rows with a provider each move: the instances stay, and keep their
   providers. *)
let keyed_move () =
  let order m = if m mod 2 = 0 then [1; 2] else [2; 1] in
  let p =
    page (fun m ->
        View.keyed El.ul
          (List.map
             (fun k ->
               let id = string_of_int k in
               ( Key.int k
               , Context.provide theme
                   (id ^ "." ^ string_of_int (m / 2))
                   (reader id) ) )
             (order m) ) )
  in
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"no mount, no view" [] (take ()) ;
  let ul = List.nth (F.children (List.hd (F.children (P.root p)))) 1 in
  equal (list string) ["2=2.0/0"; "1=1.0/0"] (List.map F.text (F.children ul)) ;
  regs p 2 ;
  bump p ;
  equal (list string) ~msg:"each moved provider marks its reader"
    ["view 1"; "view 2"] (take ()) ;
  equal string "1=1.1/0" (text p "1") ;
  equal string "2=2.1/0" (text p "2") ;
  quiet p

let unmounts () =
  let p =
    page (fun m ->
        if m = 0 then
          Context.provide theme "x" (El.div [reader "a"; reader "b"])
        else txt "" )
  in
  regs p 2 ;
  bump p ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).providers ;
  regs p 0 ;
  quiet p ;
  (* A reader that unmounts below a provider that stays. *)
  let p =
    page (fun m ->
        Context.provide theme "x" (if m = 0 then reader "a" else txt "") )
  in
  regs p 1 ;
  bump p ;
  equal int 1 (tree p).providers ;
  regs p 0 ;
  quiet p

(* A provider that appears or disappears at a place, or a new context at the
   place, remounts the subtree. *)
let remounts () =
  let p =
    page (fun m ->
        El.div
          [ ( match m with
            | 1 ->
                Context.provide theme "x" (reader "a")
            | 3 ->
                Context.provide other 1 (reader "a")
            | _ ->
                reader "a" ) ] )
  in
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"appears" ["init a"; "view a"] (take ()) ;
  equal string "a=x/0" (text p "a") ;
  bump p ;
  equal (list string) ~msg:"disappears" ["init a"; "view a"] (take ()) ;
  equal string "a=light/0" (text p "a") ;
  bump p ;
  equal (list string) ~msg:"another node kind" ["init a"; "view a"] (take ()) ;
  let p =
    page (fun m ->
        if m = 0 then Context.provide theme "x" (reader "a")
        else Context.provide other 1 (reader "a") )
  in
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"another context" ["init a"; "view a"] (take ()) ;
  equal string "a=light/0" (text p "a") ;
  equal int 1 (tree p).instances ;
  regs p 0 ;
  quiet p

(* A conditional read: a render with no read removes the registration. *)
let conditional () =
  let p =
    page (fun m ->
        Context.provide theme (string_of_int m) (C.v (module Cond) ~on:never ()) )
  in
  regs p 1 ;
  P.click p (By.test_id "cond") ;
  regs p 0 ;
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"no reader" [] (take ()) ;
  P.click p (By.test_id "cond") ;
  equal string "1" (text p "cond") ;
  regs p 1 ;
  quiet p

(* A render alone starts from the environment of the mount. *)
let alone () =
  let p = page (fun _ -> Context.provide theme "x" (El.div [reader "a"])) in
  P.click p (By.test_id "a") ;
  equal string "a=x/1" (text p "a") ;
  regs p 1 ;
  quiet p

(* A fallback that a catch builds outside the diff gets the provider of its
   place, also in the view of a component: its read registers the owner of the
   boundary. *)
module Holder = struct
  include C.Defaults

  type props = unit

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = ((), Cmd.none)

  let update _ () () () = ((), Cmd.none)

  let view () () =
    say "view holder" ;
    fb (bomb ())
end

let fallback_place () =
  let p =
    page (fun m -> Context.provide theme (string_of_int m) (fb (bomb ())))
  in
  P.click p (By.test_id "bomb") ;
  equal string "fb 0" (text p "fb") ;
  regs p 1 ;
  bump p ;
  equal string "fb 1" (text p "fb") ;
  let p =
    page (fun m ->
        Context.provide theme
          (string_of_int (m / 2))
          (C.v (module Holder) ~on:never ()) )
  in
  P.click p (By.test_id "bomb") ;
  equal string "fb 0" (text p "fb") ;
  regs ~msg:"the holder reads in its fallback" p 1 ;
  ignore (take ()) ;
  bump p ;
  equal (list string) ~msg:"the same value" [] (take ()) ;
  bump p ;
  equal (list string) ~msg:"a reader in a fallback"
    ["view holder"; "fallback fb"]
    (take ()) ;
  equal string "fb 1" (text p "fb") ;
  no_errors p ;
  quiet p

(* The removal of a provider node of a live owner: the registrations of its
   reads go. *)
let removal () =
  let p =
    page (fun m ->
        El.div
          [ ( if m = 0 then
                Context.provide theme "x"
                  (Context.read theme (fun t -> span "s" t))
              else txt "none" ) ] )
  in
  regs p 1 ;
  bump p ;
  equal int 0 (tree p).providers ;
  regs p 0 ;
  is_true (ins p).alive ;
  quiet p

(* The decision of question 9 of the Phase 2 plan, with decision 3 of the code
   review: two reads in a row with one provider and one boundary give one
   registration. Other reads of one provider can give more registrations, but
   the reader renders one time. *)
let two_reads () =
  let r = Context.read theme (fun t -> span "s" t) in
  let p = page (fun _ -> Context.provide theme "x" (El.div [r; r])) in
  regs ~msg:"two reads in a row" p 1 ;
  let p =
    page (fun _ ->
        Context.provide theme "x"
          (El.div [r; Context.provide theme "y" (El.div [r; r])]) )
  in
  regs ~msg:"two providers" p 2 ;
  let p = page (fun _ -> Context.provide theme "x" (El.div [r; fb r])) in
  regs ~msg:"two boundaries" p 2 ;
  quiet p

(* A component whose view has its reads: [n] reads in [n] boundaries, or reads
   of the outer provider before and after a read of an inner provider. *)
module Many = struct
  include C.Defaults

  type props = int

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = ((), Cmd.none)

  let update _ _ () () = ((), Cmd.none)

  let view n () =
    say "view many" ;
    let r = Context.read theme (fun t -> span "s" t) in
    if n = 0 then El.div [r; Context.provide theme "y" r; r]
    else
      El.div (List.init n (fun _ -> View.boundary ~fallback:(fun _ -> txt "") r))
end

let many_reads () =
  List.iter
    (fun (n, count) ->
      let p =
        page (fun m ->
            Context.provide theme (string_of_int m)
              (C.v (module Many) ~on:never n) )
      in
      regs p count ;
      ignore (take ()) ;
      one_render p (fun () -> bump p) ;
      equal (list string) ~msg:"one view of the owner" ["view many"] (take ()) ;
      regs p count ;
      quiet p )
    [(0, 3); (100, 100)]

(* Decision 10 of the plan review: a read function runs in the diff. Its error
   goes to the nearest boundary, or to the root catch. *)
let read_raises () =
  let reads m =
    Context.read theme (fun t -> if m > 0 then raise Boom else span "s" t)
  in
  let p = page (fun m -> fb (reads m)) in
  bump p ;
  equal string "fb light" (text p "fb") ;
  equal int 1 (stats p).caught ;
  no_errors p ;
  (* In the render of a reader alone. *)
  let p = page (fun _ -> fb (reader ~raises:true "a")) in
  P.click p (By.test_id "a") ;
  equal string "fb light" (text p "fb") ;
  equal int 0 (tree p).instances ;
  regs ~msg:"the default in the fallback" p 0 ;
  no_errors p ;
  (* With no boundary: the root catch. *)
  let p = page ~fallback:(fun _ -> txt "failed") (fun m -> El.div [reads m]) in
  bump p ;
  equal string "failed" (F.text (P.root p)) ;
  is_false (ins p).alive ;
  equal int 1 (List.length (errors p)) ;
  regs p 0

(* A provider in a boundary that fails: the owner keeps no registration of a
   read in the failed subtree. *)
let failed_subtree () =
  (* A catch in the diff, at a patch. *)
  let p =
    page (fun m ->
        fb
          (Context.provide theme "x"
             (El.div
                [ Context.read theme (fun t -> span "s" t)
                ; Context.read theme (fun t ->
                      if m > 0 then raise Boom else txt t ) ] ) ) )
  in
  regs p 1 ;
  bump p ;
  equal string "fb light" (text p "fb") ;
  regs ~msg:"no registration after a patch" p 0 ;
  equal int 0 (tree p).providers ;
  (* A catch in the diff, at the build: the provider is not in the tree. *)
  let p =
    page (fun _ ->
        fb
          (Context.provide theme "x"
             (El.div
                [ Context.read theme (fun t -> span "s" t)
                ; Context.read theme (fun _ -> raise Boom) ] ) ) )
  in
  equal string "fb light" (text p "fb") ;
  regs ~msg:"no registration after a build" p 0 ;
  (* A catch outside the diff, and a read in a nested boundary: the catch
     removes the nested boundary. *)
  let p =
    page (fun _ ->
        Context.provide theme "x"
          (fb
             (El.div
                [ Context.read theme (fun t -> span "s" t)
                ; fb ~id:"inner" (Context.read theme (fun t -> span "t" t))
                ; bomb () ] ) ) )
  in
  regs p 2 ;
  P.click p (By.test_id "bomb") ;
  equal string "fb x" (text p "fb") ;
  regs ~msg:"only the read of the fallback" p 1 ;
  no_errors p ;
  quiet p

(* Where the catch of [Owner] comes: outside the diff (an [update] raises), in
   the diff of a render alone, or in the build of the mount. *)
type trigger = Outside | Diff | Build

(* An owner with a read in a boundary of its view. With [inner], the provider is
   in the boundary too: B (P (read)). If not, the provider is in the view of the
   parent: P above B (read). The fallback does not read. A click on "o" adds 1
   to the model. *)
module Owner = struct
  include C.Defaults

  type props = {trigger: trigger; inner: bool}

  type model = int

  type msg = Incr

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update _ _ m Incr = (m + 1, Cmd.none)

  let view p m =
    say "view o" ;
    let trigger =
      match p.trigger with
      | Outside ->
          bomb ()
      | Diff ->
          Context.read theme (fun _ -> if m > 0 then raise Boom else txt "")
      | Build ->
          Context.read theme (fun _ -> raise Boom)
    in
    let body = El.div [Context.read theme (fun t -> span "s" t); trigger] in
    El.div
      [ El.button ~at:[test_id "o"] ~ev:[Ev.click Incr] [txt "+"]
      ; View.boundary
          ~fallback:(fun _ -> span "fb" "failed")
          (if p.inner then Context.provide theme "x" body else body) ]
end

(* After the catch, no registration of the owner stays, and a new value of the
   provider of the parent does not render the owner. *)
let owner_catch () =
  List.iter
    (fun (trigger, inner) ->
      let p =
        page (fun m ->
            Context.provide theme (string_of_int m)
              (C.v (module Owner) ~on:never {Owner.trigger; inner}) )
      in
      ( match trigger with
      | Outside ->
          regs ~msg:"before" p 1 ;
          P.click p (By.test_id "bomb")
      | Diff ->
          regs ~msg:"before" p 1 ;
          P.click p (By.test_id "o")
      | Build ->
          () ) ;
      equal string "failed" (text p "fb") ;
      regs ~msg:"after the catch" p 0 ;
      equal int ~msg:"the provider of the parent" 1 (tree p).providers ;
      ignore (take ()) ;
      bump p ;
      equal (list string) ~msg:"a new value does not render the owner" []
        (take ()) ;
      equal int 1 (tree p).instances ;
      no_errors p ;
      quiet p )
    [ (Outside, false)
    ; (Diff, false)
    ; (Build, false)
    ; (Outside, true)
    ; (Diff, true)
    ; (Build, true) ]

(* A catch keeps the registrations of the reads outside its subtree: in another
   boundary, and in no boundary. *)
let kept () =
  let read id = Context.read theme (fun t -> span id t) in
  let p =
    page (fun _ ->
        Context.provide theme "x"
          (El.div
             [ fb ~id:"other" (read "s")
             ; fb (El.div [read "t"; bomb ()])
             ; read "u" ] ) )
  in
  regs p 3 ;
  P.click p (By.test_id "bomb") ;
  regs ~msg:"the other boundary, and the outside with the fallback" p 2 ;
  is_false ~msg:"the read in the subtree" (has p "t") ;
  equal string "fb x" (text p "fb") ;
  no_errors p ;
  quiet p

(* A component of the root fallback is dead, and the root is dead: no read
   registers. *)
let dead_reads () =
  let p =
    page
      ~fallback:(fun _ ->
        Context.provide theme "x"
          (El.div [Context.read theme (fun t -> span "s" t); reader "a"]) )
      (fun _ -> raise Boom)
  in
  equal string "x" (text p "s") ;
  equal string "a=x/0" (text p "a") ;
  regs p 0 ;
  equal int 1 (List.length (errors p))

let tests =
  [ group "context"
      [ test "a read with no provider gets the default" default
      ; test "Context.read and Context.provide install their functions" installs
      ; test "a read gets the nearest provider" nearest
      ; test "a provider in the same view gives its value to the read" same_view
      ; test "a provider in the view of a component and its read: one view"
          same_view_child
      ; test "a new value renders only the readers" only_readers
      ; test "a reader that a render marks renders in the same render"
          marked_in_diff
      ; test "a provider of a component that renders alone marks a reader"
          alone_marks
      ; test "a reader below a component that equal_props skips still renders"
          below_skip
      ; test "a value equal by shallow_equal marks no reader" shallow
      ; test "a provider in a keyed move keeps its readers" keyed_move
      ; test "a provider that unmounts removes its readers" unmounts
      ; test "a provider or a context that changes at a place remounts" remounts
      ; test
          "a conditional read: a render with no read removes the registration"
          conditional
      ; test "a reader that renders alone uses its recorded environment" alone
      ; test
          "a fallback that a catch builds outside the diff gets the provider \
           of its place"
          fallback_place
      ; test
          "the removal of a provider node of a live owner unlinks its \
           registrations"
          removal
      ; test "two reads in a row of one context give one registration" two_reads
      ; test "many registrations of one owner: one render on a new value"
          many_reads
      ; test "a read function that raises: the boundary, or the root catch"
          read_raises
      ; test "a provider in a boundary that fails leaves no registration"
          failed_subtree
      ; test
          "a catch in the view of a component: no stale registration, no render"
          owner_catch
      ; test "a catch keeps the registrations outside its subtree" kept
      ; test "no read of the root fallback registers" dead_reads ] ]
