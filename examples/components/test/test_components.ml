(* The native test of the components example. The test drives the app only
   through the queries and the events of Page, as a user test does. The virtual
   clock of the host runs the debounce of the filter box. Kilim_testing.mount
   tests one component and reads its outputs. *)

open Windtrap
module Page = Kilim_testing.Page
module By = Kilim_testing.By
module Host = Kilim_testing.Host
module Fake_dom = Kilim_testing.Fake_dom

let no_errors p =
  equal int ~msg:"no error" 0 (List.length (Host.errors (Page.host p))) ;
  equal (list string) ~msg:"no warning" [] (Host.warnings (Page.host p))

let text p q = Fake_dom.text (Page.find p q)

let count p label = text p (By.label (label ^ " count"))

let total p = text p (By.label "Total")

let fruits p =
  List.map Fake_dom.text (Fake_dom.children (Page.find p (By.label "Fruits")))

(* True if no element has the text [s]. *)
let absent p s =
  match Page.find p (By.text s) with
  | _ ->
      false
  | exception Page.Query _ ->
      true

let class_of p label =
  match Fake_dom.parent (Page.find p (By.label (label ^ " count"))) with
  | Some div ->
      Fake_dom.attribute div "class"
  | None ->
      None

let first_render () =
  let p = Page.start Components.app in
  equal string "0" (count p "Ones") ;
  equal string "0" (count p "Tens") ;
  equal string "0" (total p) ;
  equal int ~msg:"all fruits" 8 (List.length (fruits p)) ;
  equal string "Clicks: 0" (Fake_dom.text (Page.find p (By.text "Clicks: 0"))) ;
  equal int ~msg:"4 instances below the root" 4 (Page.tree p).instances ;
  no_errors p

let counters () =
  let p = Page.start Components.app in
  Page.click p (By.label "Increment Ones") ;
  Page.click p (By.label "Increment Ones") ;
  Page.click p (By.label "Increment Tens") ;
  Page.click p (By.label "Decrement Ones") ;
  equal string "1" (count p "Ones") ;
  equal string "10" (count p "Tens") ;
  equal string ~msg:"the outputs give the total" "11" (total p) ;
  let old = Page.find p (By.label "Ones count") in
  Page.click p (By.text "Reset") ;
  equal string ~msg:"a new key resets the counter" "0" (count p "Ones") ;
  equal string "0" (count p "Tens") ;
  equal string "0" (total p) ;
  is_false ~msg:"a new instance" (Page.find p (By.label "Ones count") == old) ;
  Page.click p (By.label "Increment Tens") ;
  equal string "10" (total p) ;
  no_errors p

(* The result of the timer renders at the next frame, a multiple of 16 ms. *)
let debounce () =
  let p = Page.start Components.app in
  Page.type_ p (By.label "Filter") "ap" ;
  Page.advance p (Kilim.Span.ms 299) ;
  equal int ~msg:"no query before the delay" 8 (List.length (fruits p)) ;
  Page.type_ p (By.label "Filter") "r" ;
  Page.advance p (Kilim.Span.ms 299) ;
  equal int ~msg:"a new input restarts the delay" 8 (List.length (fruits p)) ;
  Page.advance p (Kilim.Span.ms 20) ;
  equal (list string) ["Apricot"] (fruits p) ;
  Page.input p (By.label "Filter") "AN" ;
  Page.advance p (Kilim.Span.ms 320) ;
  equal (list string) ~msg:"the filter ignores the case"
    ["Banana"; "Mango"; "Orange"]
    (fruits p) ;
  Page.stop p ;
  Page.expect_no_tasks p ;
  no_errors p

let theme () =
  let p = Page.start Components.app in
  equal (option string) (Some "counter light") (class_of p "Ones") ;
  Page.click p (By.text "Dark theme") ;
  equal (option string) ~msg:"the readers get the new theme"
    (Some "counter dark") (class_of p "Ones") ;
  equal (option string) (Some "counter dark") (class_of p "Tens") ;
  Page.click p (By.text "Light theme") ;
  equal (option string) (Some "counter light") (class_of p "Tens") ;
  no_errors p

let boundary () =
  let p = Page.start Components.app in
  Page.click p (By.text "Clicks: 0") ;
  Page.click p (By.text "Break") ;
  equal string ~msg:"the fallback" "Retry" (text p (By.text "Retry")) ;
  is_true ~msg:"no panel" (absent p "Break") ;
  equal int ~msg:"one catch" 1 (Page.stats p).caught ;
  Page.click p (By.label "Increment Ones") ;
  equal string ~msg:"the rest of the app works" "1" (total p) ;
  Page.click p (By.text "Retry") ;
  equal string ~msg:"a new panel" "Clicks: 0" (text p (By.text "Clicks: 0")) ;
  no_errors p

let mount_counter () =
  Kilim_testing.mount
    (module Components.Counter)
    {Components.Counter.label= "n"; step= 2}
    (fun p ->
      Page.click p (By.label "Increment n") ;
      Page.click p (By.label "Increment n") ;
      Page.click p (By.label "Decrement n") ;
      equal string "2" (Fake_dom.text (Page.find p (By.label "n count"))) ;
      is_true ~msg:"the outputs"
        (Page.outputs p = Components.Counter.[Changed 2; Changed 4; Changed 2]) )

let mount_filter () =
  Kilim_testing.mount
    (module Components.Filter)
    ()
    (fun p ->
      Page.type_ p (By.label "Filter") "pear" ;
      Page.advance p (Kilim.Span.ms 300) ;
      is_true ~msg:"one output after the last input"
        (Page.outputs p = [Components.Filter.Query "pear"]) )

let () =
  exit
    (Windtrap.run "components"
       [ test "the first render" first_render
       ; test "the counters give their outputs and reset by key" counters
       ; test "the filter box waits for the last input" debounce
       ; test "the counters read the theme" theme
       ; test "the boundary catches the error of the panel" boundary
       ; test "Kilim_testing.mount: the outputs of a counter" mount_counter
       ; test "Kilim_testing.mount: the filter gives one query" mount_filter ] )
