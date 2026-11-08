(* The positive control of the size check (bench/size/size.mjs): a program that
   uses each binding that the counter does not use. The release files of this
   program contain the names that the files of the counter must not contain. *)

let task =
  Kilim_browser.Async.of_fut_or_error (fun _ ->
      Kilim_web.Es_module.import (Jstr.v "./m.mjs") )

let router =
  Kilim_router.make
    ~parse:(fun _ -> ())
    ~print:(fun () -> {Kilim.Url.path= []; query= []; fragment= None})
    ()

let app =
  Kilim.App.make
    ~init:(fun () ->
      ( 0
      , Kilim.Cmd.batch
          [ Kilim.Cmd.task (Kilim.Async.map (fun _ -> 1) task)
          ; Kilim.Cmd.task
              (Kilim.Async.map (fun () -> 2) (Kilim_router.push router ())) ] ) )
    ~update:(fun _ m -> (m, Kilim.Cmd.none))
    ~view:(fun _ ->
      Kilim.View.unsafe_html ~el:(Kilim.El.div ~at:[])
        (Kilim_browser.Unsafe_html.of_trusted (Jv.get Jv.global "html")) )
    ()

let () =
  Kilim_router_browser.install () ;
  Kilim_browser.run ~mount:"app" app
