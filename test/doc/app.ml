(* The app of the install examples of the four browser libraries. *)

let app : Kilim.App.t =
  Kilim.App.make
    ~init:(fun () -> ((), Kilim.Cmd.none))
    ~update:(fun () () -> ((), Kilim.Cmd.none))
    ~view:(fun () -> Kilim.txt "")
    ()
