(** The test library of kilim (design section 14): a strict fake DOM, a host
    with a virtual clock, queries, pages with simulated events, and the mocks of
    the libraries. It uses only the standard library, kilim and the pure
    libraries, thus it builds also for js_of_ocaml and wasm_of_ocaml. *)

module Fake_dom = Fake_dom
module Host = Host
module By = By
module Page = Page
module History_mock = History_mock

val mount :
  (module C : Kilim.COMPONENT)
  -> ?host:Host.t -> C.props -> (C.out Page.t -> 'a) -> 'a
(** [mount (module C) ~host props f] mounts one instance of the component [C],
    gives its page to [f], stops the page, and gives the result of [f] (see
    {!Page.mount}). {!Page.outputs} gives the outputs of the instance with their
    type:
    {@ocaml in=test/doc/test_doc.ml[
    Kilim_testing.mount
      (module Counter)
      {Counter.step= 1; label= "n"}
      (fun page ->
        Page.click page (By.text "+") ;
        assert (Page.outputs page = [Counter.Changed 1]) )
    ]} *)
