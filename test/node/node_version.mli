val parse : string -> (int * int * int) option
(** [parse s] gives the major, minor and patch numbers of the output of
    [node --version]: the leading digits of the first three parts. For example
    ["v22.3.0"] gives [Some (22, 3, 0)], ["v24.0.0-rc.1"] gives
    [Some (24, 0, 0)] and ["v25.0.0-nightly2026"] gives [Some (25, 0, 0)]. *)
