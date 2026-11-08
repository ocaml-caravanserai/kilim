(* A program that uses one module of kilim.web: Microtask. kilim.web has no main
   module, thus the program links no other unit of kilim.web. The program never
   runs. *)

let () = if Array.length Sys.argv > 1000 then Kilim_web.Microtask.queue Jv.null
