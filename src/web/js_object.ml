(* The global [Object], read at the first call: the module initialization reads
   no global. *)
let object_ = lazy (Jv.get Jv.global "Object")

let is a b = Jv.to_bool (Jv.call (Lazy.force object_) "is" [|a; b|])
