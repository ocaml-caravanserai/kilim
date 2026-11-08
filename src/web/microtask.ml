let queue f = ignore (Jv.call Jv.global "queueMicrotask" [|f|])
