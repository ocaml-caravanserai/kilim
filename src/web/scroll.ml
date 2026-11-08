let to_ ?(smooth = false) ~left ~top () =
  let behavior = if smooth then "smooth" else "auto" in
  ignore
    (Jv.call Jv.global "scrollTo"
       [| Jv.obj
            [| ("left", Jv.of_float left)
             ; ("top", Jv.of_float top)
             ; ("behavior", Jv.of_string behavior) |] |] )
