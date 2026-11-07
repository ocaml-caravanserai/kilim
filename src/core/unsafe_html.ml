type native = ..

type t = {html: string; native: native option}

let of_string html = {html; native= None}

let of_native html n = {html; native= Some n}

let to_string t = t.html

let native t = t.native
