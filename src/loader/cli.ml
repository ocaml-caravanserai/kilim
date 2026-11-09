type error = Usage of string | Fault of string

exception Bad of string

let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

let starts p s = String.starts_with ~prefix:p s

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

(* A scheme: a ":" before the first "/". *)
let scheme s =
  match (String.index_opt s ':', String.index_opt s '/') with
  | Some c, Some l ->
      c < l
  | Some _, None ->
      true
  | None, _ ->
      false

(* A C0 control character or DEL. The URL parser removes a tab, a newline and a
   carriage return: thus "/\t/x/" is "//x/". *)
let control s = String.exists (fun c -> c < ' ' || c = '\127') s

(* A text that can leave the origin or the base: a scheme, two slashes, a
   backslash or a control character. *)
let outside s =
  scheme s || contains s "//" || String.contains s '\\' || control s

(* A segment "." or "..", as the URL parser reads it: "%2e" is ".", with no
   case. *)
let dot_segment seg =
  let b = Buffer.create (String.length seg) in
  let n = String.length seg in
  let rec go i =
    if i < n then
      if i + 3 <= n && String.lowercase_ascii (String.sub seg i 3) = "%2e" then (
        Buffer.add_char b '.' ;
        go (i + 3) )
      else (
        Buffer.add_char b seg.[i] ;
        go (i + 1) )
  in
  go 0 ;
  let d = Buffer.contents b in
  d = "." || d = ".."

let check_base b =
  if b = "" || outside b || not (String.ends_with ~suffix:"/" b) then
    bad
      "the base %S is not a path that ends with / (a scheme, //, \\ and \
       control characters are refused)"
      b

let check_name n =
  if
    n = "" || starts "/" n || outside n
    || List.exists dot_segment (String.split_on_char '/' n)
  then
    bad
      "the name %S is empty, starts with /, has a scheme, //, \\ or a control \
       character, or has a . or .. segment"
      n

let check_id id =
  let ok c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-' in
  if id = "" || not (String.for_all ok id) then
    bad "the name %S of the markers has a character that is not a-z, 0-9 or -"
      id

(* The options of one command. Each value is the last one on the command
   line. *)
type opts =
  { mutable template: string option
  ; mutable out: string option
  ; mutable base: string option
  ; mutable js: string option
  ; mutable wasm: string option
  ; mutable build: string option
  ; mutable form: string option
  ; mutable loader: string option
  ; mutable name: string option
  ; mutable page: string option }

let parse cmd args spec =
  let o =
    { template= None
    ; out= None
    ; base= None
    ; js= None
    ; wasm= None
    ; build= None
    ; form= None
    ; loader= None
    ; name= None
    ; page= None }
  in
  let str f = Arg.String (fun s -> f o s) in
  let all =
    [ ("--template", str (fun o s -> o.template <- Some s), "FILE The template")
    ; ("-o", str (fun o s -> o.out <- Some s), "PATH The output")
    ; ("--base", str (fun o s -> o.base <- Some s), "BASE The base of the URLs")
    ; ("--js", str (fun o s -> o.js <- Some s), "NAME The js_of_ocaml build")
    ; ("--wasm", str (fun o s -> o.wasm <- Some s), "NAME The launcher")
    ; ("--build", str (fun o s -> o.build <- Some s), "js No Wasm probe")
    ; ("--form", str (fun o s -> o.form <- Some s), "inline|file The form")
    ; ("--loader", str (fun o s -> o.loader <- Some s), "NAME The loader file")
    ; ("--name", str (fun o s -> o.name <- Some s), "ID The name of the markers")
    ; ("--page", str (fun o s -> o.page <- Some s), "FILE The page") ]
  in
  let specs = List.filter (fun (k, _, _) -> List.mem k spec) all in
  let argv = Array.append [|cmd|] args in
  Arg.parse_argv ~current:(ref 0) argv specs
    (fun a -> bad "%s: an argument that is not an option: %s" cmd a)
    ("kilim " ^ cmd ^ " [options]") ;
  o

let need cmd flag = function Some v -> v | None -> bad "%s: give %s" cmd flag

let file_form cmd o =
  match o.form with
  | None | Some "inline" ->
      false
  | Some "file" ->
      true
  | Some f ->
      bad "%s: --form is inline or file, not %S" cmd f

(* The build options: the launcher, or None for --build js. *)
let launcher cmd o =
  match (o.wasm, o.build) with
  | Some _, Some _ ->
      bad "%s: give --wasm or --build js, not the two" cmd
  | None, None ->
      bad "%s: give --wasm or --build js" cmd
  | Some w, None ->
      Some w
  | None, Some "js" ->
      None
  | None, Some b ->
      bad "%s: --build is js, not %S" cmd b

let name o =
  Option.iter check_id o.name ;
  o.name

let read f = In_channel.with_open_bin f In_channel.input_all

let write f s = Out_channel.with_open_bin f (fun oc -> output_string oc s)

let page args =
  let c = "page" in
  let o =
    parse c args
      [ "--template"
      ; "-o"
      ; "--base"
      ; "--js"
      ; "--wasm"
      ; "--build"
      ; "--form"
      ; "--loader"
      ; "--name" ]
  in
  let template = need c "--template" o.template in
  let out = need c "-o" o.out in
  let base = need c "--base" o.base in
  check_base base ;
  let js = need c "--js" o.js in
  check_name js ;
  let build =
    match launcher c o with
    | Some w ->
        check_name w ;
        Part.Wasm {launcher= w; main= None}
    | None ->
        Part.Js
  in
  let loader = Option.value o.loader ~default:"loader.js" in
  check_name loader ;
  let form = if file_form c o then Part.File loader else Part.Inline in
  let name = name o in
  let t = {Part.base; js; build; dev= true} in
  fun () ->
    match
      Template.fill ~name ~head:(Part.head form t) ~start:(Part.start form t)
        (read template)
    with
    | Ok p ->
        write out p ; Ok ""
    | Error e ->
        Error (Fault (template ^ ": " ^ e))

let dist args =
  let c = "dist" in
  let o =
    parse c args
      [ "--template"
      ; "-o"
      ; "--base"
      ; "--js"
      ; "--wasm"
      ; "--build"
      ; "--form"
      ; "--name"
      ; "--page" ]
  in
  let template = need c "--template" o.template in
  let dir = need c "-o" o.out in
  let base = need c "--base" o.base in
  check_base base ;
  let js = need c "--js" o.js in
  let build =
    match launcher c o with Some w -> Dist.Wasm w | None -> Dist.Js
  in
  let file_form = file_form c o in
  let name = name o in
  fun () ->
    match
      Dist.run
        {Dist.base; js; build; file_form; name; template; page= o.page; dir}
    with
    | Ok () ->
        Ok ""
    | Error e ->
        Error (Fault e)

let loader args =
  let o = parse "loader" args ["-o"] in
  let out = need "loader" "-o" o.out in
  fun () -> write out Loader_text.text ; Ok ""

let csp args =
  ignore (parse "csp" args []) ;
  fun () -> Ok (Loader_text.csp ^ "\n")

let usage =
  "usage: kilim (page | loader | csp | dist) [options]; kilim COMMAND --help; \
   kilim --version"

let version raw = if raw = "" then "dev" else raw

let run argv =
  try
    let go =
      match Array.to_list argv with
      | _ :: "page" :: rest ->
          page (Array.of_list rest)
      | _ :: "loader" :: rest ->
          loader (Array.of_list rest)
      | _ :: "csp" :: rest ->
          csp (Array.of_list rest)
      | _ :: "dist" :: rest ->
          dist (Array.of_list rest)
      | _ :: ("--help" | "-help") :: _ ->
          fun () -> Ok (usage ^ "\n")
      | [_; "--version"] ->
          fun () -> Ok (version Version.raw ^ "\n")
      | _ :: c :: _ ->
          bad "an unknown command: %s (%s)" c usage
      | _ ->
          bad "%s" usage
    in
    try go () with Sys_error s -> Error (Fault s)
  with
  | Bad s ->
      Error (Usage s)
  | Arg.Bad s ->
      Error (Usage (List.hd (String.split_on_char '\n' s)))
  | Arg.Help s ->
      Ok s

let exit_code = function
  | Ok _ ->
      0
  | Error (Fault _) ->
      1
  | Error (Usage _) ->
      2
