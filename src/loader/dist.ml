type build = Wasm of string | Js

type t =
  { base: string
  ; js: string
  ; build: build
  ; file_form: bool
  ; name: string option
  ; template: string
  ; page: string option
  ; dir: string }

exception Fault of string

let fault fmt = Printf.ksprintf (fun s -> raise (Fault s)) fmt

let read f = In_channel.with_open_bin f In_channel.input_all

let exists f = Sys.file_exists f

(* The launcher X.js has its files in X.assets. *)
let assets_of launcher =
  match Filename.chop_suffix_opt ~suffix:".js" launcher with
  | Some p ->
      p ^ ".assets"
  | None ->
      fault "%s: the launcher of wasm_of_ocaml ends with .js" launcher

(* The names of the files of the .assets directory, sorted. A missing directory
   or a directory in it gives a Sys_error. *)
let asset_files dir = List.sort compare (Array.to_list (Sys.readdir dir))

(* The files to write: a path relative to [dir] and the bytes. *)
let plan t =
  let js_bytes = read t.js in
  let js = Names.content (Filename.basename t.js) js_bytes in
  let wasm, files =
    match t.build with
    | Js ->
        (Part.Js, [(js, js_bytes)])
    | Wasm launcher ->
        let lbytes = read launcher in
        let lname = Names.content (Filename.basename launcher) lbytes in
        let adir = assets_of launcher in
        let files = asset_files adir in
        let aname = Filename.basename adir in
        let main =
          match
            List.filter (fun f -> Filename.check_suffix f ".wasm") files
          with
          | [m] ->
              aname ^ "/" ^ m
          | l ->
              fault "kilim dist needs a release build: %d .wasm files in %s"
                (List.length l) adir
        in
        let assets =
          List.map
            (fun f -> (aname ^ "/" ^ f, read (Filename.concat adir f)))
            files
        in
        ( Part.Wasm {launcher= lname; main= Some main}
        , (js, js_bytes) :: (lname, lbytes) :: assets )
  in
  let form, files =
    if t.file_form then
      let l = Names.content "loader.js" Loader_text.text in
      (Part.File l, files @ [(l, Loader_text.text)])
    else (Part.Inline, files)
  in
  ({Part.base= t.base; js; build= wasm; dev= false}, form, files)

let mkdir d = if not (exists d) then Sys.mkdir d 0o755

let write f bytes =
  Out_channel.with_open_bin f (fun oc -> output_string oc bytes)

let run t =
  try
    let part, form, files = plan t in
    let page =
      match
        Template.fill ~name:t.name ~head:(Part.head form part)
          ~start:(Part.start form part) (read t.template)
      with
      | Ok p ->
          p
      | Error e ->
          fault "%s: %s" t.template e
    in
    (* Each conflict before the first write. *)
    let todo =
      List.filter
        (fun (rel, bytes) ->
          let f = Filename.concat t.dir rel in
          if not (exists f) then true
          else if read f = bytes then false
          else fault "%s exists with other bytes" f )
        files
    in
    mkdir t.dir ;
    List.iter
      (fun (rel, bytes) ->
        mkdir (Filename.concat t.dir (Filename.dirname rel)) ;
        write (Filename.concat t.dir rel) bytes )
      todo ;
    write
      (Option.value t.page ~default:(Filename.concat t.dir "index.html"))
      page ;
    Ok ()
  with Fault s | Sys_error s -> Error s
