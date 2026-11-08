module S = Storage_state

let preset = S.preset

let contents page area =
  List.sort compare (S.items (S.of_host (Page.host page)) area)

let block page = (S.of_host (Page.host page)).blocked <- true
