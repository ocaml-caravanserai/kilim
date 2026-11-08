module S = Websocket_state

type server = S.server

let server page url = S.server (S.of_host (Page.host page)) url

let accept = S.accept

let push = S.push

let drop = S.drop

let fail = S.fail

let received (x : server) = List.rev x.received

let connections = S.connections
