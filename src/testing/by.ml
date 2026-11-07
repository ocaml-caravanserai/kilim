type t =
  | Node of Fake_dom.node
  | Test_id of string
  | Text of string
  | Label of string

let node n = Node n

let test_id s = Test_id s

let text s = Text s

let label s = Label s
